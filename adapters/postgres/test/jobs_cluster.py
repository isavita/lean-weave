#!/usr/bin/env python3
"""Real transactional outbox + independent-process SKIP LOCKED/crash recovery tests."""
from __future__ import annotations
import argparse
import hashlib
import json
import http.client
import os
from pathlib import Path
import platform
import signal
import socket
import subprocess
import tempfile
import time
from cluster_support import cluster

ROOT = Path(__file__).resolve().parents[1]
EXE = ROOT / ".lake/build/bin" / ("weave-pg-jobs-test.exe" if os.name == "nt" else "weave-pg-jobs-test")


def wait_for(predicate, message, seconds=15):
    cutoff = time.monotonic() + seconds
    while time.monotonic() < cutoff:
        if predicate():
            return
        time.sleep(.03)
    raise AssertionError(message)


def lifecycle(pg, environment, call, queue, drain_ms, delay_ms, forced):
    """Actual listener signal: admission, same cutoff, owner cleanup and recovery."""
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]

    def request(path, method="GET"):
        connection = http.client.HTTPConnection("127.0.0.1", port, timeout=2)
        try:
            connection.request(method, path)
            response = connection.getresponse()
            return response.status, response.read().decode()
        finally:
            connection.close()

    with tempfile.TemporaryFile(mode="w+") as log:
        process = subprocess.Popen([str(EXE), "lifecycle", queue, str(port), str(drain_ms)],
            env=environment, stdout=log, stderr=log,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
        try:
            def ready():
                assert process.poll() is None, "lifecycle server exited before ready"
                try: return request("/ready")[0] == 200
                except OSError: return False
            wait_for(ready, "lifecycle listener did not become ready")
            wait_for(lambda: int(request("/cron")[1]) >= 1,
                     "native timer did not dispatch and execute cron job")
            ids = []
            for i in range(3):
                status, job = request(f"/enqueue?value={i}&delay={delay_ms}", "POST")
                assert status == 202, (status, job)
                ids.append(job)
            wait_for(lambda: json.loads(request("/snapshot")[1])["active"] == 1,
                     "lifecycle job did not run")
            started = time.monotonic()
            process.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGTERM)
            process.wait(timeout=drain_ms / 1000 + 2)
            elapsed = time.monotonic() - started
            log.seek(0)
            output = log.read()
            assert process.returncode == 0, output
            assert "ADMISSION_STOPPED:" in output and "SERVER_SHUTDOWN" in output, output
            assert output.count("OWNER_CLOSED:0") == 1, output
            assert elapsed < drain_ms / 1000 + 1.5, (elapsed, output)
            rows = pg.sql(f"SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='{queue}'").strip()
            assert rows == "3", "signal lost retained jobs"
            if forced:
                assert pg.sql(f"SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='{queue}' AND state='succeeded'").strip() == "0"
                call("worker", queue, 2000, 2)
            assert pg.sql(f"SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='{queue}' AND state='succeeded'").strip() == "3"
            assert pg.sql(f"SELECT count(*) FROM public.weave_job_effects WHERE queue_name='{queue}'").strip() == "3"
            print(f"PASS native {'forced' if forced else 'graceful'} SIGTERM: 3 durable jobs, immediate admission stop, {drain_ms}ms shared cutoff, owner-close, no process hang ({elapsed:.3f}s)", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--native", action="store_true", help="also execute local native jobs/cron/workflow acceptance")
    parser.add_argument("--output", type=Path, help="write source-bound acceptance JSON after all checks pass")
    args = parser.parse_args()
    started = time.monotonic()
    if args.native:
        native = ROOT.parents[1] / ".lake/build/bin" / ("weave-jobs-test.exe" if os.name == "nt" else "weave-jobs-test")
        subprocess.run([str(native)], check=True, timeout=20)
    with cluster() as pg:
        version = pg.sql("SELECT version()").strip()
        environment = pg.environment()

        def call(mode, queue, a="0", b="0"):
            output = subprocess.run([str(EXE), mode, queue, str(a), str(b)], env=environment,
                                    text=True, capture_output=True, timeout=40)
            assert output.returncode == 0, (output.stdout, output.stderr)
            return output.stdout.strip()

        print(call("contracts", "contracts"), flush=True)
        lifecycle(pg, environment, call, "signal-graceful", 2000, 100, False)
        lifecycle(pg, environment, call, "signal-forced", 150, 1500, True)
        reserved_flow = call("workflow-seed", "bounded-flow", "saga", 0)
        assert pg.sql("SELECT jobs FROM public.leanweave_job_queues WHERE name='bounded-flow'").strip() == "5"
        rejected = subprocess.run([str(EXE), "workflow-seed", "bounded-flow", "saga", "0"], env=environment,
                                  text=True, capture_output=True, timeout=20)
        assert rejected.returncode != 0, "second workflow exceeded whole-DAG reservation"
        call("worker", "bounded-flow", 1000, 2)
        assert pg.sql("SELECT status FROM public.leanweave_workflows WHERE queue_name='bounded-flow'").strip() == "failed"
        assert pg.sql("SELECT label FROM public.weave_job_effects WHERE queue_name='bounded-flow' ORDER BY sequence").splitlines() == ["a", "b", "undo-b", "undo-a"]
        assert call("workflow-remove", "bounded-flow", reserved_flow) == "true"
        assert pg.sql("SELECT jobs||':'||bytes FROM public.leanweave_job_queues WHERE name='bounded-flow'").strip() == "0:0"
        print("PASS PostgreSQL: whole-DAG capacity reservation prevents dependency/compensation admission deadlock", flush=True)
        ids = json.loads(call("seed", "parallel", 40, 20))
        processes = []
        try:
            for _ in range(2):
                processes.append(subprocess.Popen([str(EXE), "worker", "parallel", "2500", "2"],
                                                  env=environment, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE))
            for process in processes:
                _, error = process.communicate(timeout=15)
                assert process.returncode == 0, error.decode()
            assert pg.sql("SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='parallel' AND state='succeeded'").strip() == "40"
            assert pg.sql("SELECT count(*) FROM public.weave_job_effects WHERE queue_name='parallel'").strip() == "40"
            assert len(ids) == len(set(ids)) == 40
            print("PASS PostgreSQL: 2 independent processes / 4 dedicated workers / 40 fenced effects", flush=True)
        finally:
            for process in processes:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=5)
                if process.stderr:
                    process.stderr.close()

        crashed_id = json.loads(call("seed", "crash", 1, 500))[0]
        crashed = subprocess.Popen([str(EXE), "worker", "crash", "60000", "1"], env=environment,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            wait_for(lambda: pg.sql("SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='crash' AND state='running'").strip() == "1",
                     "first process did not lease the crash job")
            crashed.kill()
            crashed.wait(timeout=5)
        finally:
            if crashed.poll() is None:
                crashed.kill()
                crashed.wait(timeout=5)
        call("worker", "crash", 2000, 1)
        assert pg.sql("SELECT state || ':' || attempt FROM public.leanweave_jobs WHERE queue_name='crash'").strip() == "succeeded:2"
        assert pg.sql("SELECT count(*) FROM public.weave_job_effects WHERE queue_name='crash'").strip() == "1"
        assert pg.sql("SELECT job_id FROM public.weave_job_effects WHERE queue_name='crash'").strip() == crashed_id
        print("PASS PostgreSQL: killed worker -> expired lease -> process recovery -> one idempotent effect", flush=True)

        print(call("workflow-contracts", "workflow-contracts"), flush=True)

        def labels(queue):
            return pg.sql(f"SELECT label FROM public.weave_job_effects WHERE queue_name='{queue}' ORDER BY sequence").splitlines()

        def workflow_state(queue):
            return pg.sql(f"SELECT status FROM public.leanweave_workflows WHERE queue_name='{queue}'").strip()

        saga = call("workflow-seed", "saga", "failure")
        call("worker", "saga", 1500, 2)
        assert workflow_state("saga") == "failed"
        assert labels("saga") == ["a", "b", "undo-b", "undo-a"]
        assert pg.sql("SELECT count(*) FROM public.leanweave_workflow_events e JOIN public.leanweave_workflows w ON w.id=e.workflow_id WHERE w.queue_name='saga'").strip() == "0"
        print("PASS PostgreSQL: dependent saga failure -> reverse compensation -> durable terminal state", flush=True)

        call("workflow-seed", "comp-fail", "comp-fail")
        call("worker", "comp-fail", 1500, 2)
        assert workflow_state("comp-fail") == "compensation_failed"
        assert labels("comp-fail") == ["a", "b"]
        pg.sql("INSERT INTO public.weave_job_controls(name,enabled) VALUES('comp-fail',true)")
        flow_id = pg.sql("SELECT id FROM public.leanweave_workflows WHERE queue_name='comp-fail'").strip()
        assert call("workflow-retry", "comp-fail", flow_id) == "true"
        call("worker", "comp-fail", 1500, 2)
        assert workflow_state("comp-fail") == "failed"
        assert labels("comp-fail") == ["a", "b", "undo-controlled", "undo-a"]
        print("PASS PostgreSQL: dead compensation retained -> explicit operator retry -> saga completion", flush=True)

        cancelled_id = call("workflow-seed", "cancel-flow", "success", 2000)
        running = subprocess.Popen([str(EXE), "worker", "cancel-flow", "3000", "1"], env=environment,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        try:
            wait_for(lambda: pg.sql("SELECT count(*) FROM public.leanweave_jobs WHERE queue_name='cancel-flow' AND state='running'").strip() == "1",
                     "workflow step did not start")
            assert call("workflow-cancel", "cancel-flow", cancelled_id) == "true"
            _, error = running.communicate(timeout=15)
            assert running.returncode == 0, error.decode()
        finally:
            if running.poll() is None:
                running.kill(); running.wait(timeout=5)
            if running.stderr:
                running.stderr.close()
        assert workflow_state("cancel-flow") == "cancelled" and labels("cancel-flow") == []
        print("PASS PostgreSQL: in-flight workflow cancellation -> cooperative job cancellation -> durable state", flush=True)

        recovered_id = call("workflow-seed", "outbox-crash", "success")
        acked = subprocess.Popen([str(EXE), "ack-stop", "outbox-crash", "0", "0"], env=environment,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            assert acked.stdout.readline().strip() == "ACK"
            assert pg.sql("SELECT count(*) FROM public.leanweave_workflow_events e JOIN public.leanweave_workflows w ON w.id=e.workflow_id WHERE w.queue_name='outbox-crash'").strip() == "1"
            acked.kill(); acked.wait(timeout=5)
        finally:
            if acked.poll() is None:
                acked.kill(); acked.wait(timeout=5)
            acked.stdout.close(); acked.stderr.close()
        call("worker", "outbox-crash", 1500, 2)
        assert workflow_state("outbox-crash") == "succeeded"
        assert labels("outbox-crash") == ["a", "b", "c"]
        print("PASS PostgreSQL: process killed after job ACK -> durable workflow outbox replay -> no lost dependency", flush=True)

    if args.output:
        source_root = ROOT.parents[1]
        paths = sorted((source_root / "LeanWeave/Jobs").glob("*.lean")) + sorted((source_root / "LeanWeave/Workflow").glob("*.lean"))
        paths += [ROOT / "LeanWeavePostgres/Jobs.lean", ROOT / "LeanWeavePostgres/Workflows.lean",
                  Path(__file__).resolve(), ROOT / "test/JobsMain.lean"]
        report = {"passed": True, "platform": platform.platform(), "postgres": version,
            "duration_seconds": round(time.monotonic() - started, 3), "local_native": args.native,
            "native_cron_timer": True, "native_sigterm_graceful": True, "native_sigterm_forced": True,
            "durable_distributed_workers": 4, "durable_parallel_jobs": 40,
            "worker_crash_lease_recovery": True, "ack_crash_workflow_outbox_replay": True,
            "transactional_enqueue": True, "saga_compensation": True, "operator_compensation_retry": True,
            "workflow_cancellation": True, "whole_dag_capacity_reservation": True,
            "persistent_service_started": False,
            "sources_sha256": {str(p.relative_to(source_root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}}
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")



if __name__ == "__main__":
    main()
