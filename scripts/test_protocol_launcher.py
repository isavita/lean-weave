#!/usr/bin/env python3
"""Cross-platform process contract; real protocol wire tests exercise native I/O.

Platform cases substitute only the child process and readiness socket. The
production context manager chooses its executable, group, signals, deadlines
and exit-status validation. The encoding case compiles real Unicode Lean input
and reads real compiler diagnostics with forced cp1252 parent defaults.
"""
from contextlib import ExitStack, contextmanager
import io
import locale
import signal
import subprocess
import unittest
from unittest.mock import MagicMock, patch

import test_protocols as protocols
import test_protocol_compile as compiler


class ProtocolLauncher(unittest.TestCase):
    @contextmanager
    def child(self, platform_name, returncode=0, timeout=False):
        process = MagicMock()
        process.poll.return_value = None
        process.returncode = returncode
        if timeout:
            process.wait.side_effect = [subprocess.TimeoutExpired('fixture', 4), None]
        with ExitStack() as stack:
            stack.enter_context(patch.object(protocols.sys, 'platform', platform_name))
            stack.enter_context(patch.object(protocols.subprocess, 'CREATE_NEW_PROCESS_GROUP', 512, create=True))
            stack.enter_context(patch.object(protocols.signal, 'CTRL_BREAK_EVENT', 21, create=True))
            launch = stack.enter_context(patch.object(protocols.subprocess, 'Popen', return_value=process))
            stack.enter_context(patch.object(protocols.socket, 'create_connection', return_value=MagicMock()))
            yield process, launch

    def test_default_executable_has_native_suffix(self):
        for platform_name, filename in [('win32', 'weave-protocol-fixture.exe'),
                ('linux', 'weave-protocol-fixture'), ('darwin', 'weave-protocol-fixture')]:
            with self.subTest(platform=platform_name), patch.object(protocols.sys, 'platform', platform_name):
                self.assertEqual(protocols.default_executable(), protocols.ROOT / '.lake/build/bin' / filename)

    def test_unicode_source_and_diagnostics_with_cp1252_defaults(self):
        original_open = io.open
        def legacy_open(file, mode='r', buffering=-1, encoding=None, errors=None,
                newline=None, closefd=True, opener=None):
            if 'b' not in mode and encoding in (None, 'locale'):
                encoding = 'cp1252'
            return original_open(file, mode, buffering, encoding, errors, newline, closefd, opener)
        with patch.object(io, 'open', legacy_open), \
                patch.object(locale, 'getpreferredencoding', return_value='cp1252'), \
                patch.object(locale, 'getencoding', return_value='cp1252', create=True):
            # Both the UTF-8 file and the real Lean subprocess diagnostics must
            # preserve characters absent from the Windows legacy code page.
            compiler.run('def unicodeValue : String := "héllo ⟨😀⟩"', True)
            compiler.run('def unicodeValue : Nat := "héllo ⟨😀⟩"', False, '⟨😀⟩')

    def test_windows_group_receives_ctrl_break(self):
        with self.child('win32') as (process, launch):
            executable = protocols.default_executable()
            with protocols.fixture(executable) as port:
                self.assertGreater(port, 0)
            self.assertEqual(launch.call_args.args[0], [str(executable), str(port)])
            self.assertEqual(launch.call_args.kwargs['creationflags'], 512)
            process.send_signal.assert_called_once_with(21)
            process.wait.assert_called_once_with(timeout=4)
            process.kill.assert_not_called()

    def test_posix_receives_sigterm_without_new_group(self):
        with self.child('linux') as (process, launch):
            with protocols.fixture(protocols.default_executable()): pass
            self.assertEqual(launch.call_args.kwargs['creationflags'], 0)
            process.send_signal.assert_called_once_with(signal.SIGTERM)
            process.wait.assert_called_once_with(timeout=4)
            process.kill.assert_not_called()

    def test_client_failure_still_shuts_down_windows_group(self):
        with self.child('win32') as (process, _):
            with self.assertRaisesRegex(RuntimeError, 'client failed'):
                with protocols.fixture(protocols.default_executable()):
                    raise RuntimeError('client failed')
            process.send_signal.assert_called_once_with(21)
            process.wait.assert_called_once_with(timeout=4)

    def test_forced_termination_does_not_count_as_graceful_success(self):
        with self.child('win32', returncode=-9, timeout=True) as (process, _):
            with self.assertRaisesRegex(AssertionError, 'fixture exit -9'):
                with protocols.fixture(protocols.default_executable()): pass
            process.kill.assert_called_once_with()
            self.assertEqual(process.wait.call_count, 2)


if __name__ == '__main__': unittest.main()
