import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createServer} from 'node:net';
import {fileURLToPath} from 'node:url';
import {readFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {getIntrospectionQuery, buildClientSchema, assertValidSchema, printSchema, parse, validate, version} from 'graphql';
import {createClient} from 'graphql-ws';
import WebSocket from 'ws';

const root = fileURLToPath(new URL('../../', import.meta.url));
const listener = createServer();
await new Promise(resolve => listener.listen(0, '127.0.0.1', resolve));
const port = listener.address().port;
await new Promise(resolve => listener.close(resolve));
const origin = `http://127.0.0.1:${port}`;
const binary = process.platform === 'win32' ? 'weave-graphql-fixture.exe' : 'weave-graphql-fixture';
const fixture = root + '.lake/build/bin/' + binary;
// Python's CREATE_NEW_PROCESS_GROUP/CTRL_BREAK_EVENT preserves graceful native
// shutdown on Windows, where Node kill(SIGINT) forcibly terminates processes.
const windowsWrapper = "import subprocess,sys,signal; p=subprocess.Popen(sys.argv[1:],creationflags=subprocess.CREATE_NEW_PROCESS_GROUP); sys.stdin.readline(); p.send_signal(signal.CTRL_BREAK_EVENT) if p.poll() is None else None; sys.exit(p.wait(timeout=5))";
const server = process.platform === 'win32'
  ? spawn(process.env.PYTHON || 'python', ['-c', windowsWrapper, fixture, String(port)], {stdio:['pipe','pipe','pipe']})
  : spawn(fixture, [String(port)], {stdio:['pipe','pipe','pipe']});
let log = '';
server.stdout.on('data', bytes => { log = (log + bytes).slice(-65536); });
server.stderr.on('data', bytes => { log = (log + bytes).slice(-65536); });
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
const deadline = (promise, ms = 3000) => Promise.race([promise, new Promise((_,reject) => { const timer = setTimeout(() => reject(Error('official client timeout')),ms); timer.unref(); })]);
async function http(query, variables = {}) {
  const response = await fetch(origin + '/graphql', {method:'POST',headers:{'content-type':'application/json'},body:JSON.stringify({query,variables})});
  return {status:response.status, result:await response.json()};
}
let client;
try {
  let ready = false;
  for (let i=0;i<200;i++) {
    if (server.exitCode !== null) throw Error('fixture exited: ' + log);
    try { ready = (await http('{counter{value}}')).status === 200; } catch {}
    if (ready) break;
    await sleep(10);
  }
  assert(ready, 'fixture startup');
  const reflected = await http(getIntrospectionQuery());
  assert.equal(reflected.status,200);
  assert(!reflected.result.errors, JSON.stringify(reflected.result));
  const schema = buildClientSchema(reflected.result.data);
  assertValidSchema(schema);
  const sdl = printSchema(schema);
  assert(sdl.includes('type Counter'));
  assert(sdl.includes('type Mutation'));
  assert(sdl.includes('type Subscription'));
  const query = 'query Current { counter { value label } }';
  assert.equal(validate(schema, parse(query)).length,0);
  class OriginSocket extends WebSocket {
    constructor(url, protocols) { super(url, protocols, {origin}); }
  }
  client = createClient({url:`ws://127.0.0.1:${port}/ws`,webSocketImpl:OriginSocket,retryAttempts:0,connectionAckWaitTimeout:1000});
  const one = query => new Promise((resolve,reject) => {
    let result;
    client.subscribe({query}, {next:value => {result=value;},error:reject,complete:()=>resolve(result)});
  });
  let receive, reject;
  const arrived = new Promise((resolve, fail) => {receive=resolve;reject=fail;});
  const cancel = client.subscribe({query:'subscription Changes { ticks { value label } }'}, {next:receive,error:reject,complete:()=>{}});
  const initial = await deadline(one(query));
  assert.equal(initial.data.counter.value,0);
  const mutation = await http('mutation Add($n:Int!){increment(amount:$n){value}}',{n:3});
  assert.equal(mutation.result.data.increment.value,3);
  const update = await deadline(arrived);
  assert.equal(update.data.ticks.value,3);
  cancel();
  let cancelled = 0;
  for(let i=0;i<100;i++) {
    cancelled = (await http('{cancelled}')).result.data.cancelled;
    if(cancelled === 1) break;
    await sleep(10);
  }
  assert.equal(cancelled,1,'client completion cleaned subscription exactly once');
  await client.dispose(); client = null;
  const inputs = [
    'LeanWeave/GraphQL.lean', 'LeanWeave/GraphQL/Parser.lean', 'LeanWeave/GraphQL/ParserTest.lean',
    'LeanWeave/GraphQL/Schema.lean', 'LeanWeave/GraphQL/Introspection.lean',
    'LeanWeave/GraphQL/Execution.lean', 'LeanWeave/GraphQL/HTTP.lean', 'LeanWeave/GraphQL/WebSocket.lean',
    'LeanWeave/Test/GraphQLSupport.lean', 'LeanWeave/Test/GraphQLFixture.lean', 'LeanWeave/Test/GraphQLMain.lean',
    'test/test_graphql.py', 'test/graphql-client/acceptance.mjs',
    'test/graphql-client/package.json', 'test/graphql-client/package-lock.json',
    'lean-toolchain', 'lakefile.toml', 'lake-manifest.json'
  ];
  const sourceHashes = Object.fromEntries(await Promise.all(inputs.map(async path =>
    [path, createHash('sha256').update(await readFile(root + path)).digest('hex')])));
  const fixtureSha256 = createHash('sha256').update(await readFile(fixture)).digest('hex');
  const manifest = JSON.parse(await readFile(root + 'lake-manifest.json','utf8'));
  const leanNetRevision = manifest.packages.find(pkg => pkg.url?.endsWith('/lean-net.git'))?.rev || null;
  console.log(JSON.stringify({node:process.version,platform:process.platform,arch:process.arch,leanNetRevision,fixtureSha256,sourceHashes,graphql:version,graphqlWs:'6.4.2',ws:'8.23.0',officialIntrospection:true,buildClientSchema:true,assertValidSchema:true,typedQuery:true,typedMutation:true,subscriptionNext:true,clientCompleteCleanupOnce:true}));
} finally {
  if(client) await client.dispose();
  if(server.exitCode === null) { if(process.platform === 'win32') server.stdin.end('stop\n'); else server.kill('SIGINT'); }
  await deadline(new Promise(resolve => { if(server.exitCode !== null) resolve(); else server.once('exit',resolve); }),5000);
  assert.equal(server.exitCode,0,log);
}
