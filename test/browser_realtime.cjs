async (page) => {
  const origin = new URL(page.url()).origin;
  await page.goto(origin + '/');
  await page.waitForFunction(() => document.querySelector('#weave-live').dataset.transport === 'websocket');
  await page.evaluate(() => { window.originalStatic = document.querySelector('#static'); });
  await page.getByRole('button', {name:'Increment', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('#count').textContent === '1');
  await page.waitForTimeout(800);
  const websocket = await page.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,version:document.querySelector('#weave-live').dataset.version,unchangedNodePreserved:window.originalStatic === document.querySelector('#static')}));
  if (!websocket.unchangedNodePreserved || websocket.transport !== 'websocket') throw Error('WebSocket diff failure');
  await page.screenshot({path:'/tmp/leanweave-realtime-browser/websocket.png'});
  await page.goto(origin + '/?sse');
  await page.waitForFunction(() => document.querySelector('#weave-live').dataset.transport === 'sse');
  await page.getByRole('button', {name:'Increment', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('#count').textContent === '1');
  const sse = await page.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,version:document.querySelector('#weave-live').dataset.version,error:document.querySelector('#weave-live').dataset.error || null}));
  if (sse.error) throw Error(sse.error);
  await page.screenshot({path:'/tmp/leanweave-realtime-browser/sse.png'});
  const first = await page.goto(origin + '/csp');
  const policy = first.headers()['content-security-policy'];
  const nonce = await page.locator('script[nonce]').first().evaluate(element => element.nonce);
  const csp = await page.evaluate(() => ({allowed:window.allowed === true,wrongBlocked:window.wrongNonce !== true,missingBlocked:window.missingNonce !== true}));
  if (!csp.allowed || !csp.wrongBlocked || !csp.missingBlocked || !policy.includes("'nonce-" + nonce + "'")) throw Error('CSP enforcement mismatch');
  const second = await page.reload();
  const nextNonce = await page.locator('script[nonce]').first().evaluate(element => element.nonce);
  if (nextNonce === nonce || !second.headers()['content-security-policy'].includes("'nonce-" + nextNonce + "'")) throw Error('CSP nonce reused/mismatched');
  return {browser:page.context().browser().version(),websocket,sse,csp:{...csp,headerMatches:true,freshOnSecondRequest:true}};
}
