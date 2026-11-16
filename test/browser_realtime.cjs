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
  const ownerBefore = (await page.context().cookies(origin)).find(cookie => cookie.name === 'weave-live');
  const firstView = await page.locator('meta[name=weave-view]').getAttribute('content');
  const secondTab = await page.context().newPage();
  let sse, multiTab;
  try {
    await secondTab.goto(origin + '/');
    await secondTab.waitForFunction(() => document.querySelector('#weave-live').dataset.transport === 'websocket');
    const secondView = await secondTab.locator('meta[name=weave-view]').getAttribute('content');
    const ownerAfter = (await page.context().cookies(origin)).find(cookie => cookie.name === 'weave-live');
    if (!ownerBefore || ownerBefore.value !== ownerAfter?.value || firstView === secondView)
      throw Error('second tab replaced owner cookie or reused view');
    if (await secondTab.locator('#count').textContent() !== '0') throw Error('second tab inherited first tab state');
    await secondTab.getByRole('button', {name:'Increment', exact:true}).click();
    await secondTab.waitForFunction(() => document.querySelector('#count').textContent === '1');
    await secondTab.getByRole('button', {name:'Increment', exact:true}).click();
    await secondTab.waitForFunction(() => document.querySelector('#count').textContent === '2');
    await page.getByRole('button', {name:'Decrement', exact:true}).click();
    await page.waitForFunction(() => document.querySelector('#count').textContent === '0');
    await page.waitForTimeout(900);
    const firstState = await page.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,error:document.querySelector('#weave-live').dataset.error || null}));
    const secondState = await secondTab.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,error:document.querySelector('#weave-live').dataset.error || null}));
    if (firstState.transport !== 'websocket' || secondState.transport !== 'websocket' || firstState.count !== '0' || secondState.count !== '2' || firstState.error || secondState.error)
      throw Error('simultaneous tab state/transport isolation failed');
    // One tab changes transport; the other keeps its original view and socket.
    await page.goto(origin + '/?sse');
    await page.waitForFunction(() => document.querySelector('#weave-live').dataset.transport === 'sse');
    await page.getByRole('button', {name:'Increment', exact:true}).click();
    await page.waitForFunction(() => document.querySelector('#count').textContent === '1');
    await secondTab.getByRole('button', {name:'Decrement', exact:true}).click();
    await secondTab.waitForFunction(() => document.querySelector('#count').textContent === '1');
    await page.waitForTimeout(900);
    sse = await page.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,version:document.querySelector('#weave-live').dataset.version,error:document.querySelector('#weave-live').dataset.error || null}));
    const survivingTab = await secondTab.evaluate(() => ({transport:document.querySelector('#weave-live').dataset.transport,count:document.querySelector('#count').textContent,error:document.querySelector('#weave-live').dataset.error || null}));
    if (sse.error || sse.transport !== 'sse' || survivingTab.error || survivingTab.transport !== 'websocket')
      throw Error('SSE tab superseded another view');
    multiTab = {stableOwnerCookie:true,distinctViewIds:true,independentWebSocketStates:[firstState,secondState],mixedTransports:{sse,survivingTab}};
    await page.screenshot({path:'/tmp/leanweave-realtime-browser/sse.png'});
  } finally { await secondTab.close(); }
  const first = await page.goto(origin + '/csp');
  const policy = first.headers()['content-security-policy'];
  const nonce = await page.locator('script[nonce]').first().evaluate(element => element.nonce);
  const csp = await page.evaluate(() => ({allowed:window.allowed === true,wrongBlocked:window.wrongNonce !== true,missingBlocked:window.missingNonce !== true}));
  if (!csp.allowed || !csp.wrongBlocked || !csp.missingBlocked || !policy.includes("'nonce-" + nonce + "'")) throw Error('CSP enforcement mismatch');
  const second = await page.reload();
  const nextNonce = await page.locator('script[nonce]').first().evaluate(element => element.nonce);
  if (nextNonce === nonce || !second.headers()['content-security-policy'].includes("'nonce-" + nextNonce + "'")) throw Error('CSP nonce reused/mismatched');
  return {browser:page.context().browser().version(),browserEngine:page.context().browser().browserType().name(),websocket,sse,multiTab,csp:{...csp,headerMatches:true,freshOnSecondRequest:true}};
}
