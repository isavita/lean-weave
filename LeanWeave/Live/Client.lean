namespace LeanWeave.Live

/-- Small browser transport: server-rendered keyed regions, WS push, EventSource
fallback/reconnect and same-origin POST commands with a session CSRF token. -/
def clientScript : String := r###"
(() => {
  const root = document.getElementById('weave-live');
  if (!root) return;
  const base = document.querySelector('meta[name=weave-base]').content;
  const csrf = document.querySelector('meta[name=weave-csrf]').content;
  let version = -1, ws = null, source = null;
  function apply(message) {
    if (!Number.isSafeInteger(message.version)) throw Error('invalid live version');
    if (message.type !== 'snapshot' && message.version <= version) return;
    if (message.type === 'snapshot') root.replaceChildren();
    else if (message.version !== version + 1) { fallback(); return; }
    for (const patch of message.patches) {
      let region = document.getElementById('weave-' + patch.key);
      if (patch.html === null) { if (region) region.remove(); continue; }
      if (!region) { region = document.createElement('div'); region.id = 'weave-' + patch.key; root.append(region); }
      const template = document.createElement('template');
      template.innerHTML = patch.html;
      region.replaceChildren(template.content);
    }
    version = message.version;
    root.dataset.version = String(version);
  }
  function fallback() {
    if (ws) { const previous = ws; ws = null; previous.onclose = null; previous.close(); }
    if (source) return;
    root.dataset.transport = 'sse';
    source = new EventSource(base + '/events');
    source.addEventListener('patch', event => apply(JSON.parse(event.data)));
  }
  async function send(event) {
    const command = JSON.stringify({event, csrf});
    if (ws && ws.readyState === WebSocket.OPEN) ws.send(command);
    else {
      const result = await fetch(base + '/event', {method:'POST', credentials:'same-origin', headers:{'content-type':'application/json'}, body:command});
      if (!result.ok) throw Error('live command rejected: ' + result.status);
    }
  }
  document.addEventListener('click', event => {
    const button = event.target.closest('button[name=weave-event]');
    if (!button) return;
    event.preventDefault(); send(button.value).catch(error => { root.dataset.error = error.message; });
  });
  if (new URLSearchParams(location.search).has('sse')) fallback();
  else {
    const url = new URL(base + '/socket', location.href); url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(url);
    ws.onopen = () => { root.dataset.transport = 'websocket'; };
    ws.onmessage = event => apply(JSON.parse(event.data));
    ws.onclose = fallback;
    ws.onerror = () => { if (ws) ws.close(); };
  }
  window.addEventListener('pagehide', () => { if (ws) ws.close(); if (source) source.close(); });
})();
"###

end LeanWeave.Live
