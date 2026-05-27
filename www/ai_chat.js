// www/ai_chat.js

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initAI);
} else {
  initAI();
}

function initAI() {
  'use strict';

  var sendBtn = document.querySelector('button[id$="-send"]');
  if (!sendBtn) {
    console.error('[AI] send button not found, retry in 200ms');
    setTimeout(initAI, 200);
    return;
  }
  var NS = sendBtn.id.replace('-send', '');
  console.log('[AI] NS:', NS);

  if (window['_ai_' + NS]) return;
  window['_ai_' + NS] = true;

  var ws, rc = 0, rt, ic = false, cp = false;
  var wu = (location.protocol === 'https:' ? 'wss:' : 'ws:') + '//' +
           (location.hostname || 'localhost') + ':' +
           (window.__SEQTOOLS_WS_PORT__ || '8765') + '/ws';

  function gid(s) { return document.getElementById(NS + '-' + s); }

  function st(ok) {
    var e = gid('ws_status');
    if (!e) return;
    e.textContent = ok ? '[已连接]' : '[未连接]';
    e.className = ok ? 'ws-connected' : 'ws-disconnected';
  }

  function scr() { var b = gid('chat_box'); if (b) b.scrollTop = b.scrollHeight; }

  function umsg(t) {
    var a = gid('chat_anchor') || gid('chat_box');
    if (!a) return;
    var d = document.createElement('div');
    d.style.margin = '4px 0';
    var s1 = document.createElement('span');
    s1.style.cssText = 'color:#0d6efd;font-weight:bold;margin-right:4px;';
    s1.textContent = '> ';
    var s2 = document.createElement('span');
    s2.style.color = '#333';
    s2.textContent = t;
    d.appendChild(s1); d.appendChild(s2);
    if (a.id === NS + '-chat_box') a.appendChild(d);
    else a.parentNode.insertBefore(d, a);
    scr();
  }

  function ael() {
    var e = gid('current_stream');
    if (e) return e;
    var a = gid('chat_anchor') || gid('chat_box');
    if (!a) {
      console.error('[AI] chat_anchor and chat_box not found');
      return null;
    }
    e = document.createElement('div');
    e.id = NS + '-current_stream';
    e.style.cssText = 'color:#333;background:#fff;border:1px solid #dee2e6;padding:8px 12px;margin:4px 0 4px 20px;border-radius:8px;min-height:20px;white-space:pre-wrap;word-break:break-word;';
    if (a.id === NS + '-chat_box') a.appendChild(e);
    else a.parentNode.insertBefore(e, a);
    return e;
  }

  function atxt(t) { var e = ael(); if (!e) return; e.textContent += t; scr(); }

  function afn() {
    var e = gid('current_stream');
    if (!e) return;
    e.removeAttribute('id');
    if (typeof marked !== 'undefined' && marked.parse) {
      try {
        e.innerHTML = marked.parse(e.textContent);
      } catch(err) {
        console.error('[AI] marked.parse error:', err);
      }
    }
  }

  function aerr(t) {
    var a = gid('chat_anchor') || gid('chat_box');
    if (!a) return;
    var d = document.createElement('div');
    d.style.cssText = 'color:#dc3545;background:#f8d7da;padding:8px 12px;border-radius:4px;margin:4px 0 4px 20px;';
    d.textContent = '[ERR] ' + t;
    if (a.id === NS + '-chat_box') a.appendChild(d);
    else a.parentNode.insertBefore(d, a);
    scr();
  }

  function rmth() {
    var e = gid('thinking');
    if (e) e.parentNode.removeChild(e);
  }

  function cnc() { ic = true; rmth(); if (ws && ws.readyState === 1) ws.send(JSON.stringify({type:'cancel'})); }

  function snd() {
    var t = gid('cmd');
    if (!t) { console.error('[AI] cmd not found'); return; }
    var v = t.value.trim();
    if (!v) return;
    t.value = '';
    umsg(v);
    var a = gid('chat_anchor') || gid('chat_box');
    if (a) {
      var th = document.createElement('div');
      th.id = NS + '-thinking';
      th.style.cssText = 'margin:8px 0 8px 20px;color:#6c757d;font-style:italic;';
      th.textContent = 'AI 正在思考...';
      if (a.id === NS + '-chat_box') a.appendChild(th);
      else a.parentNode.insertBefore(th, a);
      scr();
    }
    if (ws && ws.readyState === 1) {
      console.log('[WS] sending:', v.substring(0, 50));
      ws.send(JSON.stringify({type:'chat',prompt:v}));
    } else {
      console.log('[WS] not open, state:', ws ? ws.readyState : 'null');
      rmth(); aerr('WebSocket 未连接');
    }
  }

  sendBtn.addEventListener('click', function(e) { e.stopImmediatePropagation(); snd(); });

  var ta = gid('cmd');
  if (ta) {
    document.addEventListener('compositionstart', function(e) { if (e.target.id === NS + '-cmd') cp = true; }, true);
    document.addEventListener('compositionend', function(e) { if (e.target.id === NS + '-cmd') cp = false; }, true);
    document.addEventListener('keydown', function(e) {
      if (e.target.id !== NS + '-cmd' || e.key !== 'Enter' || e.shiftKey || e.isComposing || cp) return;
      e.preventDefault(); snd();
    }, true);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('ai_cancel', function(msg) { cnc(); });
  }

  function con() {
    if (rc >= 5) { st(false); return; }
    if (rt) clearTimeout(rt);
    try { ws = new WebSocket(wu); } catch(e) { return; }
    ws.onopen = function() { rc = 0; st(true); };
    ws.onmessage = function(ev) {
      try {
        var m = JSON.parse(ev.data);

        if (m.type === 'thinking') {
          var th = gid('thinking');
          if (!th) {
            var a = gid('chat_anchor') || gid('chat_box');
            if (a) {
              th = document.createElement('div');
              th.id = NS + '-thinking';
              th.style.cssText = 'margin:8px 0 8px 20px;color:#6c757d;font-style:italic;';
              th.textContent = 'AI 正在思考...';
              if (a.id === NS + '-chat_box') a.appendChild(th);
              else a.parentNode.insertBefore(th, a);
              scr();
            }
          }
          return;
        }

        if (m.type === 'stream_text') {
          rmth();
          atxt(m.content);
        }
        else if (m.type === 'stream_done') {
          rmth();
          afn();
        }
        else if (m.type === 'error') {
          rmth();
          aerr(m.error);
        }
        else if (m.type === 'tool_start') {
          rmth();
          atxt('[开始调用工具: ' + m.tool + '...]\n');
        }
        else if (m.type === 'tool_executing') {
          atxt('[执行工具: ' + m.tool + '...]\n');
        }
        else if (m.type === 'tool_result') {
          rmth();
          var ok = m.result && m.result.success;
          atxt('[工具 ' + m.tool + ' 执行' + (ok ? '成功' : '失败') + ']\n');
          if (window.Shiny) Shiny.setInputValue(NS + '-ai_refresh', Math.random());
        }
      } catch(e) {
        console.error('[WS] onmessage error:', e);
      }
    };
    ws.onclose = function() { st(false); if (ic) return; rc++; rt = setTimeout(con, 3000); };
    ws.onerror = function() { st(false); };
  }
  con();
}