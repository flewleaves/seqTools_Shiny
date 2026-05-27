# modules/ai_console.R

ai_console_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/marked/marked.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/highlight.js@11/highlight.min.js"),
      tags$link(rel = "stylesheet",
                href = "https://cdn.jsdelivr.net/npm/highlight.js@11/styles/github.min.css")
    ),
    tags$style(HTML("
      #", ns("chat_box"), " { height: 480px; overflow-y: auto; padding: 12px;
        background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; }
      .ai-user-msg { text-align: right; margin: 8px 0; }
      .ai-user-msg span { background: #0d6efd; color: #fff;
        padding: 6px 12px; border-radius: 12px 12px 2px 12px;
        display: inline-block; max-width: 80%; word-break: break-word; }
      .ai-bot-msg { margin: 8px 0 8px 8px; }
      .ai-bot-msg .bubble { background: #fff; border: 1px solid #dee2e6;
        padding: 8px 14px; border-radius: 2px 12px 12px 12px;
        display: inline-block; max-width: 92%; word-break: break-word; }
      .ai-markdown { font-size: 13px; line-height: 1.7; }
      .ai-markdown p  { margin: 4px 0; }
      .ai-markdown pre { background: #f6f8fa; padding: 10px; border-radius: 6px;
        overflow-x: auto; margin: 6px 0; }
      .ai-markdown code { font-family: monospace; font-size: 12px; }
      .ai-markdown table { border-collapse: collapse; width: 100%; margin: 6px 0; }
      .ai-markdown th, .ai-markdown td { border: 1px solid #dee2e6;
        padding: 4px 8px; text-align: left; }
      .ai-markdown th { background: #f0f0f0; }
      .ai-thinking { color: #999; font-style: italic; font-size: 12px; }
      .ai-tool-badge { display: inline-block; background: #e7f0ff; color: #0d6efd;
        border: 1px solid #b8d0ff; border-radius: 4px;
        padding: 2px 8px; font-size: 11px; margin: 2px 0; }
      .ai-error { color: #dc3545; }
      .ai-status-bar { font-size: 11px; color: #888; min-height: 18px;
        padding: 2px 4px; }
    ")),
    card(
      full_screen = TRUE,
      card_header(
        style = "display:flex; justify-content:space-between; align-items:center;",
        span("AI 助手"),
        actionButton(ns("clear_chat"), "清空对话", class = "btn-outline-secondary btn-sm")
      ),
      tags$div(id = ns("chat_box"),
        tags$div(id = ns("chat_placeholder"), class = "ai-thinking", "连接中..."),
        tags$div(id = ns("chat_anchor"))
      ),
      tags$div(id = ns("status_bar"), class = "ai-status-bar"),
      tags$div(
        style = "display:flex; gap:8px; margin-top:8px; align-items:flex-end;",
        tags$div(style = "flex:1;",
          textAreaInput(ns("cmd"), NULL,
                        placeholder = "Enter 发送，Shift+Enter 换行",
                        rows = 2, resize = "vertical", width = "100%")
        ),
        tags$div(style = "display:flex; flex-direction:column; gap:4px;",
          actionButton(ns("send"),   "发送", class = "btn-primary btn-sm"),
          actionButton(ns("cancel"), "中断", class = "btn-warning btn-sm",
                       style = "display:none;")
        )
      )
    ),
    tags$script(HTML(paste0(
'(function() {
  var NS = "', id, '-";
  var ws = null;
  var reconnectTimer = null;
  var reconnectCount = 0;
  var maxReconnect   = 5;
  var isCancelled    = false;

  var wsProtocol = window.location.protocol === "https:" ? "wss:" : "ws:";
  var rawHost    = window.location.hostname;
  var wsHost     = (!rawHost || rawHost === "0.0.0.0") ? "localhost" : rawHost;
  var wsPort     = window.__SEQTOOLS_WS_PORT__ || "8765";
  var wsUrl      = wsProtocol + "//" + wsHost + ":" + wsPort + "/ws";

  /* ── marked 配置：启用 highlight.js 代码高亮 ── */
  function setupMarked() {
    if (typeof marked === "undefined") return;
    marked.setOptions({
      breaks: true,
      gfm:    true,
      highlight: function(code, lang) {
        if (typeof hljs !== "undefined" && lang && hljs.getLanguage(lang)) {
          return hljs.highlight(code, { language: lang }).value;
        }
        return typeof hljs !== "undefined" ? hljs.highlightAuto(code).value : code;
      }
    });
  }

  /* ── WebSocket ── */
  function connect() {
    if (reconnectCount >= maxReconnect) {
      setStatus("连接失败，请检查 Python 后端是否运行在端口 " + wsPort, "error");
      var ph = document.getElementById(NS + "chat_placeholder");
      if (ph) ph.style.display = "none";
      return;
    }
    if (reconnectTimer) clearTimeout(reconnectTimer);
    ws = new WebSocket(wsUrl);

    ws.onopen = function() {
      console.log("WS connected:", wsUrl);
      reconnectCount = 0;
      setStatus("已连接", "ok");
      var ph = document.getElementById(NS + "chat_placeholder");
      if (ph) ph.style.display = "none";
    };
    ws.onmessage = function(e) { handleMessage(JSON.parse(e.data)); };
    ws.onclose   = function() {
      if (isCancelled) return;
      reconnectCount++;
      setStatus("连接断开，重连中 (" + reconnectCount + "/" + maxReconnect + ")...", "warn");
      reconnectTimer = setTimeout(connect, 3000);
    };
    ws.onerror = function(e) { console.error("WS error:", e); };
  }

  /* ── 消息处理 ── */
  function handleMessage(msg) {
    switch (msg.type) {
      case "thinking":
        getOrCreateBotBubble().textContent = "思考中...";
        getOrCreateBotBubble().className = "bubble ai-thinking";
        break;
      case "stream_text":
        appendStreamText(msg.content); break;
      case "tool_start":
        appendToolBadge("🔧 调用: " + msg.tool); break;
      case "tool_executing":
        appendToolBadge("⚙️ 执行: " + msg.tool); break;
      case "tool_result":
        appendToolBadge("✅ " + msg.tool + " 完成");
        if (window.Shiny) Shiny.setInputValue(NS + "ai_refresh", Math.random());
        break;
      case "stream_done":  finalizeMessage(); break;
      case "cancelled":    showCancelled();   break;
      case "error":        showError(msg.error); break;
    }
  }

  /* ── DOM 辅助 ── */
  function getAnchor() { return document.getElementById(NS + "chat_anchor"); }

  function getOrCreateBotBubble() {
    var el = document.getElementById(NS + "current_stream");
    if (!el) {
      var wrap = document.createElement("div");
      wrap.className = "ai-bot-msg";
      el = document.createElement("div");
      el.id        = NS + "current_stream";
      el.className = "bubble ai-streaming";
      wrap.appendChild(el);
      var anchor = getAnchor();
      anchor.parentNode.insertBefore(wrap, anchor);
    }
    return el;
  }

  var streamText = "";   /* 纯文本累积，用于最终 markdown 渲染 */

  function appendStreamText(text) {
    streamText += text;
    var el = getOrCreateBotBubble();
    el.className = "bubble ai-streaming";
    /* 流式阶段：plain text 显示，避免频繁重渲染 markdown */
    el.textContent = streamText;
    scrollToBottom();
  }

  function appendToolBadge(text) {
    var badge = document.createElement("div");
    badge.className = "ai-tool-badge";
    badge.textContent = text;
    var anchor = getAnchor();
    anchor.parentNode.insertBefore(badge, anchor);
    scrollToBottom();
  }

  function finalizeMessage() {
    var el = document.getElementById(NS + "current_stream");
    if (el) {
      el.id = "";
      el.className = "bubble ai-markdown";
      if (typeof marked !== "undefined" && marked.parse) {
        el.innerHTML = marked.parse(streamText);
        /* 对代码块运行 hljs */
        if (typeof hljs !== "undefined") {
          el.querySelectorAll("pre code").forEach(function(block) {
            hljs.highlightElement(block);
          });
        }
      } else {
        el.innerHTML = "<pre>" +
          streamText.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;") +
          "</pre>";
      }
    }
    streamText = "";
    setCancelVisible(false);
    isCancelled = false;
    setStatus("就绪", "ok");
    scrollToBottom();
  }

  function showCancelled() {
    var el = document.getElementById(NS + "current_stream");
    if (el) {
      el.className = "bubble ai-thinking";
      el.textContent = "已中断";
    }
    streamText = "";
    setCancelVisible(false);
    isCancelled = false;
    setStatus("已中断", "warn");
  }

  function showError(err) {
    var el = getOrCreateBotBubble();
    el.className = "bubble ai-error";
    el.textContent = "错误: " + err;
    streamText = "";
    setCancelVisible(false);
    setStatus("错误", "error");
    scrollToBottom();
  }

  function setStatus(text, level) {
    var bar = document.getElementById(NS + "status_bar");
    if (!bar) return;
    var colors = { ok: "#28a745", warn: "#fd7e14", error: "#dc3545" };
    bar.style.color   = colors[level] || "#888";
    bar.textContent   = text;
  }

  function setCancelVisible(v) {
    var btn = document.getElementById(NS + "cancel");
    if (btn) btn.style.display = v ? "inline-block" : "none";
  }

  function scrollToBottom() {
    var box = document.getElementById(NS + "chat_box");
    if (box) box.scrollTop = box.scrollHeight;
  }

  /* ── 事件绑定 ── */
  function bindEvents() {
    var sendBtn   = document.getElementById(NS + "send");
    var cancelBtn = document.getElementById(NS + "cancel");
    var clearBtn  = document.getElementById(NS + "clear_chat");
    var cmdTA     = document.getElementById(NS + "cmd");
    if (!sendBtn || !cancelBtn || !cmdTA) { setTimeout(bindEvents, 100); return; }

    sendBtn.addEventListener("click", function() {
      var text = cmdTA.value.trim();
      if (!text) return;
      cmdTA.value = "";

      /* 用户气泡 */
      var wrap = document.createElement("div");
      wrap.className = "ai-user-msg";
      var bubble = document.createElement("span");
      bubble.textContent = text;
      wrap.appendChild(bubble);
      getAnchor().parentNode.insertBefore(wrap, getAnchor());
      scrollToBottom();

      if (ws && ws.readyState === WebSocket.OPEN) {
        streamText = "";
        ws.send(JSON.stringify({ type: "chat", prompt: text }));
        setCancelVisible(true);
        setStatus("发送中...", "ok");
      } else {
        showError("AI 后端未连接（" + wsUrl + "），请确认 Python 服务已启动");
      }
    });

    cancelBtn.addEventListener("click", function() {
      isCancelled = true;
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: "cancel" }));
      }
    });

    if (clearBtn) {
      clearBtn.addEventListener("click", function() {
        var box   = document.getElementById(NS + "chat_box");
        var anchor = getAnchor();
        /* 删除 anchor 之前的所有兄弟节点 */
        while (box.firstChild && box.firstChild !== anchor) {
          box.removeChild(box.firstChild);
        }
        streamText = "";
        setStatus("对话已清空", "ok");
        /* 通知 R server 清空对话历史 */
        if (window.Shiny) Shiny.setInputValue(NS + "clear_history", Math.random());
      });
    }

    cmdTA.addEventListener("keydown", function(e) {
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        sendBtn.click();
      }
    });
  }

  setupMarked();
  connect();
  bindEvents();
})();
'
    )))
  )
}

ai_console_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # AI 工具调用完成后自动同步状态到 Shiny
    observeEvent(input$ai_refresh, {
      pull_state_from_python(state)
    })

    # 清空对话历史（通知 Python 端重置 conversation）
    observeEvent(input$clear_history, {
      # Python 端 WebSocket 的 conversation 在连接内维护
      # 前端重连会自动重置；此处发一个 reset 消息
      # （main.py 需支持 type="reset" 消息，见下方说明）
    })
  })
}
