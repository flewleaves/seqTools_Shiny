# modules/ai_console.R

ai_console_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .ai-markdown {
        font-size: 13px;
        line-height: 1.6;
      }
      .ai-markdown p {
        margin: 6px 0;
      }
      .ai-markdown h1, .ai-markdown h2, .ai-markdown h3, .ai-markdown h4 {
        margin: 10px 0 6px 0;
        font-weight: 600;
        color: #212529;
      }
      .ai-markdown pre {
        background: #f6f8fa;
        padding: 10px 12px;
        border-radius: 6px;
        overflow-x: auto;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 12px;
        line-height: 1.4;
        margin: 8px 0;
      }
      .ai-markdown code {
        background: rgba(175,184,193,0.2);
        padding: 2px 4px;
        border-radius: 3px;
        font-family: 'Consolas', 'Monaco', monospace;
        font-size: 12px;
      }
      .ai-markdown pre code {
        background: transparent;
        padding: 0;
      }
      .ai-markdown table {
        border-collapse: collapse;
        width: 100%;
        margin: 8px 0;
        font-size: 12px;
      }
      .ai-markdown th, .ai-markdown td {
        border: 1px solid #dee2e6;
        padding: 5px 8px;
        text-align: left;
      }
      .ai-markdown th {
        background: #f8f9fa;
        font-weight: 600;
      }
      .ai-markdown blockquote {
        border-left: 3px solid #0d6efd;
        margin: 8px 0;
        padding: 4px 12px;
        background: #f8f9fa;
        color: #495057;
      }
      .ai-markdown ul, .ai-markdown ol {
        padding-left: 20px;
        margin: 6px 0;
      }
      .ai-markdown img {
        max-width: 100%;
        border-radius: 4px;
        margin: 6px 0;
      }
      .ai-markdown hr {
        border: none;
        border-top: 1px solid #dee2e6;
        margin: 10px 0;
      }
    ")),
    
    card(
      full_screen = TRUE,
      card_header("AI 助手"),
      tags$div(
        id = ns("chat_box"),
        style = "height: 400px; overflow-y: auto; padding: 12px; background: #f8f9fa; color: #333; font-family: 'Consolas', monospace; font-size: 13px; border: 1px solid #dee2e6; border-radius: 4px;",
        tags$div(id = ns("chat_placeholder"), style = "color: #999;", "# 等待输入..."),
        tags$div(id = ns("chat_anchor"), style = "display: none;")
      ),
      uiOutput(ns("thinking_indicator")),
      tags$div(
        style = "display: flex; margin-top: 10px; gap: 8px;",
        tags$span("> ", style = "color: #0d6efd; padding-top: 8px; font-weight: bold;"),
        textAreaInput(ns("cmd"), NULL, placeholder = "Enter 发送，Shift+Enter 换行", 
                      rows = 2, resize = "vertical", width = "100%")
      ),
      actionButton(ns("send"), "发送", class = "btn-primary btn-sm")
    ),
    tags$script(HTML(sprintf("
      (function() {
        var NS = '%s';
        var TA_ID = '%s';
        var BTN_ID = '%s';
        var SUBMIT_ID = '%s';
        
        var boundKey = '_seqToolsAIChat_' + NS;
        if (window[boundKey]) return;
        window[boundKey] = true;
        
        var isComposing = false;
        
        document.addEventListener('compositionstart', function(e) {
          if (e.target.id === TA_ID) isComposing = true;
        }, true);
        
        document.addEventListener('compositionend', function(e) {
          if (e.target.id === TA_ID) isComposing = false;
        }, true);
        
        function submitChat() {
          var ta = document.getElementById(TA_ID);
          if (!ta) return;
          var value = ta.value;
          if (!value || !value.trim()) return;
          ta.value = '';
          Shiny.setInputValue(SUBMIT_ID, value, {priority: 'event'});
        }
        
        document.addEventListener('keydown', function(e) {
          if (e.target.id !== TA_ID) return;
          if ((e.key !== 'Enter' && e.keyCode !== 13) || e.shiftKey) return;
          if (e.isComposing || isComposing) return;
          e.preventDefault();
          submitChat();
        }, true);
        
        var btn = document.getElementById(BTN_ID);
        if (btn) {
          btn.addEventListener('click', function(e) {
            e.stopImmediatePropagation();
            submitChat();
          });
        }
      })();
      
      Shiny.addCustomMessageHandler('ai_scroll', function(msg) {
        var box = document.getElementById(msg.id);
        if (box) box.scrollTop = box.scrollHeight;
      });
    ", id, ns("cmd"), ns("send"), ns("cmd_submit"))))
  )
}

freeze_rv <- function(x) {
  if (shiny::is.reactivevalues(x)) {
    x <- shiny::reactiveValuesToList(x, all.names = TRUE)
  }
  if (is.list(x)) {
    x <- lapply(x, freeze_rv)
  }
  x
}

ai_console_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    is_processing <- reactiveVal(FALSE)
    has_message   <- reactiveVal(FALSE)
    ai_result     <- reactiveVal(NULL)
    
    output$thinking_indicator <- renderUI({
      if (!is_processing()) return(NULL)
      tags$div(
        style = "margin: 8px 0 8px 20px; color: #6c757d; font-style: italic;",
        tags$span(class = "spinner-border spinner-border-sm", 
                  style = "margin-right: 8px; width: 14px; height: 14px; border-width: 2px;"),
        "AI 正在思考..."
      )
    })
    
    do_send <- function(cmd) {
      cmd <- trimws(cmd)
      if (cmd == "") return()
      
      if (isolate(is_processing())) {
        shinyalert::shinyalert("请稍候", "当前有一条请求正在处理中...", type = "warning", timer = 1500)
        return()
      }
      
      if (!isolate(has_message())) {
        removeUI(selector = paste0("#", session$ns("chat_placeholder")), immediate = TRUE)
        has_message(TRUE)
      }
      
      insertUI(
        selector = paste0("#", session$ns("chat_anchor")),
        where = "beforeBegin",
        ui = tags$div(
          style = "margin: 4px 0;",
          tags$span(style = "color: #0d6efd; font-weight: bold;", "> "),
          tags$span(style = "color: #333;", cmd)
        ),
        immediate = TRUE
      )
      
      updateTextAreaInput(session, "cmd", value = "")
      is_processing(TRUE)
      session$sendCustomMessage("ai_scroll", list(id = session$ns("chat_box")))
      
      current_name  <- isolate(state$name) %||% "未命名"
      current_mode  <- isolate(state$mode) %||% "bulk"
      current_count <- isolate(state$count)
      count_dim     <- ifelse(is.null(current_count), "未导入", paste(dim(current_count), collapse = "x"))
      settings_list <- freeze_rv(isolate(state$settings))
      
      ctx <- paste(
        "当前分析状态:",
        sprintf("- 项目: %s", current_name),
        sprintf("- 模式: %s", current_mode),
        sprintf("- 数据: %s", count_dim),
        sep = "\n"
      )
      full_prompt <- paste(ctx, "\n\n用户问题:", cmd)
      
      later::later(function() {
        tryCatch({
          # 关键改动：传入 tools（全局变量）
          result <- call_ai_with_tools(full_prompt, settings_list, state, tools, import_methods)
          ai_result(list(type = "success", value = result))
        }, error = function(e) {
          ai_result(list(type = "error", value = conditionMessage(e)))
        })
      }, delay = 0)
    }
    
    observeEvent(ai_result(), {
      req(ai_result())
      res <- isolate(ai_result())
      
      if (res$type == "success") {
        insertUI(
          selector = paste0("#", session$ns("chat_anchor")),
          where = "beforeBegin",
          ui = tags$div(
            class = "ai-markdown",
            style = "background: #fff; border: 1px solid #dee2e6; padding: 8px 12px; margin: 4px 0 4px 20px; border-radius: 8px; color: #333;",
            HTML(shiny::markdown(res$value))   # <-- 两处改动之一：HTML() 包裹
          ),
          immediate = TRUE
        )
      } else {
        insertUI(
          selector = paste0("#", session$ns("chat_anchor")),
          where = "beforeBegin",
          ui = tags$div(
            style = "color: #dc3545; margin: 4px 0 4px 20px;",
            HTML(paste0("[ERR] ", res$value))   # <-- 两处改动之二：HTML() 包裹
          ),
          immediate = TRUE
        )
      }
      
      session$sendCustomMessage("ai_scroll", list(id = session$ns("chat_box")))
      is_processing(FALSE)
      ai_result(NULL)
    }, ignoreInit = TRUE)
    
    observeEvent(input$cmd_submit, {
      do_send(isolate(input$cmd_submit))
    }, ignoreInit = TRUE)
    
    observeEvent(input$send, {
      do_send(isolate(input$cmd))
    }, ignoreInit = TRUE)
  })
}