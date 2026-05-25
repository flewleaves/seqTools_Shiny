settings_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      card_header("AI 设置"),
      selectInput(ns("ai_provider"), "AI 提供商",
                  choices = c("DeepSeek" = "deepseek", 
                              "OpenAI" = "openai",
                              "阿里云百炼" = "aliyun",
                              "自定义 (OpenAI兼容)" = "custom"),
                  selected = "deepseek"),
      textInput(ns("ai_model"), "模型名称", value = "deepseek-chat"),
      passwordInput(ns("ai_key"), "API Key", placeholder = "sk-..."),
      textInput(ns("ai_base_url"), "自定义 API 地址 (可选)", placeholder = "https://..."),
      actionButton(ns("test_ai"), "测试连接", class = "btn-info")
    ),
    card(
      card_header("分析参数"),
      selectInput(ns("set_norm"), "归一化", choices = c("log2", "log10", "None")),
      numericInput(ns("set_seed"), "随机种子", value = 42),
      numericInput(ns("set_pval"), "p值阈值", value = 0.05, min = 0.001, step = 0.01),
      numericInput(ns("set_logfc"), "logFC 阈值", value = 1, min = 0, max = 5, step = 0.1),
      selectInput(ns("set_dup"), "基因去重策略", choices = c("kmax", "max", "mean")),
    ),
    card(
      card_header("系统设置"),
      textInput(ns("work_dir"), "工作目录", value = getwd())
    ),
    col_widths = c(4, 4, 4)
  )
}

settings_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    # 初始化：把 state$settings 同步到 UI（只执行一次）
    observe({
      req(state$settings)
      s <- state$settings
      
      updateSelectInput(session, "ai_provider", selected = s$ai$provider)
      updateTextInput(session, "ai_model", value = s$ai$model)
      updateTextInput(session, "ai_key", value = s$ai$key)
      updateTextInput(session, "ai_base_url", value = s$ai$base_url %||% "")
      updateTextInput(session, "work_dir", value = s$system$work_dir)
      updateSelectInput(session, "set_norm", selected = s$analysis$norm_method)
      updateSelectInput(session, "set_dup", selected = s$analysis$dup)
      updateNumericInput(session, "set_pval", value = s$analysis$pval_cut)
      updateNumericInput(session, "set_logfc", value = s$analysis$logfc_cut)
      updateNumericInput(session, "set_seed", value = s$analysis$seed)
    })
    
    # 切换 provider 推荐模型
    observeEvent(input$ai_provider, {
      dm <- switch(input$ai_provider,
        "deepseek" = "deepseek-chat",
        "openai" = "gpt-4o-mini",
        "aliyun" = "qwen-plus",
        "custom" = "model-name",
        "deepseek-chat"
      )
      updateTextInput(session, "ai_model", value = dm)
    })
    
    # 测试连接
    observeEvent(input$test_ai, {
      if (is.null(input$ai_key) || input$ai_key == "") {
        showNotification("请先输入 API Key", type = "warning")
        return()
      }
      
      tryCatch({
        cfg <- list(
          ai = list(
            provider = input$ai_provider,
            key = input$ai_key,
            model = input$ai_model,
            base_url = input$ai_base_url
          )
        )
        result <- call_ai("Hello", cfg)
        showNotification(paste("连接成功:", substr(result, 1, 30)), type = "message")
      }, error = function(e) {
        showNotification(paste("连接失败:", e$message), type = "error")
      })
    })
    
    # 保存配置：关键修复。observeEvent + ignoreInit = TRUE，避免初始化时级联触发
    observeEvent({
      input$ai_provider
      input$ai_model
      input$ai_key
      input$ai_base_url
      input$set_norm
      input$set_pval
      input$set_logfc
      input$set_dup
      input$set_seed
      input$work_dir
    }, {
      config <- list(
        ai = list(
          provider = input$ai_provider,
          model = input$ai_model,
          key = input$ai_key,
          base_url = input$ai_base_url
        ),
        analysis = list(
          norm_method = input$set_norm,
          pval_cut = input$set_pval,
          logfc_cut = input$set_logfc,
          seed = input$set_seed,
          dup = input$set_dup
        ),
        system = list(
          work_dir = input$work_dir
        )
      )
      
      state$settings <- config
      save_config(config, work_dir = input$work_dir)
    }, ignoreInit = TRUE)
  })
}