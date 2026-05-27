# modules/settings.R

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
      textInput(ns("ai_base_url"), "API 地址", placeholder = "https://..."),
      actionButton(ns("test_ai"), "测试连接", class = "btn-info")
    ),
    card(
      card_header("分析参数"),
      selectInput(ns("set_norm"), "归一化", choices = c("log2", "log10", "None")),
      numericInput(ns("set_seed"), "随机种子", value = 42),
      numericInput(ns("set_pval"), "p值阈值", value = 0.05, min = 0.001, step = 0.01),
      numericInput(ns("set_logfc"), "logFC 阈值", value = 1, min = 0, max = 5, step = 0.1),
      selectInput(ns("set_dup"), "基因去重策略", choices = c("kmax", "max", "mean"))
    ),
    card(
      card_header("系统设置"),
      textInput(ns("work_dir"), "工作目录", value = ".")
    ),
    col_widths = c(4, 4, 4)
  )
}

settings_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # 各 provider 的默认地址
    default_urls <- list(
      deepseek = "https://api.deepseek.com",
      openai   = "https://api.openai.com/v1",
      aliyun   = "https://dashscope.aliyuncs.com/compatible-mode/v1",
      custom   = ""
    )
    
    # 各 provider 的推荐模型
    default_models <- list(
      deepseek = "deepseek-v4-pro",
      openai   = "gpt-4o-mini",
      aliyun   = "qwen-plus",
      custom   = "model-name"
    )

    # 初始化：把 state$settings 同步到 UI
    observe({
      req(state$settings)
      s <- state$settings
      
      # 同步 provider 和 model
      updateSelectInput(session, "ai_provider", selected = s$AI$provider %||% "deepseek")
      updateTextInput(session, "ai_model", value = s$AI$model %||% "deepseek-chat")
      updateTextInput(session, "ai_key", value = s$AI$key %||% "")
      
      # base_url：配置文件里有就用配置文件的，没有就根据 provider 自动填充
      saved_url <- s$AI$base_url %||% ""
      if (saved_url == "") {
        saved_url <- default_urls[[s$AI$provider %||% "deepseek"]] %||% ""
      }
      updateTextInput(session, "ai_base_url", value = saved_url)
      
      updateTextInput(session, "work_dir", value = s$system$work_dir %||% ".")
      updateSelectInput(session, "set_norm", selected = s$analysis$norm_method %||% "log2")
      updateSelectInput(session, "set_dup", selected = s$analysis$dup %||% "kmax")
      updateNumericInput(session, "set_pval", value = s$analysis$pval_cut %||% 0.05)
      updateNumericInput(session, "set_logfc", value = s$analysis$logfc_cut %||% 1)
      updateNumericInput(session, "set_seed", value = s$analysis$seed %||% 42)
    })

    # 切换 provider 时：自动填充推荐地址和模型（仅当用户没手动改过 base_url 时）
    # 用 reactiveVal 记录用户是否手动修改过 base_url
    user_modified_url <- reactiveVal(FALSE)
    
    observeEvent(input$ai_base_url, {
      # 忽略初始化时的空值触发
      req(input$ai_provider)
      # 只有当 base_url 不为空且不是当前 provider 的默认值时，才标记为用户修改
      current_default <- default_urls[[input$ai_provider]] %||% ""
      if (!is.null(input$ai_base_url) && input$ai_base_url != "" && input$ai_base_url != current_default) {
        user_modified_url(TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$ai_provider, {
      # 更新模型（总是更新）
      updateTextInput(session, "ai_model", value = default_models[[input$ai_provider]] %||% "deepseek-chat")
      
      # base_url：只有用户没手动改过，才自动填充
      if (!user_modified_url()) {
        updateTextInput(session, "ai_base_url", value = default_urls[[input$ai_provider]] %||% "")
      }
    }, ignoreInit = TRUE)

    # 测试连接
    observeEvent(input$test_ai, {
      if (is.null(input$ai_key) || input$ai_key == "") {
        showNotification("请先输入 API Key", type = "warning")
        return()
      }
      if (is.null(input$ai_base_url) || input$ai_base_url == "") {
        showNotification("API 地址不能为空，请选择提供商或手动填写", type = "warning")
        return()
      }
      
      tryCatch({
        test_body <- jsonlite::toJSON(list(
          model = input$ai_model,
          messages = list(list(role = "user", content = "Hi")),
          max_tokens = 5
        ), auto_unbox = TRUE)

        resp <- httr::POST(
          url = paste0(input$ai_base_url, "/chat/completions"),
          httr::add_headers(Authorization = paste("Bearer", input$ai_key)),
          httr::content_type_json(),
          body = test_body,
          httr::timeout(10)
        )

        status <- httr::status_code(resp)
        if (status == 200) {
          content <- httr::content(resp, "parsed")
          reply <- content$choices[[1]]$message$content %||% "OK"
          showNotification(paste0("连接成功: ", substr(reply, 1, 30)), type = "message")
        } else {
          body <- httr::content(resp, "text", encoding = "UTF-8")
          showNotification(paste("连接失败 (HTTP", status, "):", substr(body, 1, 100)), type = "error")
        }
      }, error = function(e) {
        showNotification(paste("连接失败:", e$message), type = "error")
      })
    })

    # 保存配置
    save_trigger <- reactive({
      list(
        input$ai_provider, input$ai_model, input$ai_key, input$ai_base_url,
        input$set_norm, input$set_pval, input$set_logfc, input$set_dup, input$set_seed,
        input$work_dir
      )
    }) %>% debounce(10000)

    observeEvent(save_trigger(), {
      # 校验：base_url 不能为空
      if (is.null(input$ai_base_url) || input$ai_base_url == "") {
        showNotification("保存失败：API 地址不能为空", type = "error", duration = 5)
        return()
      }
      
      # 先读现有配置，保留 key（不用 configr，避免键名错位 bug）
      existing <- tryCatch(
        load_config(),
        error = function(e) list(AI = list(key = ""))
      )
      
      # 如果 UI 的 key 为空（passwordInput 保护），保留文件里的 key
      key_to_save <- input$ai_key
      if (is.null(key_to_save) || key_to_save == "") {
        key_to_save <- existing$AI$key %||% ""
      }
      
      config <- list(
        AI = list(
          provider = input$ai_provider,
          model    = input$ai_model,
          key      = key_to_save,
          base_url = input$ai_base_url   # ← 直接存 UI 里的值，不做推断
        ),
        analysis = list(
          norm_method = input$set_norm,
          pval_cut    = input$set_pval,
          logfc_cut   = input$set_logfc,
          seed        = input$set_seed,
          dup         = input$set_dup
        ),
        system = list(work_dir = input$work_dir)
      )

      state$settings <- config
      save_config(config)

      if (!is.null(engine) && !is.null(engine$project)) {
        engine$project$settings <- config$analysis
      }

      showNotification("配置已自动保存", type = "message", duration = 2)
    }, ignoreInit = TRUE)
  })
}