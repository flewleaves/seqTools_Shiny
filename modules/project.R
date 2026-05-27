# modules/project.R

project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      card(card_header("新建项目"),
           textInput(ns("new_name"), "项目名称", placeholder = "输入项目名称..."),
           actionButton(ns("btn_new"), "创建", class = "btn-success")),
      card(card_header("加载项目"),
           selectInput(ns("rds_select"), "项目文件", choices = c("无项目文件" = "")),
           div(style = "display:flex; gap:8px;",
             actionButton(ns("btn_load"), "加载", class = "btn-primary"),
             actionButton(ns("btn_save"), "保存", class = "btn-info")
           )),
      col_widths = c(6, 6)
    ),
    card(card_header("状态"), verbatimTextOutput(ns("status")))
  )
}

project_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {

    # 扫描工作目录中的 .rds 文件
    observe({
      req(state$settings)
      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      if (!dir.exists(wd)) return()
      files <- list.files(wd, pattern = "[.]rds$", full.names = FALSE)
      if (length(files) == 0) files <- c("无项目文件" = "")
      updateSelectInput(session, "rds_select", choices = files)
    })

    observeEvent(input$btn_new, {
      req(input$new_name)
      if (nzchar(input$new_name) == FALSE) {
        showNotification("项目名称不能为空", type = "error")
        return()
      }

      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      if (!dir.exists(wd)) dir.create(wd, recursive = TRUE)

      # 重置全局 R6 Project
      engine$project <<- Project$new(
        name = input$new_name,
        settings = state$settings$analysis
      )

      # 重新注册导入方法（新 Project 的 import_registry 是空的）
      for (m in ImportRegistry$methods) {
        engine$project$register_import(m$id, m$name, m$schema, m$run)
      }

      # 同步 state
      state$name <- input$new_name
      state$data <- list()
      state$res <- list()
      state$meta <- list()
      state$omics_type <- NULL

      # 保存完整状态（R6 + reactiveValues）
      save_project(input$new_name, wd, state$settings)
      push_state_to_python()

      showNotification(paste("创建项目:", state$name), type = "message")

      # 刷新文件列表
      files <- list.files(wd, pattern = "[.]rds$", full.names = FALSE)
      if (length(files) == 0) files <- c("无项目文件" = "")
      updateSelectInput(session, "rds_select", choices = files,
                        selected = paste0(input$new_name, ".rds"))
    })

    observeEvent(input$btn_load, {
      req(input$rds_select)
      if (input$rds_select == "") {
        showNotification("请先选择项目文件", type = "warning")
        return()
      }

      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      path <- file.path(wd, input$rds_select)
      if (!file.exists(path)) {
        showNotification("文件不存在", type = "error")
        return()
      }

      loaded <- readRDS(path)

      # 恢复 R6 Project（先恢复 R6，再从 R6 同步到 state）
      engine$project <<- Project$new(
        name = loaded$r6$name %||% loaded$rv$name %||% "未命名",
        settings = state$settings$analysis
      )
      for (m in ImportRegistry$methods) {
        engine$project$register_import(m$id, m$name, m$schema, m$run)
      }
      if (!is.null(loaded$r6)) engine$project$deserialize(loaded$r6)

      # 立即从 R6 同步到 state（不依赖 2 秒轮询）
      state$name       <- engine$project$name
      state$data       <- engine$project$data
      state$res        <- engine$project$results
      state$meta       <- engine$project$meta
      state$omics_type <- engine$project$omics_type

      push_state_to_python()

      showNotification(paste("加载项目:", state$name), type = "message")
    })

    # ── 手动保存 ─────────────────────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(state$name)
      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      if (!dir.exists(wd)) dir.create(wd, recursive = TRUE)
      save_project(state$name, wd, state$settings)
      push_state_to_python()
      showNotification(paste("项目已保存:", state$name), type = "message")
    })

    # ── 自动保存（每 5 分钟） ─────────────────────────────────────────────
    observe({
      invalidateLater(300000)
      isolate({
        if (!is.null(state$name) && nzchar(state$name)) {
          wd <- state$settings$system$work_dir %||% "."
          wd <- normalizePath(wd, mustWork = FALSE)
          if (dir.exists(wd)) {
            pull_state_from_python(state)  # 先拉取 AI 结果
            save_project(state$name, wd, state$settings)
            push_state_to_python()
          }
        }
      })
    })

    output$status <- renderPrint({
      if (is.null(state$name)) {
        cat("无项目\n")
      } else {
        cat("项目:", state$name, "\n")
        cat("组学:", state$omics_type %||% "未设置", "\n")
        cat("数据:", paste(names(state$data), collapse = ", "), "\n")
        cat("结果:", paste(names(state$res), collapse = ", "), "\n")
        cat("历史:", length(engine$project$history), "条记录\n")
      }
    })
  })
}

# 辅助函数：保存完整项目状态
save_project <- function(name, wd, settings) {
  save_data <- list(
    rv = list(
      name       = name,
      data       = engine$project$data,
      res        = engine$project$results,
      meta       = engine$project$meta,
      omics_type = engine$project$omics_type,
      settings   = settings
    ),
    r6 = engine$project$serialize()
  )
  saveRDS(save_data, file.path(wd, paste0(name, ".rds")))
}

# 辅助函数：推送项目状态到 Python AI 后端
push_state_to_python <- function() {
  tryCatch({
    tmp <- tempfile(fileext = ".rds")
    saveRDS(engine$project$serialize(), tmp)
    httr::POST(
      "http://localhost:8765/sync_project",
      body = list(rds_path = normalizePath(tmp)),
      encode = "json",
      httr::timeout(10)
    )
  }, error = function(e) {
    message("[push_state] ", e$message)
  })
}

# 辅助函数：从 Python AI 后端拉取项目状态（AI 运行工具后的结果）
pull_state_from_python <- function(state) {
  tryCatch({
    tmp <- tempfile(fileext = ".rds")
    resp <- httr::GET(
      "http://localhost:8765/pull_state",
      httr::write_disk(tmp, overwrite = TRUE),
      httr::timeout(10)
    )
    if (resp$status_code != 200 || !file.exists(tmp) || file.info(tmp)$size == 0) {
      message("[pull_state] 无可用状态")
      return()
    }
    saved <- readRDS(tmp)
    if (is.null(saved)) return()

    engine$project$deserialize(saved)
    state$data       <- engine$project$data
    state$res        <- engine$project$results
    state$meta       <- engine$project$meta
    state$omics_type <- engine$project$omics_type
    message("[pull_state] AI 结果已同步到 Shiny 会话")
  }, error = function(e) {
    message("[pull_state] ", e$message)
  })
}