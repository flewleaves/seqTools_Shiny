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

    # 扫描工作目录中的 .rds 文件（排除 .tmp 和 .bak）
    observe({
      req(state$settings)
      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      if (!dir.exists(wd)) return()
      proj_dir <- file.path(wd, "cache", "projects")
      if (dir.exists(proj_dir)) {
        # 清理残留的 .tmp 文件（上次保存中断的产物）
        tmp_files <- list.files(proj_dir, pattern = "[.]tmp$", full.names = TRUE)
        for (f in tmp_files) try(unlink(f), silent = TRUE)
        # 只列出 .rds 文件，排除 .bak
        files <- list.files(proj_dir, pattern = "[.]rds$")
        files <- files[!grepl("[.]bak$", files)]
      } else {
        files <- character(0)
      }
      if (length(files) == 0) files <- c("无项目文件" = "")
      updateSelectInput(session, "rds_select", choices = files)
    })

    observeEvent(input$btn_new, {
      req(input$new_name)
      if (nzchar(input$new_name) == FALSE) {
        showNotification("项目名称不能为空", type = "error")
        return()
      }
      shinyjs::disable("btn_new"); on.exit(shinyjs::enable("btn_new"))
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

      # 同步 state + 推送空状态到 Python（清除 AI 侧旧数据）
      state$name <- input$new_name
      state$data <- list()
      state$res <- list()
      state$meta <- list()
      state$omics_type <- NULL
      push_state_to_python()

      # 保存完整状态（R6 + reactiveValues）
      tryCatch({
        save_project(input$new_name, wd, state$settings)
        push_state_to_python()
      }, error = function(e) {
        showNotification(paste("项目保存失败:", e$message), type = "error", duration = NULL)
      })

      showNotification(paste("创建项目:", state$name), type = "message")

      # 刷新文件列表
      proj_dir <- file.path(wd, "cache", "projects")
      files <- if (dir.exists(proj_dir)) list.files(proj_dir, pattern = "[.]rds$") else character(0)
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
      shinyjs::disable("btn_load"); on.exit(shinyjs::enable("btn_load"))

      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      path <- file.path(wd, "cache", "projects", input$rds_select)
      if (!file.exists(path)) {
        showNotification("文件不存在", type = "error")
        return()
      }

      showNotification("加载中，请稍候...", type = "message", duration = NULL, id = "load_notify")
      tryCatch({
        loaded <- readRDS(path)
        # 用 inherits 先判断类型，避免 Seurat $ 抛错
        if (inherits(loaded, "Seurat")) {
          stop("该文件是 Seurat 对象，不是项目文件。请通过数据导入功能加载。")
        }
        if (!is.list(loaded) || (is.null(loaded$r6) && is.null(loaded$rv))) {
          stop("不是有效的项目文件")
        }

        engine$project <<- Project$new(
          name = loaded$r6$name %||% loaded$rv$name %||% "未命名",
          settings = state$settings$analysis
        )
        for (m in ImportRegistry$methods) {
          engine$project$register_import(m$id, m$name, m$schema, m$run)
        }
        if (!is.null(loaded$r6)) engine$project$deserialize(loaded$r6)

        # 将磁盘上的 Seurat 对象加载到内存，避免后续弹窗每次读盘
        tryCatch({
          for (key in names(engine$project$data)) {
            if (grepl("^dataList|seurat|clustered|integrated|sct|merged", key, ignore.case = TRUE))
              sc_load_from_disk(engine$project, key)
          }
        }, error = function(e) NULL)

        state$name       <- engine$project$name
        state$data       <- engine$project$data
        state$res        <- engine$project$results
        state$meta       <- engine$project$meta
        state$omics_type <- engine$project$omics_type

        push_state_to_python()
        removeNotification(id = "load_notify")
        showNotification(paste("加载项目:", state$name), type = "message")
      }, error = function(e) {
        removeNotification(id = "load_notify")
        # 尝试 .bak 恢复
        bak_path <- paste0(path, ".bak")
        bak_available <- file.exists(bak_path)
        hint <- if (bak_available)
          "\n\n检测到备份文件(.bak)，可尝试从备份恢复。请删除损坏的 .rds 文件，将 .bak 重命名为 .rds 后重试。"
        else
          "\n\n该文件可能在上次保存时被中断导致损坏，无法恢复。"
        showNotification(paste0("加载失败: ", e$message, hint),
                        type = "error", duration = NULL)
      })
    })

    # ── 手动保存 ─────────────────────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(state$name)
      shinyjs::disable("btn_save")
      updateActionButton(session, "btn_save", label = "保存中...")
      nid_saving <- showNotification("保存中，请稍候...", type = "message", duration = NULL)
      on.exit({
        removeNotification(nid_saving)
        shinyjs::enable("btn_save")
        updateActionButton(session, "btn_save", label = "保存")
      })
      wd <- state$settings$system$work_dir %||% "."
      wd <- normalizePath(wd, mustWork = FALSE)
      if (!dir.exists(wd)) dir.create(wd, recursive = TRUE)
      tryCatch({
        save_project(state$name, wd, state$settings)
        push_state_to_python()
        showNotification(paste("项目已保存:", state$name), type = "message")
      }, error = function(e) {
        showNotification(paste("保存失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ── 自动保存（每 5 分钟，不拉取 AI 状态） ─────────────────────────
    sync_counter <- reactiveVal(0)
    observe({
      invalidateLater(30000)
      isolate({
        if (!is.null(state$name) && nzchar(state$name)) {
          cnt <- sync_counter() + 1
          sync_counter(cnt)
          if (cnt %% 10 == 0) {
            captured_name <- state$name
            captured_settings <- state$settings
            later::later(function() {
              wd <- captured_settings$system$work_dir %||% "."
              wd <- normalizePath(wd, mustWork = FALSE)
              if (dir.exists(wd)) {
                tryCatch({
                  save_project(captured_name, wd, captured_settings)
                  push_state_to_python()
                }, error = function(e) NULL)
              }
            }, 1)
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
        cat("数据:", gsub(",", ", ", state$data_keys), "\n")
        cat("结果:", gsub(",", ", ", state$res_keys), "\n")
        cat("历史:", length(engine$project$history), "条记录\n")
      }
    })
  })
}

# 辅助函数：保存完整项目状态（原子写入 + .bak 备份，防止中断导致文件损坏）
save_project <- function(name, wd, settings) {
  proj_dir <- file.path(wd, "cache", "projects")
  dir.create(proj_dir, showWarnings = FALSE, recursive = TRUE)
  target <- file.path(proj_dir, paste0(name, ".rds"))

  # rv 仅保留 name/settings 做向后兼容，数据不重复存储（r6 已包含全部）
  r6_data <- engine$project$serialize()
  save_data <- list(
    rv = list(name = name, settings = settings),
    r6 = r6_data
  )

  # ---- 原子写入：临时文件 → 快速校验 → 重命名 ----
  tmp <- paste0(target, ".tmp")
  if (file.exists(tmp)) unlink(tmp)

  saveRDS(save_data, tmp)

  # 快速完整性校验
  if (!file.exists(tmp) || file.info(tmp)$size < 50)
    stop("保存失败: 临时文件异常（可能磁盘已满）")

  # 备份旧文件（只保留一份 .bak）
  if (file.exists(target)) {
    bak <- paste0(target, ".bak")
    if (file.exists(bak)) unlink(bak)
    file.rename(target, bak)
  }

  # 原子重命名（同文件系统上 rename 是原子的）
  if (!file.rename(tmp, target)) {
    unlink(tmp)
    stop("无法完成保存: 重命名失败")
  }
}

# 辅助函数：推送项目状态到 Python AI 后端
push_state_to_python <- function() {
  tmp <- summary_tmp <- NULL
  tryCatch({
    engine$project$meta$state_version <-
      (engine$project$meta$state_version %||% 0) + 1
    tmp <- tempfile(fileext = ".rds")
    summary_tmp <- tempfile(fileext = ".json")
    saveRDS(engine$project$serialize(), tmp, compress = FALSE)
    jsonlite::write_json(engine$project$to_list(), summary_tmp,
                         auto_unbox = TRUE, pretty = FALSE)
    httr::POST(
      "http://localhost:8765/sync_project",
      body = list(
        rds_path = normalizePath(tmp),
        summary_path = normalizePath(summary_tmp),
        version = engine$project$meta$state_version
      ),
      encode = "json",
      httr::timeout(60)
    )
  }, error = function(e) {
    message("[push_state] ", e$message)
  }, finally = {
    # 清理临时文件，防止磁盘/虚拟内存泄漏（每次 push 可能产生 GB 级文件）
    if (!is.null(tmp)) try(unlink(tmp), silent = TRUE)
    if (!is.null(summary_tmp)) try(unlink(summary_tmp), silent = TRUE)
    gc()
  })
}

# 辅助函数：从 Python AI 后端拉取项目状态（AI 运行工具后的结果）
pull_state_from_python <- function(state) {
  tryCatch({
    res_path <- file.path(APP_ROOT, "cache", "ai_state", "project_state.rds.results")
    if (!file.exists(res_path)) return()

    res_data <- readRDS(res_path)  # 几 KB，毫秒级
    if (!is.null(res_data$results)) engine$project$results <- res_data$results
    if (!is.null(res_data$meta))    engine$project$meta    <- res_data$meta
    if (!is.null(res_data$history)) engine$project$history <- res_data$history

    state$data       <- engine$project$data
    state$res        <- engine$project$results
    state$meta       <- engine$project$meta
    state$omics_type <- engine$project$omics_type
    message("[pull_state] 同步完成, results: ", paste(names(engine$project$results), collapse=", "))
  }, error = function(e) {
    message("[pull_state] ERROR: ", e$message)
    stop(e$message)
  })
}