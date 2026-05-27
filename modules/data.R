# modules/data.R

data_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "读取数据",
      radioGroupButtons(
        inputId = ns("import_method"), label = "选择组学类型",
        choices = c("加载中..." = ""), selected = "", status = "primary"
      ),
      hr(),
      uiOutput(ns("method_ui")),
      hr(),
      sliderInput(ns("page_size"), "每页行数", min = 5, max = 100, value = 10),
      actionButton(ns("confirm_input"), "确认导入", class = "btn-info")
    ),
    card(
      full_screen = TRUE,
      card_header("数据预览"),
      verbatimTextOutput(ns("debug_info")),
      DT::dataTableOutput(ns("preview"))
    )
  )
}

data_server <- function(id, state, nav_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 临时存文件上传后读取的原始 df（用于列选择 modal）
    temp_df   <- reactiveVal(NULL)
    temp_cols <- reactiveVal(list())   # 用户在 modal 里确认的列信息

    # ---------- 注册表同步 ----------
    observe({
      invalidateLater(2000)
      methods <- ImportRegistry$list_methods()
      if (length(methods) > 0) {
        choices <- setNames(
          sapply(methods, `[[`, "id"),
          sapply(methods, `[[`, "name")
        )
        updateRadioGroupButtons(session, "import_method",
                                choices = choices, selected = choices[1])
      }
    })

    current_method <- reactive({
      req(input$import_method, input$import_method != "")
      ImportRegistry$get(input$import_method)
    })

    # ---------- 动态 UI（schema → 控件），bulk_rna 特殊处理 ----------
    output$method_ui <- renderUI({
      req(current_method())
      mid <- input$import_method

      if (mid == "bulk_rna") {
        # bulk_rna 的列选择在 modal 里做，这里只显示元数据控件
        tagList(
          helpText("请上传基因表达矩阵，行为基因，列为样本。"),
          fileInput(ns("infile"), "选择 CSV/TXT", accept = c(".csv", ".txt")),
          hr(),
          selectInput(ns("data_type"), "数据类型",
                      choices = c("auto" = "auto", "count","tpm","fpkm","cpm"), selected = "auto"),
          selectInput(ns("log_state"), "是否log化",
                      choices = c("自动检测" = "auto", "是" = "TRUE", "否" = "FALSE"), selected = "auto"),
          selectInput(ns("species"), "物种",
                      choices = c("自动检测" = "auto", "Human" = "Hs", "Mouse" = "Mm"), selected = "auto"),
          selectInput(ns("id_type"), "基因ID类型",
                      choices = c("自动检测" = "auto", "SYMBOL","ENSEMBL","ENTREZID"), selected = "auto"),
          textAreaInput(ns("group_info"), "分组信息",
                        placeholder = "组名,样本数;...  例: Control,3;Treat,3", rows = 3),
          hr(),
          actionButton(ns("id_convert"), "手动ID转换为SYMBOL", class = "btn-outline-info btn-sm")
        )
      } else {
        # 其他导入方法走通用 schema_to_ui
        schema <- current_method()$schema
        if (length(schema) == 0) return(NULL)
        schema_to_ui(schema, ns, state)
      }
    })

    # ================================================================
    #  bulk_rna：上传文件后弹 modal 选列
    # ================================================================
    observeEvent(input$infile, {
      req(input$import_method == "bulk_rna", input$infile)
      if (is.null(state$name)) {
        showNotification("请先创建项目", type = "error", duration = NULL); return()
      }
      showNotification("读取中...", type = "message", duration = 2)
      tryCatch({
        df <- data.table::fread(input$infile$datapath, header = TRUE, data.table = FALSE)
        df <- na.omit(df)
        temp_df(df)

        showModal(modalDialog(
          title = "确认数据结构", size = "l",
          p("请指定基因ID列和样本列。"),
          selectInput(ns("gene_col"), "基因ID列",
                      choices = names(df), selected = names(df)[1]),
          selectInput(ns("gene_length_col"), "基因长度列（可选，用于 RPKM/FPKM）",
                      choices = c("无" = "", names(df)), selected = ""),
          checkboxGroupInput(ns("sample_col"), "样本列（可多选）",
                             choices = names(df), selected = names(df)[-1]),
          footer = tagList(
            actionButton(ns("cancel_cols"), "取消"),
            actionButton(ns("confirm_cols"), "确认", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error", duration = NULL)
      })
    })

    # 用户取消列选择
    observeEvent(input$cancel_cols, {
      temp_df(NULL); temp_cols(list()); removeModal()
    })

    # 用户确认列选择 → 自动检测数据属性，更新 UI
    observeEvent(input$confirm_cols, {
      req(input$import_method == "bulk_rna")
      df <- temp_df()
      req(df)

      gene_col    <- input$gene_col
      sample_cols <- input$sample_col
      gene_length <- input$gene_length_col

      # 只保留选中列
      df_sel <- df[, c(gene_col, sample_cols), drop = FALSE]
      temp_df(df_sel)
      temp_cols(list(gene_col = gene_col, sample_cols = sample_cols,
                     gene_length = gene_length))

      # 自动检测
      meta <- detect_data(df_sel, list(filename = input$infile$name))
      for (field in c("data_type", "log_state", "species", "id_type")) {
        val <- meta[[field]]
        if (!is.null(val)) {
          updateSelectInput(session, field, selected = as.character(val))
        }
      }
      removeModal()
      showNotification(
        paste0("已选 ", nrow(df_sel), " 行 × ", length(sample_cols), " 个样本"),
        type = "message", duration = 4
      )

      # 若检测到非 SYMBOL，提示转换
      if (!is.null(meta$id_type) && meta$id_type != "SYMBOL") {
        shinyalert(
          title = "ID 转换提示",
          text  = paste0("检测到基因ID为 ", meta$id_type, "，是否在导入时转换为 SYMBOL？"),
          type  = "info",
          showCancelButton  = TRUE,
          confirmButtonText = "是，导入时转换",
          cancelButtonText  = "否，保持原样",
          callbackR = function(value) {
            if (value) updateSelectInput(session, "id_type", selected = meta$id_type)
            # 实际转换在 confirm_input 里根据 id_type != SYMBOL 决定
          }
        )
      }
    })

    # 手动 ID 转换按钮（立即对 temp_df 执行）
    observeEvent(input$id_convert, {
      req(input$import_method == "bulk_rna")
      df <- temp_df(); req(df)
      tryCatch({
        colnames(df)[1] <- "Geneid"
        df <- seqTools::Quick_ID_conversion(
          df, species = input$species, from = input$id_type, to = "SYMBOL"
        )
        temp_df(df)
        updateSelectInput(session, "id_type", selected = "SYMBOL")
        showNotification("ID 转换完成", type = "message")
      }, error = function(e) {
        showNotification(paste("转换失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ================================================================
    #  通用确认导入
    # ================================================================
    observeEvent(input$confirm_input, {
      req(state$name, input$import_method != "")
      method <- current_method(); req(method)
      mid <- input$import_method

      if (mid == "bulk_rna") {
        # bulk_rna：必须已选列
        df <- temp_df()
        if (is.null(df)) {
          showNotification("请先上传文件并确认数据结构", type = "error"); return()
        }
        cols <- temp_cols()
        group_vec <- parse_group_info(input$group_info %||% "")
        n_samples  <- length(cols$sample_cols %||% (ncol(df) - 1))
        if (!is.null(group_vec) && length(group_vec) != n_samples) {
          showNotification(paste0("分组数(", length(group_vec),
                                  ") 与样本数(", n_samples, ") 不匹配"),
                           type = "error", duration = NULL)
          return()
        }

        # 去重策略：优先用 settings
        dup_from_settings <- tryCatch(
          state$settings$analysis$dup %||% "kmax",
          error = function(e) "kmax"
        )

        params <- list(
          gene_col          = cols$gene_col    %||% "",
          sample_cols       = paste(cols$sample_cols %||% "", collapse = ","),
          gene_length       = cols$gene_length %||% "",
          data_type         = input$data_type  %||% "auto",
          log_state         = input$log_state  %||% "auto",
          species           = input$species    %||% "auto",
          id_type           = input$id_type    %||% "auto",
          id_convert        = if (!is.null(input$id_type) && input$id_type != "SYMBOL" &&
                                  input$id_type != "auto") "to_SYMBOL" else "no",
          dup_method        = "settings",
          dup_from_settings = dup_from_settings,
          group_info        = input$group_info %||% ""
        )

        # 把已预处理的 temp_df 写成临时文件，让 IMPORT_RUN 读取
        # （这样 AI 和 UI 都走同一个 IMPORT_RUN 入口，架构一致）
        tmp <- tempfile(fileext = ".csv")
        data.table::fwrite(cbind(rowname__ = rownames(df), df)
                           |> (\(x) { names(x)[1] <- cols$gene_col %||% names(df)[1]; x })(),
                           tmp)
        # 如果 temp_df 已经是纯数值（列选择后），直接 fwrite
        data.table::fwrite(data.frame(Gene = rownames(df), df, check.names = FALSE), tmp)

      } else {
        # 其他方法：从 schema 收集 input
        inputs <- list(); file_path <- NULL
        for (pid in names(method$schema)) {
          pdef <- method$schema[[pid]]
          val  <- input[[pid]]
          if (pdef$type == "file") {
            if (is.list(val) && !is.null(val$datapath)) file_path <- val$datapath
          } else {
            inputs[[pid]] <- val
          }
        }
        if (is.null(file_path)) {
          showNotification("请先上传文件", type = "error"); return()
        }
        tmp    <- file_path
        params <- inputs
      }

      nid <- showNotification("导入中...", type = "message", duration = NULL)
      tryCatch({
        result <- engine$project$import_data(mid, tmp, params)
        if (!isTRUE(result$success) && !is.null(result$error)) stop(result$error)

        state$data       <- engine$project$data
        state$meta       <- engine$project$meta
        state$omics_type <- engine$project$omics_type
        temp_df(NULL); temp_cols(list())

        removeNotification(nid)
        showNotification("导入完成", type = "message")
        nav_select("main_nav", selected = "分析", session = nav_session)
      }, error = function(e) {
        removeNotification(nid)
        showNotification(paste("导入失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ================================================================
    #  数据预览
    # ================================================================
    output$preview <- DT::renderDataTable({
      # 优先展示 temp_df（列选择后、导入前）
      df <- temp_df()
      if (!is.null(df)) {
        return(DT::datatable(head(df, 200),
          options = list(pageLength = input$page_size, scrollX = TRUE, dom = 'tip'),
          rownames = FALSE))
      }
      # 已导入数据
      if (!is.null(state$name) && length(state$data) > 0) {
        mat <- state$data[[1]]
        if (!is.null(mat)) {
          df <- as.data.frame(head(mat, 100))
          return(DT::datatable(df,
            options = list(pageLength = input$page_size, scrollX = TRUE, dom = 'tip')))
        }
      }
      validate(need(FALSE, "请上传文件并确认数据结构"))
    })

    output$debug_info <- renderPrint({
      cat("项目:", state$name %||% "无", "\n")
      cat("导入方法:", input$import_method %||% "无", "\n")
      df <- temp_df()
      if (!is.null(df)) {
        cat("待导入数据:", paste(dim(df), collapse = " x "), "\n")
        cols <- temp_cols()
        cat("基因列:", cols$gene_col %||% "未选", "\n")
        cat("样本列:", length(cols$sample_cols %||% c()), "个\n")
      } else if (length(state$data) > 0) {
        cat("已导入数据:", paste(dim(state$data[[1]]), collapse = " x "), "\n")
      }
    })
  })
}
