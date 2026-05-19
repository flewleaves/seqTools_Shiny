data_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "读取数据",
      
      # 选择组学类型的按钮组（高亮）
      shinyWidgets::radioGroupButtons(
        inputId = ns("import_method"),
        label = "选择组学类型",
        choices = setNames(
          names(import_methods),
          sapply(import_methods, `[[`, "name")
        ),
        selected = "bulk_rna",
        status = "primary"
      ),
      hr(),
      
      # 这里动态显示当前导入方法对应的 UI
      uiOutput(ns("method_ui")),
      
      hr(),
      sliderInput(ns("page_size"), "每页行数", min = 5, max = 100, value = 10),
      actionButton(ns("confirm_input"), "确认输入", class = "btn-info")
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

    # ---------- 当前选中的导入方法 ----------
    current_method <- reactive({
      req(input$import_method)
      import_methods[[input$import_method]]
    })

    # ---------- 动态生成导入方法的 UI ----------
    output$method_ui <- renderUI({
      req(current_method())
      current_method()$ui(ns)
    })

    # ====================================================================
    #   以下保留 RNA‑seq 特有的交互逻辑（如列选择、ID 转换等）
    #   通过 req(input$import_method == "bulk_rna") 限制其只在 RNA‑seq 模式下生效
    # ====================================================================

    # ---- 1. RNA‑seq：上传文件后的列选择对话框 ----
    observeEvent(input$infile, {
      req(input$import_method == "bulk_rna")
      showNotification("读取中...", type = "message", duration = 2)
      
      if (is.null(state$name)) {
        showNotification("请先创建项目", type = "error", duration = NULL)
        return()
      }
      
      tryCatch({
        df <- data.table::fread(input$infile$datapath, header = TRUE, data.table = FALSE)
        state$meta[["filename"]] <- input$infile$name
        state$temp_df <- na.omit(df)
        
        showModal(
          modalDialog(
            title = "确认数据结构", size = "l",
            p("请指定基因 ID 列和样本表达值列。"),
            selectInput(ns("gene_col"), "基因 ID 列",
                        choices = names(df), selected = names(df)[1]),
            selectInput(ns("gene_length"), "基因长度信息(可选)",
                        choices = c("", names(df)), selected = ""),
            checkboxGroupInput(ns("sample_col"), "样本列（可多选）",
                               choices = names(df), selected = names(df)[-1]),
            footer = tagList(
              actionButton(ns("cancel_cols"), "取消"),
              actionButton(ns("confirm_cols"), "确认", class = "btn-primary")
            ),
            easyClose = FALSE
          )
        )
      }, error = function(e) {
        showNotification(paste("失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ---- 2. RNA‑seq：确认列选择 ----
    observeEvent(input$confirm_cols, {
      req(input$import_method == "bulk_rna")
      
      state$temp_df <- state$temp_df[, c(input$gene_col, input$sample_col)]
      state$meta[["gene.length"]] <- if (is.null(input$gene_length) || input$gene_length == "") NULL else input$gene_length
      
      detected <- detect_data(state$temp_df, state$meta)
      showNotification("自动检测数据属性中...", type = "message", duration = 5)
      state$meta <- detected
      update_fields <- c("data_type", "log_state", "species", "id_type")
      for (x in update_fields) {
        if (!is.null(detected[[x]])) {
          updateSelectInput(session, x, selected = detected[[x]])
        }
      }
      removeModal()
      showNotification(paste("导入:", nrow(state$temp_df), "x", ncol(state$temp_df)),
                       type = "message", duration = 5)
      
      if (!is.null(detected$id_type) && detected$id_type != "SYMBOL") {
        showModal(modalDialog(
          title = "ID 转换提示",
          p(paste0("检测到基因 ID 类型为 ", detected$id_type, "，是否转换为 SYMBOL？")),
          footer = tagList(
            actionButton(ns("skip_convert"), "否，保持原样"),
            actionButton(ns("do_convert"), "是，转换", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      }
    })

    observeEvent(input$cancel_cols, {
      state$temp_df <- NULL
      removeModal()
    })
    observeEvent(input$skip_convert, {
      removeModal()
    })

    # ---- 3. RNA‑seq：手动属性更新 ----
    observeEvent(input$data_type, {
      req(input$import_method == "bulk_rna")
      state$meta[["data_type"]] <- input$data_type
    })
    observeEvent(input$id_type, {
      req(input$import_method == "bulk_rna")
      state$meta[["id_type"]] <- input$id_type
    })
    observeEvent(input$log_state, {
      req(input$import_method == "bulk_rna")
      state$meta[["log_state"]] <- input$log_state
    })
    observeEvent(input$species, {
      req(input$import_method == "bulk_rna")
      state$meta[["species"]] <- input$species
    })
    observeEvent(input$group_info, {
      req(input$import_method == "bulk_rna")
      state$meta[["group_info"]] <- parse_group_info(input$group_info)
    })

    # ---- 4. RNA‑seq：ID 转换按钮 ----
    observeEvent(input$id_convert, {
      req(input$import_method == "bulk_rna")
      tryCatch({
        colnames(state$temp_df)[1] <- "Geneid"
        state$temp_df <- seqTools::Quick_ID_conversion(
          state$temp_df, species = state$meta[["species"]],
          from = state$meta[["id_type"]], to = "SYMBOL"
        )
        state$meta[["id_type"]] <- "SYMBOL"
      }, error = function(e) {
        showNotification(paste("失败:", e$message), type = "error", duration = NULL)
      })
    })

    observeEvent(input$do_convert, {
      tryCatch({
        colnames(state$temp_df)[1] <- "Geneid"
        state$temp_df <- seqTools::Quick_ID_conversion(
          state$temp_df, species = state$meta[["species"]],
          from = state$meta[["id_type"]], to = "SYMBOL"
        )
        state$meta[["id_type"]] <- "SYMBOL"
      }, error = function(e) {
        showNotification(paste("失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ====================================================================
    #   通用确认输入按钮（所有导入方法共用）
    # ====================================================================
    observeEvent(input$confirm_input, {
      method <- current_method()
      
      # 1. 调用方法自己的验证函数
      valid <- method$validate(input, state)
      if (is.character(valid)) {
        showNotification(valid, type = "error", duration = NULL)
        return()
      }
      
      # 2. 执行导入
      tryCatch({
        method$run(input, state, session, ns)
        # 导入成功后自动跳转到“分析”面板
        nav_select("main_nav", selected = "分析", session = nav_session)
      }, error = function(e) {
        showNotification(paste("导入失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ====================================================================
    #   数据预览（根据当前导入方法动态显示）
    # ====================================================================
    output$preview <- DT::renderDataTable({
      validate(need(!is.null(state$name), "请先创建项目"))
      method <- current_method()
      # 使用方法的预览函数获取数据，如果没有则返回 NULL
      data <- if (!is.null(method$preview_data)) method$preview_data(input, state) else NULL
      validate(need(!is.null(data), "请先导入数据"))
      
      DT::datatable(data, options = list(
        pageLength = input$page_size,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'lfrtip'
      ), rownames = FALSE)
    })

    # ---- 调试信息 ----
    output$debug_info <- renderPrint({
      cat("项目:", ifelse(is.null(state$name), "无", state$name), "\n")
      cat("当前导入方法:", input$import_method, "\n")
      if (input$import_method == "bulk_rna") {
        cat("临时数据:", ifelse(is.null(state$temp_df), "NULL", paste(dim(state$temp_df), collapse = " x ")), "\n")
      } else if (input$import_method == "single_cell") {
        cat("Seurat对象:", ifelse(is.null(state$seurat_obj), "未创建", "已创建"), "\n")
      }
    })
  })
}