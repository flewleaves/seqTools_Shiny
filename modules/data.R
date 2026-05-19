# 数据模块：导入 + 预览

data_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "读取数据",
      helpText("请上传基因表达矩阵，行为基因表达水平，列包含不同样本信息。第一行应为样本名。"),
      
      # 或者更详细的带格式说明
      tags$div(
        style = "font-size: 0.9em; color: #666; margin-bottom: 10px;",
        tags$strong("格式要求："),
        tags$ul(
          tags$li("第一列：基因 ID（唯一）"),
          tags$li("第一行：样本名称"),
          tags$li("数值：原始 counts 或 TPM")
        )
      ),
      
      fileInput(ns("infile"), "选择 CSV/TXT", accept = c(".csv", ".txt")),
      hr(),
      selectInput(ns("data_type"), "数据类型",
                  choices = c("", "count", "TPM", "FPKM", "CPM"),
                  selected = ""),

      selectInput(ns("log_state"), "是否log化",
                  choices = c("", TRUE, FALSE),
                  selected = ""),
      
      selectInput(ns("species"), "物种",
                  choices = c("", "Human", "Mouse"),
                  selected = ""),
      
      selectInput(ns("id_type"), "基因 ID 类型",
                  choices = c("", "SYMBOL", "ENSEMBL", "ENTREZID"),
                  selected = ""),
      textAreaInput(ns("group_info"), "分组信息",
                    placeholder = "格式：组名,样本数,不同组用半角;隔开,区分大小写。例如：SiRNA,3;Ctrl,3;SiRNA,2",
                    rows = 3),
      hr(),
      sliderInput(ns("page_size"), "每页行数", min = 5, max = 100, value = 10),
      actionButton(ns("id_convert"), "基因ID转换", class = "btn-info"),
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
    # 导入数据
    observeEvent(input$infile, {
      showNotification("读取中...", type = "message", duration = 2)
      
      if (is.null(state$name)) {
        showNotification("请先创建项目", type = "error", duration = NULL)
        return()
      }
      
      tryCatch({
        df <- data.table::fread(input$infile$datapath, header = TRUE, data.table = FALSE)
        state$meta[["filename"]] = input$infile$name
        # 临时存放，等用户确认列后再正式保存
        state$temp_df <- na.omit(df)
        
        # 弹出列选择对话框
        showModal(
          modalDialog(
            title = "确认数据结构",
            size = "l",
            p("请指定基因 ID 列和样本表达值列。"),
            helpText("第一列通常是基因名，其余为样本。"),
            
            selectInput(ns("gene_col"), "基因 ID 列",
                        choices = names(df), selected = names(df)[1]),
            selectInput(ns("gene_length"), "基因长度信息(可选)",
                        choices = c("",names(df)), selected = ""),
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
    
    # ---------- 确认列选择：这里执行你原来的后续逻辑 ----------
    observeEvent(input$confirm_cols, {
      
      # 1. 正式保存数据
      state$temp_df <- state$temp_df[,c(input$gene_col,input$sample_col)]
      state$gene.length = ifelse(is.null(input$gene_length), NULL, input$gene_length)
      # 3. 自动检测（原来在 input$infile 里的逻辑移到这里）
      detected <- detect_data(state$temp_df, state$meta)
      showNotification("自动检测数据属性中...", 
                       type = "message", duration = 5)
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
        showModal(
          modalDialog(
            title = "ID 转换提示",
            p(paste0("检测到基因 ID 类型为 ", detected$id_type, "，是否转换为 SYMBOL？")),
            footer = tagList(
              actionButton(ns("skip_convert"), "否，保持原样"),
              actionButton(ns("do_convert"), "是，转换", class = "btn-primary")
            ),
            easyClose = FALSE
          )
        )
      }
    })
    
    #取消分支
    observeEvent(input$cancel_cols, {
      state$temp_df <- NULL
      removeModal()
    })

    observeEvent(input$skip_convert, {
      removeModal()
    })

    #读取属性
    observeEvent(input$data_type, {
        state$meta[["data_type"]] = input$data_type
    })

    observeEvent(input$id_type, {
        state$meta[["id_type"]] = input$id_type
    })

    observeEvent(input$log_state, {
        state$meta[["log_state"]] = input$log_state
    })

    observeEvent(input$species, {
        state$meta[["species"]] = input$species
    })

    observeEvent(input$group_info, {
        state$meta[["group_info"]] = parse_group_info(input$group_info)
    })

    #ID转换
    observeEvent(input$id_convert, {
        tryCatch({colnames(state$temp_df)[1] = "Geneid"
                  state$temp_df <- seqTools::Quick_ID_conversion(state$temp_df, species = state$meta[["species"]], from = state$meta[["id_type"]], to = "SYMBOL")
                  state$meta[["id_type"]] = "SYMBOL"
        },
                error = function(e){
                  showNotification(paste("失败:", e$message), type = "error", duration = NULL)
                })
    })

    observeEvent(input$do_convert, {
        tryCatch({colnames(state$temp_df)[1] = "Geneid"
                  state$temp_df <- seqTools::Quick_ID_conversion(state$temp_df, species = state$meta[["species"]], from = state$meta[["id_type"]], to = "SYMBOL")
                  state$meta[["id_type"]] = "SYMBOL"
        },
                error = function(e){
                  showNotification(paste("失败:", e$message), type = "error", duration = NULL)
                })
    })

    #确认最终输入
    observeEvent(input$confirm_input, {
        
        if (is.null(state$temp_df)) {
          showNotification("失败：请先导入并确认数据", type = "error", duration = NULL)
          return()
        }
        
        if (length(state$meta[["group_info"]]) != ncol(state$temp_df) - 1) {
          showNotification("失败：分组信息有误", type = "error", duration = NULL)
          return()  # 直接返回，不执行后续操作
        }
        
        temp <- state$temp_df
        showNotification("去除重复基因中...", type = "message", duration = 5)
        temp <- seqTools::remove_dup(temp, 1, method = state$settings$dup)
        showNotification(paste0("原矩阵",nrow(state$temp_df),"行", "剩余", nrow(temp), "行"), type = "message", duration = 5)
        row.names(temp) = temp[,1]
        temp <- temp[,-1]
        if(state$meta[["data_type"]] == "count") temp <- round(temp)
        state$data[[state$meta[["data_type"]]]] <- temp
        state$temp_df = NULL
        nav_select("main_nav", selected = "分析", session = nav_session)
    })

    # 调试信息
    output$debug_info <- renderPrint({
      cat("项目:", ifelse(is.null(state$name), "无", state$name), "\n")
      cat("数据:", ifelse(is.null(state$temp_df), "NULL", paste(dim(state$temp_df), collapse = " x ")), "\n")
    })
    
    # 表格
    output$preview <- DT::renderDataTable({
      validate(need(!is.null(state$name), "请先创建项目"))
      validate(need(!is.null(state$temp_df), "请先导入数据"))
      
      DT::datatable(state$temp_df,
        options = list(
          pageLength = input$page_size,
          lengthMenu = c(5, 10, 25, 50, 100),
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'lfrtip'
        ),
        rownames = FALSE)
    })

  })
}

