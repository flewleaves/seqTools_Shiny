analysis_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_tool <- reactiveVal(NULL)

    # ---------- 动态生成工具面板 ----------
    output$toolbar <- renderUI({
      if (is.null(state$omics_type)) {
        return(div(class = "text-center p-5", h4("请先在数据模块中导入数据并确认组学类型")))
      }
      
      make_tool_panel <- function(tool_ids) {
        buttons <- lapply(tool_ids, function(tid) {
          if (tid %in% names(tools)) {
            actionButton(ns(paste0("run_", tid)), tools[[tid]]$name,
                         class = "btn-outline-primary btn-sm m-1")
          }
        })
        do.call(tagList, buttons)
      }
      
      omics_type <- state$omics_type
      panels <- lapply(names(tool_categories), function(cat) {
        tool_ids <- tool_categories[[cat]]
        valid_ids <- Filter(function(tid) {
          tool <- tools[[tid]]
          is.null(tool$omics) || omics_type %in% tool$omics
        }, tool_ids)
        if (length(valid_ids) > 0) {
          accordion_panel(cat, make_tool_panel(valid_ids))
        } else {
          NULL
        }
      })
      panels <- Filter(Negate(is.null), panels)
      
      accordion(
        id = ns("tool_accordion"),
        open = names(tool_categories)[1],
        !!!panels
      )
    })

    # ---------- 核心运行函数 ----------
    run_tool <- function(tid, inputs) {
      tool <- tools[[tid]]
      
      shinyjs::runjs(sprintf("
        var resultDiv = document.getElementById('%s');
        if(resultDiv) {
          resultDiv.innerHTML = '<div class=\"text-center p-5\"><h3>分析中，请稍候...</h3></div>';
        }
      ", ns("result_area")))
      
      current_tool(NULL)
      nid <- showNotification(paste("正在运行", tool$name, "..."), type = "message", duration = NULL)
      
      tryCatch({
        if (!is.null(tool$run)) {
          tool$run(state, inputs, session, ns)
        }
        current_tool(tid)
        removeNotification(nid)
        showNotification(paste(tool$name, "完成"), type = "message")
      }, error = function(e) {
        removeNotification(nid)
        showNotification(paste("运行失败:", e$message), type = "error", duration = NULL)
        current_tool(NULL)
      })
    }

    # ---------- 按钮绑定 ----------
    lapply(names(tools), function(tid) {
      observeEvent(input[[paste0("run_", tid)]], {
        tool <- tools[[tid]]
        if (!is.null(tool$params)) {
          param_data <- tool$params(ns, state)
          showModal(modalDialog(
            title = paste("设置参数 -", tool$name),
            param_data$ui,
            footer = tagList(
              actionButton(ns(paste0("cancel_", tid)), "取消"),
              actionButton(ns(paste0("confirm_", tid)), "确认", class = "btn-primary")
            )
          ))
        } else {
          run_tool(tid, list())
        }
      })
    })

    # ---------- 确认/取消绑定 ----------
    lapply(names(tools), function(tid) {
      tool <- tools[[tid]]
      if (!is.null(tool$params)) {
        observeEvent(input[[paste0("cancel_", tid)]], removeModal())
        observeEvent(input[[paste0("confirm_", tid)]], {
          param_data <- tool$params(ns, state)
          ids <- param_data$ids
          inputs_list <- setNames(lapply(ids, function(p) input[[p]]), ids)
          removeModal()
          run_tool(tid, inputs_list)
        })
      }
    })

    # ---------- 动态输出区域 ----------
    output$result_area <- renderUI({
      if (is.null(current_tool())) {
        return(div(class = "text-center p-5", h3("请运行分析工具以查看结果", style = "color: grey;")))
      }
      tool <- tools[[current_tool()]]
      type <- tool$output_type

      save_plot_btn <- actionButton(ns("save_plot_dialog"), "保存图片 (PDF)", 
                                    icon = icon("download"), class = "btn-success btn-sm")
      save_table_btn <- downloadButton(ns("download_table"), "保存表格 (CSV)", 
                                       class = "btn-success btn-sm")

      if (type == "plot") {
        tagList(
          save_plot_btn,
          plotOutput(ns("result_plot"), height = "500px")
        )
      } else if (type == "table") {
        tagList(
          save_table_btn,
          DT::dataTableOutput(ns("result_table"))
        )
      } else if (type == "both") {
        tabsetPanel(
          tabPanel("图形",
            tagList(
              save_plot_btn,
              plotOutput(ns("result_plot"), height = "500px")
            )
          ),
          tabPanel("表格",
            tagList(
              save_table_btn,
              DT::dataTableOutput(ns("result_table"))
            )
          )
        )
      } else {
        p("此工具无可视化输出")
      }
    })

    # ---------- 渲染函数 ----------
    output$result_plot <- renderPlot({
      req(current_tool())
      tool <- tools[[current_tool()]]
      req(tool$plot)
      tool$plot(state)
    })
    
    output$result_table <- DT::renderDataTable({
      req(current_tool())
      tool <- tools[[current_tool()]]
      req(tool$table)
      tool$table(state)
    })

    # ---------- 图片下载（通过模态框中的下载按钮） ----------
    output$download_plot_modal <- downloadHandler(
      filename = function() {
        tool <- tools[[current_tool()]]
        paste0(tool$name, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        tool <- tools[[current_tool()]]
        p <- tool$plot(state)
        w <- input$plot_width %||% 10
        h <- input$plot_height %||% 6
        ggplot2::ggsave(file, plot = p, device = "pdf", width = w, height = h)
      }
    )

    # ---------- 表格下载 ----------
    output$download_table <- downloadHandler(
      filename = function() {
        tool <- tools[[current_tool()]]
        paste0(tool$name, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        tool <- tools[[current_tool()]]
        tab <- tool$table(state)
        if (inherits(tab, "datatables")) {
          write.csv(tab$x$data, file, row.names = FALSE)
        } else if (is.data.frame(tab)) {
          write.csv(tab, file, row.names = FALSE)
        }
      }
    )

    # ---------- 点击“保存图片”按钮 → 弹出尺寸设置对话框 ----------
    observeEvent(input$save_plot_dialog, {
      showModal(modalDialog(
        title = "设置图片尺寸",
        numericInput(ns("plot_width"), "宽度 (英寸)", value = 10, min = 3, max = 20, step = 0.5),
        numericInput(ns("plot_height"), "高度 (英寸)", value = 6, min = 3, max = 20, step = 0.5),
        footer = tagList(
          downloadButton(ns("download_plot_modal"), "下载 PDF", class = "btn-primary"),
          modalButton("取消")
        ),
        easyClose = TRUE
      ))
    })

  }) # 结束 moduleServer 内部的函数
} # 结束 analysis_server 函数