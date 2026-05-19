

analysis_ui <- function(id) {
  ns <- NS(id)

  # 根据分类生成按钮面板
  make_tool_panel <- function(tool_ids) {
    buttons <- lapply(tool_ids, function(tid) {
      if (tid %in% names(tools)) {
        actionButton(ns(paste0("run_", tid)), tools[[tid]]$name,
                     class = "btn-outline-primary btn-sm m-1")
      }
    })
    do.call(tagList, buttons)
  }

  # 构建 accordion
  panels <- lapply(names(tool_categories), function(cat) {
    accordion_panel(cat, make_tool_panel(tool_categories[[cat]]))
  })

  tagList(
    accordion(
      id = ns("tool_accordion"),
      open = names(tool_categories)[1],   # 默认展开第一个
      !!!panels
    ),
    hr(),
    # 输出区域：根据工具类型动态显示 plot 或 table
    uiOutput(ns("result_area"))
  )
}

analysis_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_tool <- reactiveVal(NULL)

    # ---------- 核心运行函数 ----------
    run_tool <- function(tid, inputs) {
      tool <- tools[[tid]]
      tryCatch({
        if (!is.null(tool$run)) {
          tool$run(state, inputs, session, ns)   # 传递 session 和 ns
        }
        current_tool(tid)
        showNotification(paste(tool$name, "完成"), type = "message")
      }, error = function(e) {
        showNotification(paste("运行失败:", e$message), type = "error", duration = NULL)
      })
    }

    # ---------- 为所有工具绑定主按钮 ----------
    lapply(names(tools), function(tid) {
      observeEvent(input[[paste0("run_", tid)]], {
        tool <- tools[[tid]]
        
        if (!is.null(tool$params)) {
          # 获取基础参数结构
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

    # ---------- 为有参数的工具绑定确认/取消 ----------
    lapply(names(tools), function(tid) {
      tool <- tools[[tid]]
      if (!is.null(tool$params)) {
        observeEvent(input[[paste0("cancel_", tid)]], {
          removeModal()
        })
        observeEvent(input[[paste0("confirm_", tid)]], {
          param_data <- tool$params(ns,state)   # 再次获取 ids
          ids <- param_data$ids
          # 从模块命名空间里收集参数值
          inputs_list <- setNames(lapply(ids, function(p) input[[p]]), ids)
          removeModal()
          run_tool(tid, inputs_list)
        })
      }
    })

    # ---------- 动态输出区域 ----------
    output$result_area <- renderUI({
      req(current_tool())
      tool <- tools[[current_tool()]]
      if (tool$output_type == "plot") {
        plotOutput(ns("result_plot"), height = "500px")
      } else if (tool$output_type == "table") {
        DT::dataTableOutput(ns("result_table"))
      } else {
        p("此工具无可视化输出")
      }
    })

    # ---------- 渲染函数 ----------
    output$result_plot <- renderPlot({
      req(current_tool())
      req(tools[[current_tool()]]$plot)
      tools[[current_tool()]]$plot(state)
    })

    output$result_table <- DT::renderDataTable({
      req(current_tool())
      req(tools[[current_tool()]]$plot)
      tools[[current_tool()]]$plot(state)
    })
  })
}