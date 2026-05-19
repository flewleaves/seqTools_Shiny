analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 动态生成工具面板（替换原来静态的 accordion）
    uiOutput(ns("toolbar")),
    hr(),
    uiOutput(ns("result_area"))
  )
}

analysis_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_tool <- reactiveVal(NULL)

    # ---------- 动态生成工具面板 ----------
    output$toolbar <- renderUI({
      req(state$omics_type)   # 确保组学类型已设置
      
      # 辅助函数：从工具 ID 生成按钮
      make_tool_panel <- function(tool_ids) {
        buttons <- lapply(tool_ids, function(tid) {
          if (tid %in% names(tools)) {
            actionButton(ns(paste0("run_", tid)), tools[[tid]]$name,
                         class = "btn-outline-primary btn-sm m-1")
          }
        })
        do.call(tagList, buttons)
      }
      
      # 过滤工具：保留通用工具或匹配当前组学类型的工具
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
      # 移除空分类
      panels <- Filter(Negate(is.null), panels)
      
      accordion(
        id = ns("tool_accordion"),
        open = names(tool_categories)[1],
        !!!panels
      )
    })

    # ---------- 核心运行函数（完全不变） ----------
    run_tool <- function(tid, inputs) {
      tool <- tools[[tid]]
      tryCatch({
        if (!is.null(tool$run)) {
          tool$run(state, inputs, session, ns)
        }
        current_tool(tid)
        showNotification(paste(tool$name, "完成"), type = "message")
      }, error = function(e) {
        showNotification(paste("运行失败:", e$message), type = "error", duration = NULL)
      })
    }

    # ---------- 按钮绑定（完全不变） ----------
    lapply(names(tools), function(tid) {
      observeEvent(input[[paste0("run_", tid)]], {
        tool <- tools[[tid]]
        if (!is.null(tool$params)) {
          param_data <- tool$params(ns,state)
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

    # ---------- 确认/取消绑定（完全不变） ----------
    lapply(names(tools), function(tid) {
      tool <- tools[[tid]]
      if (!is.null(tool$params)) {
        observeEvent(input[[paste0("cancel_", tid)]], removeModal())
        observeEvent(input[[paste0("confirm_", tid)]], {
          param_data <- tool$params(ns,state)
          ids <- param_data$ids
          inputs_list <- setNames(lapply(ids, function(p) input[[p]]), ids)
          removeModal()
          run_tool(tid, inputs_list)
        })
      }
    })

    # ---------- 动态输出区域（完全不变） ----------
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