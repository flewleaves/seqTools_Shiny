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
      if (is.null(state$omics_type)) {
        return(div(class = "text-center p-5", h4("请先在数据模块中导入数据并确认组学类型")))
      }
      
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
      
      # 1. 立即用 JS 隐藏结果区并显示“分析中”提示
      #    因为 Shiny 单线程，这一步会在浏览器立即生效
      shinyjs::runjs(sprintf("
        var resultDiv = document.getElementById('%s');
        if(resultDiv) {
          resultDiv.innerHTML = '<div class=\"text-center p-5\"><h3>分析中，请稍候...</h3></div>';
        }
      ", ns("result_area")))   # 注意：结果区必须有一个 id 为 ns("result_area") 的容器
      
      # 2. 清空当前工具标记，防止旧输出残留
      current_tool(NULL)
      
      # 3. 执行工具计算（期间浏览器保持“分析中”）
      nid <- showNotification(paste("正在运行", tool$name, "..."), type = "message", duration = NULL)
      
      tryCatch({
        if (!is.null(tool$run)) {
          tool$run(state, inputs, session, ns)
        }
        current_tool(tid)   # 成功后标记当前工具
        removeNotification(nid)
        showNotification(paste(tool$name, "完成"), type = "message")
      }, error = function(e) {
        removeNotification(nid)
        showNotification(paste("运行失败:", e$message), type = "error", duration = NULL)
        current_tool(NULL)  # 失败时保持无工具状态
      })
      # tryCatch 结束后，current_tool(tid) 会触发正常的 renderUI 恢复界面
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
    # ---------- 动态输出区域 ----------
    output$result_area <- renderUI({
      if (is.null(current_tool())) {
        return(div(class = "text-center p-5", h3("请运行分析工具以查看结果", style = "color: grey;")))
      }
      tool <- tools[[current_tool()]]
      type <- tool$output_type
      
      if (type == "plot") {
          plotOutput(ns("result_plot"), height = "500px")
        } else if (type == "table") {
          DT::dataTableOutput(ns("result_table"))
        } else if (type == "both") {
          tabsetPanel(
            tabPanel("图形", plotOutput(ns("result_plot"), height = "500px")),
            tabPanel("表格", DT::dataTableOutput(ns("result_table")))
          )
        } else {
          p("此工具无可视化输出")
        }
    })

    # ---------- 渲染函数 ----------
    output$result_plot <- renderPlot({
      req(current_tool())
      tool <- tools[[current_tool()]]
      # 只有 output_type 为 "plot" 或 "both" 时才有图形
      req(tool$plot)
      tool$plot(state)
    })
    
    output$result_table <- DT::renderDataTable({
      req(current_tool())
      tool <- tools[[current_tool()]]
      # 只有 output_type 为 "table" 或 "both" 时才有表格
      req(tool$table)
      tool$table(state)
    })
  })
}