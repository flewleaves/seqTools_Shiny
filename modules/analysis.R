# modules/analysis.R

analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "margin-bottom: 10px;",
      actionButton(ns("reload_tools"), "🔄 热加载工具", class = "btn-outline-secondary btn-sm")
    ),
    uiOutput(ns("toolbar")),
    hr(),
    uiOutput(ns("result_area"))
  )
}

analysis_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_tool <- reactiveVal(NULL)
    processing    <- reactiveVal(FALSE)
    bound_tools   <- reactiveVal(character(0))

    # ── 工具列表：ui_only=TRUE 排除 import/system ──────────────────────────
    # 用 reactiveVal 存工具列表，只在热加载或 omics 变化时才更新，不轮询
    tools_snapshot <- reactiveVal(list())

    # 首次加载 + omics 变化时刷新工具列表
    observe({
      omics <- state$omics_type    # omics 变化会触发
      isolate({
        tools_snapshot(
          ToolRegistry$list_tools(omics_type = omics, ui_only = TRUE)
        )
      })
    })

    # 热加载按钮
    observeEvent(input$reload_tools, {
      tryCatch({
        reload_tools(app_root("R", "tools"))
        bound_tools(character(0))
        # 刷新快照
        tools_snapshot(
          ToolRegistry$list_tools(omics_type = state$omics_type, ui_only = TRUE)
        )
        showNotification("工具已热加载", type = "message")
      }, error = function(e) {
        showNotification(paste("热加载失败:", e$message), type = "error", duration = NULL)
      })
    })

    tool_categories <- reactive({
      tools <- tools_snapshot()
      if (length(tools) == 0) return(list())
      cats <- lapply(tools, function(t) t$category)
      nms  <- lapply(tools, function(t) t$name)
      result <- list()
      for (i in seq_along(tools)) {
        cat <- cats[[i]]; nm <- nms[[i]]
        result[[cat]] <- c(result[[cat]], nm)
      }
      result
    })

    # ── toolbar：只在 tool_categories 变化时重渲染 ──────────────────────────
    output$toolbar <- renderUI({
      # 有项目名或有数据均可（AI 导入／加载项目也可能没有项目名）
      if (is.null(state$name) && length(names(state$data)) == 0) {
        return(div(class = "text-center p-5", h4("请创建项目或导入数据")))
      }
      if (length(names(state$data)) == 0) {
        return(div(class = "text-center p-5", h4("请先在数据模块中导入数据")))
      }
      cats <- tool_categories()
      if (length(cats) == 0) return(div("当前组学类型无可用工具"))

      panels <- lapply(names(cats), function(cat) {
        tnames  <- cats[[cat]]
        buttons <- lapply(tnames, function(tname) {
          tool <- ToolRegistry$get(tname)
          if (is.null(tool)) return(NULL)
          label <- tool$display_name %||% tool$name %||% tname
          actionButton(ns(paste0("run_", tname)), label,
                       class = "btn-outline-primary btn-sm m-1")
        })
        buttons <- Filter(Negate(is.null), buttons)
        if (length(buttons) > 0) accordion_panel(cat, do.call(tagList, buttons)) else NULL
      })
      panels <- Filter(Negate(is.null), panels)
      if (length(panels) == 0) return(div("当前组学类型无可用工具"))
      accordion(id = ns("tool_accordion"), open = names(cats)[1], !!!panels)
    })

    # ── 工具执行 ────────────────────────────────────────────────────────────
    do_run_tool <- function(tid, inputs) {
      schema <- ToolRegistry$get(tid)$schema
      required_params <- names(schema)[vapply(schema, function(s) isTRUE(s$required), logical(1))]
      missing <- required_params[vapply(required_params, function(p) {
        v <- inputs[[p]]
        is.null(v) || (length(v) == 1 && is.character(v) && v == "")
      }, logical(1))]
      if (length(missing) > 0) {
        showNotification(paste("缺少必填参数:", paste(missing, collapse = ", ")), type = "error")
        return()
      }

      processing(TRUE)
      current_tool(NULL)
      nid <- showNotification(paste("正在运行", tid, "..."), type = "message", duration = NULL)
      tryCatch({
        result <- engine$run(tid, inputs)
        if (isTRUE(result$success)) {
          state$res <- engine$project$results
          current_tool(tid)
          showNotification(paste(tid, "完成"), type = "message")
        } else {
          showNotification(paste("运行失败:", result$error %||% "未知错误"),
                           type = "error", duration = NULL)
        }
      }, error = function(e) {
        showNotification(paste("运行失败:", e$message), type = "error", duration = NULL)
      }, finally = {
        removeNotification(nid)
        processing(FALSE)
      })
    }

    # ── 动态绑定工具按钮（只绑新出现的，不重复绑定） ───────────────────────
    observe({
      tools        <- tools_snapshot()
      current_bound <- bound_tools()
      all_names    <- sapply(tools, function(t) t$name)
      new_tools    <- setdiff(all_names, current_bound)

      for (tid in new_tools) {
        tool <- ToolRegistry$get(tid)
        if (is.null(tool)) next

        local({
          local_tid    <- tid
          local_schema <- tool$schema

          # 点击工具按钮
          observeEvent(input[[paste0("run_", local_tid)]], {
            tdef <- ToolRegistry$get(local_tid)
            if (length(tdef$schema) > 0) {
              showModal(modalDialog(
                title = paste("设置参数 -", tdef$display_name %||% tdef$name),
                schema_to_ui(tdef$schema, ns, state),
                footer = tagList(
                  actionButton(ns(paste0("cancel_",  local_tid)), "取消"),
                  actionButton(ns(paste0("confirm_", local_tid)), "确认", class = "btn-primary")
                ),
                easyClose = TRUE
              ))
            } else {
              do_run_tool(local_tid, list())
            }
          }, ignoreInit = TRUE)

          # 取消 / 确认 modal
          if (length(local_schema) > 0) {
            observeEvent(input[[paste0("cancel_", local_tid)]],
                         removeModal(), ignoreInit = TRUE)
            observeEvent(input[[paste0("confirm_", local_tid)]], {
              inputs <- lapply(names(local_schema), function(pid) {
                pdef <- local_schema[[pid]]
                val  <- input[[pid]]
                # fileInput 返回 list，取 datapath；其余 list 值置 NULL
                if (!is.null(pdef$type) && pdef$type == "file") {
                  if (is.list(val) && !is.null(val$datapath)) return(val$datapath)
                  return(NULL)
                }
                if (is.list(val)) return(NULL)
                val
              })
              names(inputs) <- names(local_schema)
              removeModal()
              do_run_tool(local_tid, inputs)
            }, ignoreInit = TRUE)
          }
        })
      }

      if (length(new_tools) > 0) bound_tools(c(current_bound, new_tools))
    })

    # ── 结果展示 ────────────────────────────────────────────────────────────
    get_stored_obj <- function(tid) {
      # engine$run 对 named list data 存为 tool_key，对单值存为 tool_res
      # 优先取第一个匹配的结果对象
      tool <- ToolRegistry$get(tid)
      if (is.null(tool)) return(NULL)

      # 先找 named list 存的各个键（如 deg_result, gsea_obj, gsea_dotplot）
      # 再找旧式 _res 键
      all_result_names <- names(engine$project$results)
      prefix <- paste0(tid, "_")
      matching <- all_result_names[startsWith(all_result_names, prefix)]

      # 对有 plot output 的工具，优先找 plot 对象（ggplot）
      if (!is.null(tool$outputs$plot)) {
        for (nm in matching) {
          obj <- engine$project$get_result(nm)
          if (inherits(obj, "ggplot")) return(obj)
        }
      }
      # 对有 table output 的工具，优先找 data.frame / gseaResult
      if (!is.null(tool$outputs$table)) {
        for (nm in matching) {
          obj <- engine$project$get_result(nm)
          if (is.data.frame(obj) || inherits(obj, "gseaResult")) return(obj)
        }
      }
      # 兜底：返回第一个匹配对象
      if (length(matching) > 0) return(engine$project$get_result(matching[1]))
      NULL
    }

    output$result_area <- renderUI({
      if (processing()) return(div(class = "text-center p-5",
                                    h3("分析中，请稍候...", style = "color: grey;")))
      req(current_tool())
      tool <- ToolRegistry$get(current_tool())
      req(tool)

      has_plot  <- !is.null(tool$outputs$plot)
      has_table <- !is.null(tool$outputs$table)

      save_plot_btn  <- actionButton(ns("save_plot_dialog"), "保存图片 (PDF)",
                                     icon = icon("download"), class = "btn-success btn-sm")
      save_table_btn <- downloadButton(ns("download_table"), "保存表格 (CSV)",
                                       class = "btn-success btn-sm")

      # 固定绘图容器，图片自适应
      plot_container <- div(
        style = "width:100%; max-width:960px; margin:0 auto; aspect-ratio:16/9;",
        plotOutput(ns("result_plot"), width = "100%", height = "100%")
      )

      if (has_plot && !has_table) {
        tagList(save_plot_btn, plot_container)
      } else if (!has_plot && has_table) {
        tagList(save_table_btn, DT::dataTableOutput(ns("result_table")))
      } else if (has_plot && has_table) {
        tabsetPanel(
          tabPanel("图形", tagList(save_plot_btn,  plot_container)),
          tabPanel("表格", tagList(save_table_btn, DT::dataTableOutput(ns("result_table"))))
        )
      } else {
        p("此工具无可视化输出")
      }
    })

    # 找 plot 对象（ggplot）
    get_plot_obj <- function(tid) {
      all_result_names <- names(engine$project$results)
      prefix   <- paste0(tid, "_")
      matching <- all_result_names[startsWith(all_result_names, prefix)]
      for (nm in matching) {
        obj <- engine$project$get_result(nm)
        if (inherits(obj, "ggplot")) return(obj)
      }
      NULL
    }

    # 找 table 对象（data.frame / gseaResult / matrix）
    get_table_obj <- function(tid) {
      all_result_names <- names(engine$project$results)
      prefix   <- paste0(tid, "_")
      matching <- all_result_names[startsWith(all_result_names, prefix)]
      for (nm in matching) {
        obj <- engine$project$get_result(nm)
        if (is.data.frame(obj) || inherits(obj, c("gseaResult","matrix"))) return(obj)
      }
      NULL
    }

    # vis 工具：从存储的 vis_key 取回用户选择的对象
    get_vis_obj <- function() {
      key <- engine$project$results[["vis_key"]]
      if (is.null(key) || key == "") return(NULL)
      # R6 查找不到时 fallback 到 state（UI 缓存），提高加载/同步后的可用性
      engine$project$results[[key]] %||% engine$project$data[[key]] %||%
        state$res[[key]] %||% state$data[[key]]
    }

    # 统一取渲染对象
    get_render_obj <- function(tid, for_plot = TRUE) {
      if (tid == "vis") return(get_vis_obj())
      if (for_plot) get_plot_obj(tid) else get_table_obj(tid)
    }

    output$result_plot <- renderPlot({
      req(current_tool())
      tool <- ToolRegistry$get(current_tool())
      req(tool$outputs$plot)
      obj <- get_render_obj(current_tool(), for_plot = TRUE); req(obj)
      tool$outputs$plot(obj)
    })

    output$result_table <- DT::renderDataTable({
      req(current_tool())
      tool <- ToolRegistry$get(current_tool())
      req(tool$outputs$table)
      obj <- get_render_obj(current_tool(), for_plot = FALSE); req(obj)
      df  <- tool$outputs$table(obj); req(df)
      DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$download_table <- downloadHandler(
      filename = function() paste0(current_tool(), "_", Sys.Date(), ".csv"),
      content  = function(file) {
        tool <- ToolRegistry$get(current_tool())
        obj  <- get_render_obj(current_tool(), for_plot = FALSE)
        df   <- tool$outputs$table(obj)
        write.csv(df, file, row.names = FALSE)
      }
    )

    observeEvent(input$save_plot_dialog, {
      showModal(modalDialog(
        title = "设置图片尺寸",
        numericInput(ns("plot_width"),  "宽度 (英寸)", value = 10, min = 3, max = 20, step = 0.5),
        numericInput(ns("plot_height"), "高度 (英寸)", value = 6,  min = 3, max = 20, step = 0.5),
        footer = tagList(
          downloadButton(ns("download_plot_modal"), "下载 PDF", class = "btn-primary"),
          modalButton("取消")
        ),
        easyClose = TRUE
      ))
    })

    output$download_plot_modal <- downloadHandler(
      filename = function() paste0(current_tool(), "_", Sys.Date(), ".pdf"),
      content  = function(file) {
        tool <- ToolRegistry$get(current_tool())
        obj  <- get_render_obj(current_tool(), for_plot = TRUE); req(obj)
        p    <- tool$outputs$plot(obj)
        ggplot2::ggsave(file, plot = p, device = "pdf",
                        width  = input$plot_width  %||% 10,
                        height = input$plot_height %||% 6)
      }
    )
  })
}
