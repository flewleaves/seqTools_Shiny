# modules/analysis.R

analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "margin-bottom: 10px;",
      actionButton(ns("reload_tools"), "🔄 热加载工具", class = "btn-outline-secondary btn-sm"),
      actionButton(ns("pull_ai_results"), "📥 拉取AI结果", class = "btn-outline-secondary btn-sm"),
      actionButton(ns("select_tools"), "🔧 选择工具", class = "btn-outline-secondary btn-sm")
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
    tool_run_count <- reactiveVal(0)     # renderPlot 缓存版本号

    # ── 工具列表：获取所有工具（不按 omics 过滤） ──────────────────────────
    all_tools <- reactiveVal(list())
    selected_tools <- reactiveVal(character(0))  # 用户选中的工具 id

    # 首次加载时获取所有工具
    observe({
      isolate({
        all_tools(
          ToolRegistry$list_tools(omics_type = NULL, ui_only = TRUE)
        )
      })
    })

    # omics_type 变化时自动选择对应工具
    observeEvent(state$omics_type, {
      omics <- state$omics_type
      tools <- ToolRegistry$list_tools(omics_type = NULL, ui_only = TRUE)
      all_tools(tools)

      if (!is.null(omics) && omics %in% c("bulk_rna", "single_cell")) {
        # 自动选择匹配 omics 的工具（含 omics=NULL 的通用工具）
        auto <- sapply(tools, function(t) {
          is.null(t$omics) || omics %in% t$omics
        })
        selected_tools(sapply(tools[auto], `[[`, "id"))
      } else {
        # 自定义数据：不预选，等用户手动选择
        selected_tools(character(0))
      }
    }, ignoreNULL = FALSE)

    # 热加载按钮
    observeEvent(input$reload_tools, {
      tryCatch({
        reload_tools(app_root("R", "tools"))
        bound_tools(character(0))
        tools <- ToolRegistry$list_tools(omics_type = NULL, ui_only = TRUE)
        all_tools(tools)
        # 保持当前选择（清除已不存在的工具）
        cur <- selected_tools()
        available <- sapply(tools, `[[`, "id")
        selected_tools(intersect(cur, available))
        showNotification("工具已热加载", type = "message")
      }, error = function(e) {
        showNotification(paste("热加载失败:", e$message), type = "error", duration = NULL)
      })
    })

    notify <- function(msg, type = "message") {
      tryCatch(shiny::showNotification(msg, type = type, duration = 3, session = session),
               error = function(e) message("[notify] ", msg))
    }
    observeEvent(input$pull_ai_results, {
      notify("正在拉取...")
      tryCatch({
        pull_state_from_python(state)
        tool_run_count(tool_run_count() + 1)
        keys <- names(engine$project$results)
        plot_keys <- keys[grep("_plot$", keys)]
        notify(if (length(plot_keys)) paste("已拉取:", paste(plot_keys, collapse=", "))
               else paste("结果:", length(keys), "个"))
      }, error = function(e) {
        notify(paste("拉取失败:", e$message), "error")
      })
    })

    # ── 工具选择对话框 ──────────────────────────────────────────────────────
    observeEvent(input$select_tools, {
      show_tool_selection_dialog()
    })

    show_tool_selection_dialog <- function() {
      tools <- all_tools()
      cur_sel <- selected_tools()

      # 按 category 分组
      cats <- unique(sapply(tools, `[[`, "category"))
      cats <- cats[order(cats)]

      checkbox_groups <- lapply(cats, function(cat) {
        cat_tools <- Filter(function(t) t$category == cat, tools)
        cat_names <- sapply(cat_tools, `[[`, "id")
        cat_labels <- sapply(cat_tools, function(t) t$display_name %||% t$name)
        tagList(
          tags$h6(cat, style = "margin-top:10px;"),
          checkboxGroupInput(ns(paste0("chk_cat_", gsub("[^a-zA-Z0-9]", "_", cat))),
                             label = NULL,
                             choiceNames  = cat_labels,
                             choiceValues = cat_names,
                             selected = intersect(cur_sel, cat_names))
        )
      })

      showModal(modalDialog(
        title = "选择分析工具",
        size = "l",
        tagList(
          tags$div(style = "margin-bottom:10px;",
            actionButton(ns("sel_all"), "全选", class = "btn-sm"),
            actionButton(ns("sel_none"), "清除", class = "btn-sm"),
            actionButton(ns("sel_bulk"), "Bulk RNA", class = "btn-sm"),
            actionButton(ns("sel_sc"),   "Single Cell", class = "btn-sm")
          ),
          checkbox_groups
        ),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_tool_selection"), "确认", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }

    # 快捷选择按钮
    observeEvent(input$sel_all, {
      tools <- all_tools()
      cats <- unique(sapply(tools, `[[`, "category"))
      all_ids <- sapply(tools, `[[`, "id")
      for (cat in cats) {
        cat_tools <- Filter(function(t) t$category == cat, tools)
        cat_ids <- sapply(cat_tools, `[[`, "id")
        updateCheckboxGroupInput(session,
          paste0("chk_cat_", gsub("[^a-zA-Z0-9]", "_", cat)),
          selected = intersect(all_ids, cat_ids))
      }
    })

    observeEvent(input$sel_none, {
      tools <- all_tools()
      cats <- unique(sapply(tools, `[[`, "category"))
      for (cat in cats) {
        updateCheckboxGroupInput(session,
          paste0("chk_cat_", gsub("[^a-zA-Z0-9]", "_", cat)),
          selected = character(0))
      }
    })

    auto_select_by_omics <- function(omics) {
      tools <- all_tools()
      auto <- sapply(tools, function(t) is.null(t$omics) || omics %in% t$omics)
      auto_ids <- sapply(tools[auto], `[[`, "id")
      cats <- unique(sapply(tools, `[[`, "category"))
      for (cat in cats) {
        cat_tools <- Filter(function(t) t$category == cat, tools)
        cat_ids <- sapply(cat_tools, `[[`, "id")
        updateCheckboxGroupInput(session,
          paste0("chk_cat_", gsub("[^a-zA-Z0-9]", "_", cat)),
          selected = intersect(auto_ids, cat_ids))
      }
    }

    observeEvent(input$sel_bulk, { auto_select_by_omics("bulk_rna") })
    observeEvent(input$sel_sc,   { auto_select_by_omics("single_cell") })

    observeEvent(input$confirm_tool_selection, {
      tools <- all_tools()
      cats <- unique(sapply(tools, `[[`, "category"))
      all_sel <- character(0)
      for (cat in cats) {
        sel <- input[[paste0("chk_cat_", gsub("[^a-zA-Z0-9]", "_", cat))]]
        if (!is.null(sel)) all_sel <- c(all_sel, sel)
      }
      selected_tools(all_sel)
      removeModal()
    })

    # ── 按选中工具构建分类 ──────────────────────────────────────────────────
    tool_categories <- reactive({
      tools <- all_tools()
      sel <- selected_tools()
      if (length(sel) == 0) return(list())
      # 只保留选中的
      tools <- Filter(function(t) t$id %in% sel, tools)
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

    # ── toolbar：只在 selected_tools 变化时重渲染（isolate 切断无关依赖） ──
    output$toolbar <- renderUI({
      cats <- tool_categories()
      # 数据可用性仅在初始状态检测，之后用 isolate 避免每次数据变更就重渲染
      sel <- selected_tools()
      if (length(sel) == 0) {
        no_data  <- isolate(!nzchar(state$data_keys))
        no_proj  <- isolate(is.null(state$name))
        if (no_proj && no_data)
          return(div(class = "text-center p-5", h4("请创建项目或导入数据")))
        if (no_data)
          return(div(class = "text-center p-5", h4("请先在数据模块中导入数据")))
        return(div(class = "text-center p-5",
          h4("请点击「选择工具」选择要使用的分析工具"),
          actionButton(ns("select_tools_init"), "🔧 选择工具", class = "btn-primary btn-lg")
        ))
      }
      if (length(cats) == 0) return(div("当前无可用工具"))

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
      if (length(panels) == 0) return(div("当前无可用工具"))
      accordion(id = ns("tool_accordion"), open = names(cats)[1], !!!panels)
    })

    # 首次进入时自动弹出选择对话框
    observeEvent(input$select_tools_init, {
      show_tool_selection_dialog()
    })
    # 有数据但没选工具时自动弹窗
    observe({
      tools <- all_tools()
      if (length(tools) > 0 &&
          nzchar(state$data_keys) &&
          length(selected_tools()) == 0) {
        # 如果 omics_type 已设置，直接自动选择而不弹窗
        if (!is.null(state$omics_type) && state$omics_type %in% c("bulk_rna", "single_cell")) {
          auto <- sapply(tools, function(t) {
            is.null(t$omics) || state$omics_type %in% t$omics
          })
          selected_tools(sapply(tools[auto], `[[`, "id"))
        }
      }
    })

    # ── 工具执行 ────────────────────────────────────────────────────────────
    do_run_tool <- function(tid, inputs) {
      if (processing()) {
        showNotification("正在运行中，请稍候...", type = "warning")
        return()
      }
      schema <- ToolRegistry$get(tid)$schema
      required_params <- names(schema)[vapply(schema, function(s) isTRUE(s$required), logical(1))]
      # 与 engine$validate_inputs 的 is_blank 保持一致
      is_blank <- function(v) {
        if (is.null(v)) return(TRUE)
        if (identical(v, "")) return(TRUE)
        (length(v) == 1 && is.na(v) && !is.logical(v))
      }
      # 只检查没有默认值的必填参数（有默认值的由 validate_inputs 处理）
      missing <- required_params[vapply(required_params, function(p) {
        s <- schema[[p]]
        if (!is.null(s$default)) return(FALSE)  # 有默认值，交给 validate_inputs
        is_blank(inputs[[p]])
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
          # 只同步轻量 keys（大矩阵在 engine$project 中单例存储）
          state$data_keys <- paste(names(engine$project$data), collapse = ",")
          state$res_keys  <- paste(names(engine$project$results), collapse = ",")
          state$data <- engine$project$data
          state$res  <- engine$project$results
          state$meta <- engine$project$meta
          # 只在工具可能修改 meta.data 时才清缓存
          if (tid %in% c("sc_cluster", "sc_annotate", "sc_quick_pipeline",
                         "sc_integrate", "sc_sctransform", "sc_preprocessing",
                         "sc_normalize", "sc_cell_ratio")) {
            engine$project$meta$sc_meta_cols <- NULL
            engine$project$meta$sc_cluster_ids <- NULL
          }
          current_tool(tid)
          tool_run_count(tool_run_count() + 1)
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
      tools        <- all_tools()
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
            if (processing()) {
              showNotification("正在运行中，请稍候...", type = "warning")
              return()
            }
            tdef <- ToolRegistry$get(local_tid)
            if (length(tdef$schema) > 0) {
              removeModal()
              showModal(modalDialog(
                title = paste("设置参数 -", tdef$display_name %||% tdef$name),
                schema_to_ui(tdef$schema, ns, state),
                footer = tagList(
                  actionButton(ns(paste0("cancel_",  local_tid)), "取消"),
                  actionButton(ns(paste0("confirm_", local_tid)), "确认", class = "btn-primary")
                ),
                easyClose = TRUE
              ))

              # 级联下拉：observeEvent 监听源参数，updateCheckboxGroupInput 更新目标
              for (pid in names(tdef$schema)) {
                pdef <- tdef$schema[[pid]]
                src <- pdef$cascade_from
                if (is.null(src)) next
                local({
                  local_pid <- pid; local_src <- src; local_multi <- isTRUE(pdef$multiple)
                  update_cascade_choices <- function(col) {
                    vals <- tryCatch({
                      sc_key <- sc_find_seurat_key(engine$project, NULL)
                      val <- engine$project$data[[sc_key]]
                      # 只从内存读取（不触发磁盘 I/O），避免每次弹窗卡顿
                      sobj <- if (!is.character(val) && inherits(val, "Seurat")) val else NULL
                      if (!is.null(sobj) && col %in% colnames(sobj@meta.data)) {
                        uv <- sort(unique(as.character(sobj@meta.data[[col]])))
                        uv <- uv[!is.na(uv) & nzchar(uv)]
                        if (length(uv) > 0) setNames(uv, uv) else c("无数据" = "")
                      } else c("无数据" = "")
                    }, error = function(e) c("加载失败" = ""))
                    if (local_multi) {
                      updateCheckboxGroupInput(session, local_pid,
                        choiceNames = names(vals), choiceValues = unname(vals),
                        selected = character(0))
                    } else {
                      updateSelectInput(session, local_pid, choices = vals)
                    }
                  }
                  observeEvent(input[[local_src]], {
                    update_cascade_choices(input[[local_src]])
                  }, ignoreNULL = TRUE, ignoreInit = FALSE)
                })
              }

              # ── sc_cor 两级级联：group_by(多选) → ident_group → target_group ──
              if (local_tid == "sc_cor") {
                # 更新 target_group 的下拉选项（根据 ident_group 列读取值）
                update_tg <- function(ig) {
                  if (is.null(ig) || !nzchar(ig)) {
                    updateCheckboxGroupInput(session, "target_group",
                      choiceNames = "请先选择身份列", choiceValues = "")
                    return()
                  }
                  vals <- tryCatch({
                    sc_key <- sc_find_seurat_key(engine$project, NULL)
                    val <- engine$project$data[[sc_key]]
                    sobj <- if (!is.character(val) && inherits(val, "Seurat")) val else NULL
                    if (!is.null(sobj) && ig %in% colnames(sobj@meta.data)) {
                      uv <- sort(unique(as.character(sobj@meta.data[[ig]])))
                      uv <- uv[!is.na(uv) & nzchar(uv)]
                      if (length(uv) > 0) setNames(uv, uv) else c("无数据" = "")
                    } else c("无数据" = "")
                  }, error = function(e) c("加载失败" = ""))
                  updateCheckboxGroupInput(session, "target_group",
                    choiceNames = names(vals), choiceValues = unname(vals),
                    selected = character(0))
                }

                observeEvent(input[["group_by"]], {
                  gb <- input[["group_by"]]
                  if (is.null(gb) || length(gb) == 0) {
                    updateSelectInput(session, "ident_group",
                      choices = c("请先选择分组列" = ""))
                    update_tg("")
                  } else {
                    updateSelectInput(session, "ident_group",
                      choices = setNames(gb, gb), selected = gb[[1]])
                    update_tg(gb[[1]])
                  }
                }, ignoreNULL = FALSE, ignoreInit = FALSE)
              }

            } else {
              do_run_tool(local_tid, list())
            }
          }, ignoreInit = TRUE)

          # 取消 / 确认 modal
          if (length(local_schema) > 0) {
            observeEvent(input[[paste0("cancel_", local_tid)]], {
              removeModal()
            }, ignoreInit = TRUE)
            observeEvent(input[[paste0("confirm_", local_tid)]], {
              if (processing()) return()
              inputs <- list()
              for (pid in names(local_schema)) {
                pdef <- local_schema[[pid]]
                val  <- input[[pid]]
                if (!is.null(pdef$type) && pdef$type == "file") {
                  if (is.list(val) && !is.null(val$datapath)) val <- val$datapath
                  else next
                }
                if (is.null(val) || is.list(val)) next
                if (is.character(val) && !any(nzchar(val))) next
                inputs[[pid]] <- val
              }
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
      if (identical(current_tool(), "sc_annotate")) {
        return(div(class = "text-center p-5",
          h4("注释工具已启动"), p("请在弹窗中操作。若弹窗关闭可刷新。"))
        )
      }
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

      if (is_multi_plot(tool)) {
        # 多图：每张图一个 tab
        plot_tabs <- lapply(names(tool$outputs$plot), function(pname) {
          tabPanel(pname,
            div(style = "width:100%; max-width:960px; margin:0 auto; aspect-ratio:16/9;",
              plotOutput(ns(paste0("result_plot_", pname)), width = "100%", height = "100%")
            )
          )
        })
        if (has_table) {
          plot_tabs <- c(plot_tabs,
            list(tabPanel("表格", tagList(
              save_table_btn, DT::dataTableOutput(ns("result_table"))
            )))
          )
        }
        do.call(tabsetPanel, c(plot_tabs, list(id = ns("plot_tabs"))))
      } else if (has_plot && !has_table) {
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

    # 多图工具：返回命名列表。单图工具：返回单个对象。
    is_multi_plot <- function(tool) {
      is.list(tool$outputs$plot) && !is.null(names(tool$outputs$plot))
    }
    get_plot_names <- function(tool) {
      if (is_multi_plot(tool)) names(tool$outputs$plot) else "plot"
    }

    # 多图工具：取该工具的所有结果对象，传给各 renderer 自行挑选
    get_multi_plot_objs <- function(tid) {
      all_names <- names(engine$project$results)
      prefix <- paste0(tid, "_")
      matching <- all_names[startsWith(all_names, prefix)]
      setNames(lapply(matching, engine$project$get_result), matching)
    }

    # ── 细胞注释交互状态 ────────────────────────────────────────────────────
    annotate_history <- reactiveVal(list())  # 注释记录 list(cluster = cell_type)

    show_annotation_modal <- function() {
      sobj <- engine$project$data[["_annotate_seurat"]]
      if (is.null(sobj)) return()
      if (is.character(sobj)) sobj <- readRDS(sobj)

      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"
      clusters <- tryCatch({
        cl <- sort(unique(as.character(sobj@meta.data[[cluster_col]])))
        cl <- cl[!is.na(cl) & nzchar(cl)]
        if (length(cl) == 0) c("0") else cl
      }, error = function(e) c("0"))
      reduction <- engine$project$meta$sc_annotate_reduction %||% "umap"

      annotate_history(list())

      showModal(modalDialog(
        title = "细胞类群注释",
        size = "xl",
        easyClose = FALSE,
        fluidRow(
          # === 左栏：参数 + 注释 ===
          column(3,
            selectInput(ns("annot_cluster"), "选择 Cluster", choices = clusters),
            textInput(ns("annot_celltype"), "细胞类型名称",
                      placeholder = "如: CD4+ T cell"),
            actionButton(ns("annot_confirm"), "✓ 确认注释", class = "btn-primary", width = "100%"),
            hr(),
            actionButton(ns("annot_undo"), "↩ 撤销上次", class = "btn-outline-secondary btn-sm", width = "100%"),
            hr(),
            h6("注释记录:"),
            uiOutput(ns("annot_history_ui"))
          ),
          # === 中栏：DimPlot ===
          column(5,
            plotOutput(ns("annot_dimplot"), height = "500px",
                       click = ns("annot_dimplot_click"))
          ),
          # === 右栏：基因表达 ===
          column(4,
            textInput(ns("annot_gene"), "基因名", placeholder = "输入基因名后回车"),
            plotOutput(ns("annot_geneplot"), height = "400px")
          )
        ),
        footer = tagList(
          actionButton(ns("annot_done"), "完成注释", class = "btn-success"),
          actionButton(ns("annot_cancel"), "取消", class = "btn-secondary")
        )
      ))
    }

    # annotation 的 Seurat 对象（触发基因表达图更新）
    annot_sobj <- reactive({
      annotate_history()
      sobj <- engine$project$data[["_annotate_seurat"]]
      if (is.null(sobj)) return(NULL)
      if (is.character(sobj)) readRDS(sobj) else sobj
    })

    # 显示 DimPlot
    output$annot_dimplot <- renderPlot({
      sobj <- annot_sobj()
      req(sobj)
      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"
      reduction   <- engine$project$meta$sc_annotate_reduction %||% "umap"

      hist <- annotate_history()
      lbl_data <- sobj@meta.data
      display_col <- "cell_type"
      if (length(hist) > 0) {
        for (h in hist) {
          lbl_data[[display_col]][lbl_data[[cluster_col]] == h$cluster] <- h$cell_type
        }
        sobj$cell_type_display <- lbl_data[[display_col]]
        display_col <- "cell_type_display"
      }

      DimPlot(sobj, reduction = reduction, group.by = display_col,
              label = TRUE, repel = TRUE)
    })

    # DimPlot 点击选 cluster
    observeEvent(input$annot_dimplot_click, {
      sobj <- annot_sobj()
      req(sobj)
      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"
      reduction   <- engine$project$meta$sc_annotate_reduction %||% "umap"

      click <- input$annot_dimplot_click
      embedding <- Embeddings(sobj, reduction = reduction)
      near <- which.min(colSums((t(embedding) - c(click$x, click$y))^2))
      if (length(near) > 0) {
        cl <- as.character(sobj@meta.data[[cluster_col]][near])
        updateSelectInput(session, "annot_cluster", selected = cl)
      }
    })

    # 基因表达图（防抖，避免逐字触发）
    annot_gene_debounced <- reactive(input$annot_gene) |> debounce(800)

    observeEvent(annot_gene_debounced(), {
      gene <- annot_gene_debounced()
      if (is.null(gene) || !nzchar(gene)) return()
      sobj <- annot_sobj()
      req(sobj)

      if (!gene %in% rownames(sobj)) {
        showNotification(paste("基因", gene, "不存在"), type = "warning")
        return()
      }

      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"
      output$annot_geneplot <- renderPlot({
        tryCatch(
          Seurat::DotPlot(sobj, features = gene, group.by = cluster_col,
                          cols = c("#9E9AC8", "#3F007D"), dot.scale = 8) +
            RotatedAxis() + coord_flip() +
            theme(panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)),
          error = function(e) {
            Seurat::FeaturePlot(sobj, features = gene, reduction = "umap")
          }
        )
      })
    })

    # 确认注释
    observeEvent(input$annot_confirm, {
      sobj <- annot_sobj()
      req(sobj)
      cluster  <- input$annot_cluster
      celltype <- input$annot_celltype
      req(cluster, nzchar(celltype))

      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"

      # 调用 seqTools 注释
      tryCatch({
        seqTools::quick_manual_annotation(sobj, cluster = cluster, cell_type = celltype,
                                          cluster_col = cluster_col)
      }, error = function(e) {
        # seqTools 可能没这个函数，手动设置 meta.data
        sobj$cell_type[sobj@meta.data[[cluster_col]] == cluster] <- celltype
      })

      engine$project$data[["_annotate_seurat"]] <- sobj

      # 更新历史
      hist <- annotate_history()
      hist <- hist[!sapply(hist, function(h) h$cluster == cluster)]
      annotate_history(c(hist, list(list(cluster = cluster, cell_type = celltype))))

      updateTextInput(session, "annot_celltype", value = "")
      showNotification(paste("已注释 cluster", cluster, "→", celltype), type = "message")
    })

    # 撤销
    observeEvent(input$annot_undo, {
      hist <- annotate_history()
      if (length(hist) == 0) {
        showNotification("没有可撤销的注释", type = "warning"); return()
      }
      last <- hist[[length(hist)]]
      annotate_history(hist[-length(hist)])

      sobj <- annot_sobj()
      cluster_col <- engine$project$meta$sc_annotate_cluster_col %||% "seurat_clusters"
      sobj$cell_type[sobj@meta.data[[cluster_col]] == last$cluster] <- last$cluster
      engine$project$data[["_annotate_seurat"]] <- sobj

      showNotification(paste("已撤销 cluster", last$cluster, "的注释"), type = "message")
    })

    # 历史 UI
    output$annot_history_ui <- renderUI({
      hist <- annotate_history()
      if (length(hist) == 0) return(p("暂无", style = "color:grey;"))
      tags <- lapply(hist, function(h) {
        tags$div(style = "font-size:12px; padding:2px 0;",
          tags$b(h$cluster), " → ", h$cell_type)
      })
      do.call(tagList, tags)
    })

    # 完成
    observeEvent(input$annot_done, {
      sobj <- annot_sobj()
      if (!is.null(sobj)) {
        engine$project$data[["clustered_seurat"]] <- sobj
        engine$project$meta$sc_annotated <- TRUE
      }
      engine$project$data[["_annotate_seurat"]] <- NULL
      annotate_history(list())
      removeModal()
      current_tool(NULL)
      showNotification("注释完成，已保存", type = "message")
    })

    observeEvent(input$annot_cancel, {
      engine$project$data[["_annotate_seurat"]] <- NULL
      annotate_history(list())
      removeModal()
      current_tool(NULL)
    })

    # 注释工具完成后自动弹窗
    observeEvent(current_tool(), {
      if (identical(current_tool(), "sc_annotate")) {
        show_annotation_modal()
      }
    }, ignoreNULL = TRUE)

    output$result_plot <- renderPlot({
      req(current_tool())
      tool <- ToolRegistry$get(current_tool())
      req(tool$outputs$plot)
      req(!is_multi_plot(tool))
      obj <- get_render_obj(current_tool(), for_plot = TRUE); req(obj)
      tool$outputs$plot(obj)
    }, res = 72) %>% bindCache(current_tool(), tool_run_count())

    # 多图渲染
    observeEvent(current_tool(), {
      tid <- current_tool()
      req(tid)
      tool <- ToolRegistry$get(tid)
      req(tool, is_multi_plot(tool))
      obj_list <- get_multi_plot_objs(tid)
      for (pname in names(tool$outputs$plot)) {
        local({
          local_name <- pname
          out_id <- paste0("result_plot_", local_name)
          output[[out_id]] <- renderPlot({
            tool$outputs$plot[[local_name]](obj_list)
          })
        })
      }
    }, ignoreNULL = TRUE)

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
