# utils/ai.R

# ==================== 原有 call_ai 保留 ====================

call_ai <- function(prompt, config) {
  provider <- config$ai$provider
  key <- config$ai$key
  model_id <- config$ai$model
  base_url <- config$ai$base_url
  
  if (is.null(key) || key == "") stop("API Key 未设置")
  
  model <- switch(provider,
    "openai" = {
      if (!is.null(base_url) && base_url != "") {
        openai$language_model(model_id, api_key = key, base_url = base_url)
      } else {
        openai$language_model(model_id, api_key = key)
      }
    },
    "deepseek" = create_deepseek(api_key = key)$language_model(model_id),
    "aliyun"   = create_aliyun(api_key = key)$language_model(model_id),
    "custom"   = openai$language_model(model_id, api_key = key, base_url = base_url),
    stop("不支持的提供商: ", provider)
  )
  
  generate_text(model, prompt)$text
}

# ==================== 新增：对接 tools 注册表 + import_methods 的 AI 工具 ====================

#' 创建 mock session（供 tool$run 使用）
create_mock_session <- function() {
  s <- list(
    sendCustomMessage = function(...) NULL,
    sendInputMessage = function(...) NULL,
    sendInsertUI = function(...) NULL,
    sendRemoveUI = function(...) NULL,
    registerDataObj = function(...) NULL,
    ns = function(x) x
  )
  # 让 shiny 的 update*Input 函数认为这是一个有效 session
  class(s) <- c("ShinySession", "R6")
  s
}

#' 创建项目工具（直接对接你的 tools list 和 import_methods）
#' @param state reactiveValues
#' @param tools_list 你的分析工具注册表（全局变量 tools）
#' @param import_methods_list 你的导入方法注册表（全局变量 import_methods）
create_project_tools <- function(state, tools_list, import_methods_list) {
  list(
    # ---- 工具1：列出所有可用分析工具 ----
    aisdk::tool(
      name = "list_tools",
      description = "列出所有可用的分析工具及其分类",
      parameters = aisdk::z_object(
        dummy = aisdk::z_string(description = "占位参数", default = "")
      ),
      execute = function(args) {
        info <- lapply(names(tools_list), function(id) {
          t <- tools_list[[id]]
          list(
            id = id,
            name = t$name,
            category = t$category,
            omics = t$omics %||% "all",
            output_type = t$output_type
          )
        })
        names(info) <- names(tools_list)
        list(tools = info, categories = unique(sapply(tools_list, `[[`, "category")))
      }
    ),
    
    # ---- 工具2：列出所有导入方法（data_list.R）----
    aisdk::tool(
      name = "list_import_methods",
      description = "列出所有可用的数据导入方法（如 RNA-seq、单细胞等）",
      parameters = aisdk::z_object(
        dummy = aisdk::z_string(description = "占位参数", default = "")
      ),
      execute = function(args) {
        info <- lapply(names(import_methods_list), function(id) {
          m <- import_methods_list[[id]]
          list(
            id = id,
            name = m$name,
            description = paste("导入方法:", m$name)
          )
        })
        names(info) <- names(import_methods_list)
        list(methods = info)
      }
    ),
    
    # ---- 工具3：获取导入方法的参数定义 ----
    aisdk::tool(
      name = "get_import_method_params",
      description = "获取某个导入方法的参数定义（如需要上传什么文件、选择什么选项）",
      parameters = aisdk::z_object(
        method_id = aisdk::z_string(description = "导入方法ID，如 'bulk_rna', 'single_cell'")
      ),
      execute = function(args) {
        id <- args$method_id
        if (!id %in% names(import_methods_list)) {
          return(list(error = paste("无此导入方法。可用:", paste(names(import_methods_list), collapse = ", "))))
        }
        
        m <- import_methods_list[[id]]
        
        # 解析 UI 定义（简化版，提取输入控件信息）
        dummy_ns <- function(id) id
        ui_def <- m$ui(dummy_ns)
        
        list(
          method_id = id,
          name = m$name,
          has_validate = !is.null(m$validate),
          has_preview = !is.null(m$preview_data),
          ui_components = length(ui_def$children)
        )
      }
    ),
    
    # ---- 工具4：获取项目状态（isolate 包裹所有 state 访问）----
    aisdk::tool(
      name = "get_state_summary",
      description = "获取当前项目状态摘要：有哪些数据、结果、元信息、可用工具",
      parameters = aisdk::z_object(
        dummy = aisdk::z_string(description = "占位参数", default = "")
      ),
      execute = function(args) {
        # 关键：用 isolate() 读取 reactiveValues，避免 "outside of reactive consumer" 错误
        list(
          project = shiny::isolate(state$name) %||% "未命名",
          omics = shiny::isolate(state$omics_type) %||% "未设置",
          data = names(shiny::isolate(state$data)),
          meta = names(shiny::isolate(state$meta)),
          results = names(shiny::isolate(state$res)),
          available_tools = names(tools_list),
          available_import_methods = names(import_methods_list)
        )
      }
    ),
    
    # ---- 工具5：获取分析工具参数定义 ----
    aisdk::tool(
      name = "get_tool_params",
      description = "获取某个分析工具的参数定义和默认值，AI 执行前必须先调用此工具确认参数",
      parameters = aisdk::z_object(
        tool_id = aisdk::z_string(description = "工具ID，如 'deg', 'pca', 'volcano', 'gsea'")
      ),
      execute = function(args) {
        id <- args$tool_id
        if (!id %in% names(tools_list)) {
          return(list(error = paste("无此工具。可用:", paste(names(tools_list), collapse = ", "))))
        }
        
        t <- tools_list[[id]]
        
        # 用 isolate 读取 state，然后传给 params()
        current_state <- shiny::isolate(reactiveValuesToList(state))
        dummy_ns <- function(id) id
        param_def <- t$params(dummy_ns, current_state)
        
        ui_list <- param_def$ui
        ids <- param_def$ids
        
        params_info <- list()
        for (pid in ids) {
          ctrl <- NULL
          for (el in ui_list$children) {
            if (!is.null(el$attribs$id) && el$attribs$id == pid) {
              ctrl <- el
              break
            }
          }
          
          params_info[[pid]] <- list(
            type = if (!is.null(ctrl)) ctrl$name else "unknown",
            default = if (!is.null(ctrl$attribs$value)) ctrl$attribs$value else NULL,
            choices = if (!is.null(ctrl$attribs$choices)) as.list(ctrl$attribs$choices) else NULL
          )
        }
        
        list(
          tool_id = id,
          name = t$name,
          description = paste("分类:", t$category, "| 输出:", t$output_type),
          params = params_info,
          required_params = ids
        )
      }
    ),
    
    # ---- 工具6：执行分析工具（核心：用 isolate 读写 state，结果同步回 state）----
    aisdk::tool(
      name = "run_tool",
      description = "执行指定的分析工具，结果自动同步到 state$res。必须先调用 get_tool_params 确认参数",
      parameters = aisdk::z_object(
        tool_id = aisdk::z_string(description = "工具ID"),
        inputs_json = aisdk::z_string(description = "JSON格式的参数对象", default = "{}")
      ),
      execute = function(args) {
        id <- args$tool_id
        inputs_raw <- jsonlite::fromJSON(args$inputs_json %||% "{}")
        
        if (!id %in% names(tools_list)) {
          return(list(error = paste("无此工具:", id)))
        }
        
        t <- tools_list[[id]]
        
        # 构建 inputs
        dummy_ns <- function(x) x
        current_state <- shiny::isolate(reactiveValuesToList(state))
        param_def <- t$params(dummy_ns, current_state)
        all_ids <- param_def$ids
        
        inputs <- list()
        for (pid in all_ids) {
          if (pid %in% names(inputs_raw)) {
            inputs[[pid]] <- inputs_raw[[pid]]
          } else {
            inputs[[pid]] <- NULL
          }
        }
        
        # 执行 run 函数
        tryCatch({
          mock_session <- create_mock_session()
          mock_ns <- function(x) x
          
          # 关键：直接调用 t$run，传入真正的 state（不是副本）
          # 用 isolate 包裹，允许在 non-reactive 上下文修改 reactiveValues
          result <- shiny::isolate({
            t$run(state, inputs, mock_session, mock_ns)
          })
          
          # 检查 state$res 是否更新
          new_results <- names(shiny::isolate(state$res))
          
          list(
            success = TRUE,
            tool = id,
            message = paste("工具", t$name, "执行完成"),
            results_in_state = new_results,
            has_plot = !is.null(t$plot),
            has_table = !is.null(t$table)
          )
          
        }, error = function(e) {
          list(
            success = FALSE,
            error = conditionMessage(e),
            hint = "请确认：1) 数据已导入 2) 元信息完整 3) 参数正确。某些工具可能需要先执行其他工具（如先 deg 再 volcano）"
          )
        })
      }
    ),
    
    # ---- 工具7：获取分析结果 ----
    aisdk::tool(
      name = "get_analysis_result",
      description = "获取已完成的分析结果表格或摘要",
      parameters = aisdk::z_object(
        result_name = aisdk::z_string(description = "结果名，如 'deg_res', 'pca_res', 'gsea_res'"),
        preview_n = aisdk::z_integer(description = "预览行数", default = 10)
      ),
      execute = function(args) {
        name <- args$result_name
        n <- args$preview_n %||% 10
        
        res_all <- shiny::isolate(state$res)
        if (!name %in% names(res_all)) {
          return(list(error = paste("无此结果。可用:", paste(names(res_all), collapse = ", "))))
        }
        
        obj <- res_all[[name]]
        
        if (is.data.frame(obj)) {
          return(list(
            type = "data.frame",
            rows = nrow(obj),
            cols = ncol(obj),
            colnames = colnames(obj),
            preview = head(obj, n)
          ))
        } else if (is.list(obj) && !is.null(obj$plot)) {
          return(list(type = "plot_result", components = names(obj)))
        } else {
          return(list(type = class(obj), summary = capture.output(str(obj))))
        }
      }
    ),
    
    # ---- 工具8：执行自定义 R 代码 ----
    aisdk::tool(
      name = "run_r_code",
      description = "执行任意 R 代码，环境中有 state$data, state$meta, state$res 及所有全局函数",
      parameters = aisdk::z_object(
        code = aisdk::z_string(description = "R 代码")
      ),
      execute = function(args) {
        env <- new.env(parent = globalenv())
        env$state <- shiny::isolate(reactiveValuesToList(state))
        env$data <- shiny::isolate(state$data)
        env$meta <- shiny::isolate(state$meta)
        env$res <- shiny::isolate(state$res)
        env$tools <- tools_list
        env$import_methods <- import_methods_list
        
        tryCatch({
          result <- eval(parse(text = args$code), envir = env)
          list(success = TRUE, class = class(result), summary = capture.output(print(result)))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e))
        })
      }
    )
  )
}

#' 带 Tool 的 AI 调用（对接 tools 注册表 + import_methods）
call_ai_with_tools <- function(prompt, config, state, tools_list, import_methods_list) {
  provider <- config$ai$provider
  key <- config$ai$key
  model_id <- config$ai$model
  base_url <- config$ai$base_url
  
  if (is.null(key) || key == "") stop("API Key 未设置")
  
  model <- switch(provider,
    "openai" = {
      if (!is.null(base_url) && base_url != "") {
        openai$language_model(model_id, api_key = key, base_url = base_url)
      } else {
        openai$language_model(model_id, api_key = key)
      }
    },
    "deepseek" = create_deepseek(api_key = key)$language_model(model_id),
    "aliyun"   = create_aliyun(api_key = key)$language_model(model_id),
    "custom"   = openai$language_model(model_id, api_key = key, base_url = base_url),
    stop("不支持的提供商: ", provider)
  )
  
    tools <- create_project_tools(state, tools_list, import_methods_list)
  
  steps <- character()  # 收集每一步的文本
  
  system_prompt <- paste(
    "你是一个生物信息学分析助手。你可以访问用户的项目数据并执行分析。",
    "",
    "可用工具：",
    "1. list_tools(dummy='') - 列出分析工具（差异分析、PCA、火山图等）",
    "2. list_import_methods(dummy='') - 列出数据导入方法（RNA-seq、单细胞等）",
    "3. get_state_summary(dummy='') - 查看当前项目状态",
    "4. get_tool_params(tool_id='deg') - 获取分析工具的参数定义",
    "5. get_import_method_params(method_id='bulk_rna') - 获取导入方法的参数",
    "6. run_tool(tool_id='deg', inputs_json='{\"method\":\"DESeq2\"}') - 执行分析（结果同步到 state）",
    "7. get_analysis_result(result_name='deg_res', preview_n=10) - 获取结果",
    "8. run_r_code(code='...') - 执行自定义 R 代码",
    "",
    "工作流：",
    "1. 用户要求分析 → get_state_summary 确认数据状态",
    "2. get_tool_params 确认参数 → run_tool 执行 → get_analysis_result 查看结果",
    "3. 导入数据问题 → list_import_methods → get_import_method_params",
    "执行规范（必须遵守）：",
    "1. 开始任何分析前，先输出'🚀 正在启动 XX 分析...'",
    "2. 调用 list_tools / get_tool_params 后，输出'📋 已获取工具参数'",
    "3. 调用 run_tool 后，输出'⚙️ 正在执行...'",
    "4. 调用 get_analysis_result 后，输出'✅ 分析完成，结果如下'",
    "5. 如果接近步数上限，输出'⚠️ 步骤较多，建议分步执行'",
    "【结果展示规范 - 必须遵守】：",
    "1. 分析完成后，先用 1-2 句话概括结果（如上调/下调基因数、关键发现）",
    "2. 必须明确告知用户查看位置：",
    "   - 差异分析/富集分析/绘图结果 → '📊 完整结果请前往【分析】模块查看'",
    "   - 导入的数据矩阵 → '📁 数据已导入，请前往【数据预处理】模块预览和确认'",
    "   - 项目文件 → '💾 项目已保存，请前往【项目管理】模块查看'",
    "3. 如需展示预览，调用 get_analysis_result 获取前10行，用 Markdown 表格展示",
    "4. 绝不声称'没有结果'或'结果未生成'，只要 run_tool 返回 success，结果就在 state$res 中",
    sep = "\n"
  )

  # 流式调用，通过回调追加步骤
  result <- aisdk::generate_text(
    model = model,
    prompt = prompt,
    system = system_prompt,
    tools = tools,
    max_steps = 100,
    temperature = 0.2,
    stream = TRUE,
    on_chunk = function(chunk) {
      # 每个 chunk 可能是文本或工具调用请求
      if (!is.null(chunk$tool_call)) {
        # 记录工具调用
        step_msg <- paste0("🔧 正在调用工具: ", chunk$tool_call$name, "(", 
                          jsonlite::toJSON(chunk$tool_call$arguments), ")")
        steps <<- c(steps, step_msg)
      } else if (!is.null(chunk$text)) {
        steps <<- c(steps, chunk$text)
      }
    }
  )
  
  # 将步骤合并为最终显示
  paste(steps, collapse = "\n\n")
  
  if (!is.null(result$finish_reason) && result$finish_reason == "max_steps") {
    return(paste0(
      result$text,
      "\n\n---\n[系统提示] AI 思考步数已达上限，分析可能未完成。",
      "请简化问题，或分步执行（如先只做差异分析，确认结果后再做下游分析）。"
    ))
  }

  result$text
}