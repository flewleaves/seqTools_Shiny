# utils/ai.R
# 适配新架构：直接操作全局 engine$project (R6)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------- 创建项目工具（对接 ToolRegistry / ImportRegistry） ----------
create_project_tools <- function(on_tool_call = NULL) {
  list(
    # ---- 工具1：列出所有可用分析工具 ----
    aisdk::tool(
      name = "list_tools",
      description = "列出所有可用的分析工具及其分类",
      parameters = aisdk::z_object(dummy = aisdk::z_string(default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("list_tools", args)
        info <- ToolRegistry$list_tools(omics_type = engine$project$omics_type)
        names(info) <- sapply(info, function(x) x$name)
        list(tools = info, categories = unique(sapply(info, function(x) x$category)))
      }
    ),

    # ---- 工具2：列出所有导入方法 ----
    aisdk::tool(
      name = "list_import_methods",
      description = "列出所有可用的数据导入方法",
      parameters = aisdk::z_object(dummy = aisdk::z_string(default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("list_import_methods", args)
        info <- ImportRegistry$list_methods()
        names(info) <- sapply(info, function(x) x$id)
        list(methods = info)
      }
    ),

    # ---- 工具3：获取导入方法的参数定义 ----
    aisdk::tool(
      name = "get_import_method_params",
      description = "获取某个导入方法的参数定义和默认值",
      parameters = aisdk::z_object(method_id = aisdk::z_string()),
      execute = function(args) {
        id <- args$method_id
        method <- ImportRegistry$get(id)
        if (is.null(method)) {
          return(list(error = paste("无此导入方法。可用:", paste(names(ImportRegistry$methods), collapse = ", "))))
        }
        schema <- method$schema
        params_info <- lapply(names(schema), function(pid) {
          p <- schema[[pid]]
          list(type = p$type, default = p$default %||% NULL, choices = p$choices %||% NULL,
               required = p$required %||% FALSE, description = p$description %||% pid)
        })
        names(params_info) <- names(schema)
        list(method_id = id, name = method$name, params = params_info,
             required_params = names(schema)[sapply(schema, function(x) isTRUE(x$required))])
      }
    ),

    # ---- 工具4：获取分析工具的参数定义 ----
    aisdk::tool(
      name = "get_tool_params",
      description = "获取某个分析工具的参数定义和默认值",
      parameters = aisdk::z_object(tool_id = aisdk::z_string()),
      execute = function(args) {
        id <- args$tool_id
        tool <- ToolRegistry$get(id)
        if (is.null(tool)) {
          return(list(error = paste("无此工具。可用:", paste(names(ToolRegistry$tools), collapse = ", "))))
        }
        schema <- tool$schema
        params_info <- lapply(names(schema), function(pid) {
          p <- schema[[pid]]
          list(type = p$type, default = p$default %||% NULL, choices = p$choices %||% NULL,
               required = p$required %||% FALSE, description = p$description %||% pid,
               min = p$min %||% NULL, max = p$max %||% NULL)
        })
        names(params_info) <- names(schema)
        output_type <- if (!is.null(tool$outputs$plot) && !is.null(tool$outputs$table)) "both"
                       else if (!is.null(tool$outputs$plot)) "plot"
                       else if (!is.null(tool$outputs$table)) "table"
                       else "none"
        list(tool_id = id, name = tool$name, category = tool$category,
             description = paste("分类:", tool$category, "| 输出:", output_type),
             params = params_info,
             required_params = names(schema)[sapply(schema, function(x) isTRUE(x$required))])
      }
    ),

    # ---- 工具5：获取项目状态 ----
    aisdk::tool(
      name = "get_state_summary",
      description = "获取当前项目状态摘要",
      parameters = aisdk::z_object(dummy = aisdk::z_string(default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("get_state_summary", args)
        st <- engine$project$to_list()
        list(
          project = st$name,
          omics = st$omics_type,
          data = st$data,
          meta = st$meta,
          results = st$results,
          available_tools = names(ToolRegistry$tools),
          available_import_methods = names(ImportRegistry$methods)
        )
      }
    ),

    # ---- 工具6：执行分析工具 ----
    aisdk::tool(
      name = "run_tool",
      description = "执行指定的分析工具，结果自动同步到 engine$project$results",
      parameters = aisdk::z_object(
        tool_id = aisdk::z_string(description = "工具ID"),
        inputs_json = aisdk::z_string(description = "JSON格式的参数对象", default = "{}")
      ),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("run_tool", args)
        id <- args$tool_id
        inputs_raw <- tryCatch(jsonlite::fromJSON(args$inputs_json %||% "{}", simplifyVector = TRUE), error = function(e) list())
        if (is.character(inputs_raw) && length(inputs_raw) == 1) {
          inputs_raw <- tryCatch(jsonlite::fromJSON(inputs_raw, simplifyVector = TRUE), error = function(e) list())
        }
        if (!id %in% names(ToolRegistry$tools)) {
          return(list(error = paste("无此工具:", id)))
        }
        tryCatch({
          result <- engine$run(id, inputs_raw)
          if (result$success) {
            tool <- ToolRegistry$get(id)
            list(success = TRUE, tool = id,
                 message = paste("工具", id, "执行完成"),
                 project_state = engine$project$to_list(),
                 has_plot = !is.null(tool$outputs$plot),
                 has_table = !is.null(tool$outputs$table))
          } else {
            list(success = FALSE, error = result$error %||% "未知错误")
          }
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e),
               hint = "请确认：1) 数据已导入 2) 元信息完整 3) 参数正确")
        })
      }
    ),

    # ---- 工具7：获取分析结果 ----
    aisdk::tool(
      name = "get_analysis_result",
      description = "获取已完成的分析结果表格或摘要",
      parameters = aisdk::z_object(
        result_name = aisdk::z_string(description = "结果名"),
        preview_n = aisdk::z_integer(description = "预览行数", default = 10)
      ),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("get_analysis_result", args)
        name <- args$result_name
        n <- args$preview_n %||% 10
        obj <- engine$project$get_result(name)
        if (is.null(obj)) obj <- engine$project$data[[name]]
        if (is.null(obj)) {
          return(list(error = paste("无此结果。可用 results:", paste(names(engine$project$results), collapse = ", "),
                                    "| 可用 data:", paste(names(engine$project$data), collapse = ", "))))
        }
        if (is.data.frame(obj)) {
          return(list(type = "data.frame", rows = nrow(obj), cols = ncol(obj),
                      colnames = colnames(obj), preview = head(obj, n)))
        } else if (inherits(obj, "gseaResult")) {
          return(list(type = "gseaResult", rows = nrow(obj@result),
                      preview = head(obj@result, n)))
        } else if (inherits(obj, "ggplot")) {
          return(list(type = "ggplot", components = names(obj)))
        } else {
          return(list(type = class(obj), summary = capture.output(str(obj))))
        }
      }
    ),

    # ---- 工具8：导入数据 ----
    aisdk::tool(
      name = "import_data",
      description = "导入组学数据到项目中。直接调用此工具，不要通过 run_tool。",
      parameters = aisdk::z_object(
        method_id = aisdk::z_string(description = "导入方法ID，如 'bulk_rna'"),
        inputs_json = aisdk::z_string(description = "JSON格式的参数对象", default = "{}"),
        omics_type = aisdk::z_string(description = "组学类型标识", default = "bulk_rna")
      ),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("import_data", args)
        id <- args$method_id
        omics <- args$omics_type %||% "bulk_rna"
        inputs_raw <- tryCatch(jsonlite::fromJSON(args$inputs_json %||% "{}", simplifyVector = TRUE), error = function(e) list())
        if (is.character(inputs_raw) && length(inputs_raw) == 1) {
          inputs_raw <- tryCatch(jsonlite::fromJSON(inputs_raw, simplifyVector = TRUE), error = function(e) list())
        }
        if (!id %in% names(ImportRegistry$methods)) {
          return(list(error = paste("无此导入方法:", id)))
        }
        tryCatch({
          file_path <- inputs_raw$file_path %||% inputs_raw$file %||% NULL
          if (is.null(file_path)) {
            return(list(error = "缺少 file_path 参数。请提供文件的绝对路径"))
          }
          params <- inputs_raw
          params$file_path <- NULL
          engine$project$import_data(id, file_path, params)
          st <- engine$project$to_list()
          list(success = TRUE, method = id, omics_type = engine$project$omics_type,
               data_summary = st$data,
               message = paste("导入完成:", id, "| 数据:", paste(names(engine$project$data), collapse = ", ")))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e),
               hint = "请确认：1) 文件路径正确且可读 2) 格式符合要求 3) 分组信息正确")
        })
      }
    ),

    # ---- 工具9：执行自定义 R 代码（只读） ----
    aisdk::tool(
      name = "run_r_code",
      description = "执行任意 R 代码（只读查询）。禁止修改 project 状态",
      parameters = aisdk::z_object(code = aisdk::z_string(description = "R 代码。只能读取，不能修改")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("run_r_code", args)
        env <- new.env(parent = globalenv())
        env$project <- engine$project
        env$data <- engine$project$data
        env$meta <- engine$project$meta
        env$results <- engine$project$results
        env$tools <- ToolRegistry$tools
        env$import_methods <- ImportRegistry$methods
        tryCatch({
          result <- eval(parse(text = args$code), envir = env)
          list(success = TRUE, class = class(result), summary = capture.output(print(result)))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e))
        })
      }
    ),

    # ---- 工具10：热加载工具 ----
    aisdk::tool(
      name = "reload_tools",
      description = "热加载 R/tools/ 目录下的新工具文件",
      parameters = aisdk::z_object(path = aisdk::z_string(description = "工具目录路径", default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("reload_tools", args)
        tryCatch({
          # 如果路径为空，使用默认路径
          path <- args$path
          if (is.null(path) || path == "") {
            path <- file.path(APP_ROOT, "R", "tools")  # ← 修复：使用 APP_ROOT
          }
          reload_tools(path)
          list(success = TRUE, tools = names(ToolRegistry$tools),
               message = paste("已加载", length(ToolRegistry$tools), "个工具"))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e))
        })
      }
    )
  )
}

# ---------- 带 Tool 的 AI 调用（同步执行，操作 engine$project） ----------
call_ai_with_tools <- function(prompt, config, on_tool_call = NULL) {

  # 临时屏蔽 UI 函数
  has_orig_showNotification <- exists("showNotification", envir = .GlobalEnv)
  if (!has_orig_showNotification) {
    assign("showNotification", function(...) invisible(NULL), envir = .GlobalEnv)
  }
  has_orig_shinyalert <- exists("shinyalert", envir = .GlobalEnv)
  if (!has_orig_shinyalert && requireNamespace("shinyalert", quietly = TRUE)) {
    assign("shinyalert", function(...) invisible(NULL), envir = .GlobalEnv)
  }
  on.exit({
    if (!has_orig_showNotification) rm("showNotification", envir = .GlobalEnv)
    if (!has_orig_shinyalert) rm("shinyalert", envir = .GlobalEnv)
  })

  provider <- config$ai$provider
  key <- config$ai$key
  model_id <- config$ai$model
  base_url <- config$ai$base_url

  if (is.null(key) || key == "") stop("API Key 未设置")

  # 兼容性处理：aisdk 包版本差异
  model <- tryCatch({
    switch(provider,
      "openai" = {
        if (!is.null(base_url) && base_url != "") {
          openai$language_model(model_id, api_key = key, base_url = base_url)
        } else {
          openai$language_model(model_id, api_key = key)
        }
      },
      "deepseek" = {
        if (exists("create_deepseek", mode = "function")) {
          create_deepseek(api_key = key)$language_model(model_id)
        } else {
          openai$language_model(model_id, api_key = key, base_url = "https://api.deepseek.com")
        }
      },
      "aliyun" = {
        if (exists("create_aliyun", mode = "function")) {
          create_aliyun(api_key = key)$language_model(model_id)
        } else {
          openai$language_model(model_id, api_key = key, base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1")
        }
      },
      "custom" = openai$language_model(model_id, api_key = key, base_url = base_url),
      stop("不支持的提供商: ", provider)
    )
  }, error = function(e) {
    if (provider == "deepseek") {
      openai$language_model(model_id, api_key = key, base_url = "https://api.deepseek.com")
    } else if (provider == "aliyun") {
      openai$language_model(model_id, api_key = key, base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1")
    } else {
      stop(e)
    }
  })

  tools <- create_project_tools(on_tool_call = on_tool_call)

  system_prompt <- paste(
    "你是生物信息学分析助手。",
    "",
    "可用工具（直接调用，不是 run_tool 的参数）：",
    "• import_data(method_id, inputs_json, omics_type) — 数据导入",
    "• run_tool(tool_id, inputs_json) — 分析执行",
    "• get_state_summary() — 查看状态",
    "• get_tool_params(tool_id) — 查分析工具参数",
    "• get_import_method_params(method_id) — 查导入方法参数",
    "• list_tools() / list_import_methods() — 列可用项",
    "• get_analysis_result(result_name) — 取结果",
    "• run_r_code(code) — 只读查询（修改不生效）",
    "• reload_tools(path) — 热加载新工具",
    "",
    "工作流（严格顺序）：",
    "1. 导入 → import_data（不要预读文件，直接传路径）",
    "2. 分析 → get_state_summary → get_tool_params → run_tool",
    "3. 查看 → get_analysis_result",
    "",
    "铁律：",
    "× 禁止直接修改 project（run_r_code 只读，import_data/run_tool 是唯二写入方式）",
    "× import_data 失败即停，禁止用 run_r_code 反复试探",
    "× 单任务 ≤5 个 tool call，超则建议用户分步",
    "× 不暴露绝对路径",
    "× run_tool 失败后，禁止用 run_r_code 侦查原因。直接报告错误给用户。",
    "× get_tool_params 已返回完整参数定义，禁止再用 run_r_code 验证。",
    "",
    "回复格式：",
    "• 结果概括（1-2句）→ 查看位置（📊分析/📁数据/💾项目）→ 可选表格预览",
    sep = "
"
  )

  result <- aisdk::generate_text(
    model = model,
    prompt = prompt,
    system = system_prompt,
    tools = tools,
    max_steps = 25,
    temperature = 0.2
  )

  if (!is.null(result$finish_reason) && result$finish_reason == "max_steps") {
    return(paste0(
      result$text,
      "

---
[系统提示] AI 思考步数已达上限，分析可能未完成。",
      "请简化问题，或分步执行。"
    ))
  }

  result$text
}