# utils/ai.R
# 适配新架构：直接操作全局 engine$project (R6)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------- 创建项目工具（对接 ToolRegistry / ImportRegistry / SkillRegistry） ----------
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
        wd <- tryCatch(get("APP_WORK_DIR", envir = .GlobalEnv), error = function(e) getwd())
        # 列出工作目录下可导入的文件
        ws_files <- if (dir.exists(wd)) list.files(wd, pattern = "\\.(rds|h5|txt|csv|tsv|gz|mtx)$",
                      ignore.case = TRUE, recursive = TRUE) else character(0)
        list(
          project = st$name,
          omics = st$omics_type,
          data = st$data,
          meta = st$meta,
          results = st$results,
          work_dir = wd,
          workspace_files = head(ws_files, 20),
          available_tools = names(ToolRegistry$tools),
          available_skills = names(SkillRegistry$skills),
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
      description = "导入组学数据到项目中。single_cell 默认 action=import，传入 file=<目录路径> 即可。",
      parameters = aisdk::z_object(
        method_id = aisdk::z_string(description = "导入方法ID，如 'single_cell' 或 'bulk_rna'"),
        inputs_json = aisdk::z_string(description = "JSON格式的参数。single_cell 必填: file(目录绝对路径), species。可选: action(默认import)", default = "{}"),
        omics_type = aisdk::z_string(description = "组学类型标识，single_cell 或 bulk_rna", default = "single_cell")
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
            return(list(error = "缺少 file 参数。请提供数据目录的绝对路径（single_cell 扫描目录，bulk_rna 指向文件）"))
          }
          if (!dir.exists(file_path) && !file.exists(file_path)) {
            return(list(error = paste("路径不存在:", file_path)))
          }
          params <- inputs_raw
          params$file_path <- NULL
          # single_cell: 默认 action = "import"
          if (id == "single_cell" && is.null(params$action)) {
            params$action <- "import"
          }
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
      description = "在沙箱中执行 R 代码。只能读写程序目录和工作目录中的文件",
      parameters = aisdk::z_object(code = aisdk::z_string(description = "R 代码。文件读写受沙箱限制")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("run_r_code", args)
        env <- new.env(parent = globalenv())
        env$project <- engine$project
        env$data <- engine$project$data
        env$meta <- engine$project$meta
        env$results <- engine$project$results
        env$tools <- ToolRegistry$tools
        env$import_methods <- ImportRegistry$methods

        # 初始化文件沙箱
        wd <- get("APP_WORK_DIR", envir = .GlobalEnv) %||% getwd()
        app_root <- get("APP_ROOT", envir = .GlobalEnv) %||% getwd()
        sys.source(file.path(app_root, "R", "utils", "sandbox.R"), envir = env)
        evalq(
          sandbox_init(
            allowed_read  = c(app_root, wd),
            allowed_write = c(wd)
          ),
          envir = env
        )

        tryCatch({
          result <- eval(parse(text = args$code), envir = env)
          list(success = TRUE, class = class(result), summary = capture.output(print(result)))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e))
        })
      }
    ),

    # ---- 工具10：热加载工具和技能 ----
    aisdk::tool(
      name = "reload_tools",
      description = "热加载 R/tools/ 和 R/skills/ 目录下的新文件",
      parameters = aisdk::z_object(path = aisdk::z_string(description = "工具目录路径", default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("reload_tools", args)
        tryCatch({
          path <- args$path
          if (is.null(path) || path == "") {
            path <- file.path(APP_ROOT, "R", "tools")
          }
          reload_tools(path)
          list(success = TRUE, tools = names(ToolRegistry$tools),
               skills = names(SkillRegistry$skills),
               message = paste("已加载", length(ToolRegistry$tools), "个工具,", length(SkillRegistry$skills), "个技能"))
        }, error = function(e) {
          list(success = FALSE, error = conditionMessage(e))
        })
      }
    ),

    # ---- 工具11：列出所有技能 ----
    aisdk::tool(
      name = "list_skills",
      description = "列出所有可用的分析技能（多步骤自动化工作流）",
      parameters = aisdk::z_object(dummy = aisdk::z_string(default = "")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("list_skills", args)
        skills <- SkillRegistry$list_skills(omics_type = engine$project$omics_type)
        list(skills = skills, count = length(skills),
             hint = "使用 get_skill_info 查看某技能的详细步骤，使用 run_skill 执行")
      }
    ),

    # ---- 工具12：获取技能详情 ----
    aisdk::tool(
      name = "get_skill_info",
      description = "获取某个技能的完整定义，包括所有步骤和可覆盖的参数。调用 run_skill 前先用此工具了解技能结构。",
      parameters = aisdk::z_object(skill_id = aisdk::z_string(description = "技能ID")),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("get_skill_info", args)
        id <- args$skill_id
        skill <- SkillRegistry$get(id)
        if (is.null(skill)) {
          return(list(error = paste("无此技能。可用:", paste(names(SkillRegistry$skills), collapse = ", "))))
        }
        list(
          skill_id      = skill$name,
          display_name  = skill$display_name %||% skill$name,
          description   = skill$description %||% "",
          version       = skill$version %||% "1.0.0",
          omics         = skill$omics %||% NULL,
          steps         = lapply(skill$steps, function(st) {
            list(id = st$id, tool = st$tool, params = st$params, description = st$description %||% st$tool)
          }),
          usage = paste0(
            "执行: run_skill(\"", id, "\", overrides_json)。",
            "overrides_json 格式: {\"step_id\": {\"param\": value}}，",
            "用于覆盖特定步骤的参数（如动态的 contrast、deg_input 等）"
          )
        )
      }
    ),

    # ---- 工具13：执行技能 ----
    aisdk::tool(
      name = "run_skill",
      description = "执行一个技能（多步骤分析工作流）。优先使用技能而非逐个调用工具——技能更可靠、步骤更少、不易出错。覆盖参数: overrides_json={\"step_id\":{\"param\":value}}",
      parameters = aisdk::z_object(
        skill_id = aisdk::z_string(description = "技能ID"),
        overrides_json = aisdk::z_string(description = "JSON格式的参数覆盖，格式: {\"step_id\": {\"param\": value}}。用于传入动态参数如contrast、deg_input", default = "{}")
      ),
      execute = function(args) {
        if (!is.null(on_tool_call)) on_tool_call("run_skill", args)
        id <- args$skill_id
        overrides_raw <- tryCatch(jsonlite::fromJSON(args$overrides_json %||% "{}", simplifyVector = TRUE), error = function(e) list())
        if (is.character(overrides_raw) && length(overrides_raw) == 1) {
          overrides_raw <- tryCatch(jsonlite::fromJSON(overrides_raw, simplifyVector = TRUE), error = function(e) list())
        }

        skill <- SkillRegistry$get(id)
        if (is.null(skill)) {
          return(list(error = paste("无此技能:", id, "。可用:", paste(names(SkillRegistry$skills), collapse = ", "))))
        }

        result <- SkillRegistry$execute(id, overrides_raw, engine$project)

        if (result$success) {
          list(
            success = TRUE, skill = id,
            steps_done = result$steps_done,
            message = paste0("技能 ", id, " 执行完成 (", length(result$steps_done), "步)"),
            project_state = engine$project$to_list()
          )
        } else {
          list(
            success = FALSE, skill = id,
            failed_step = tail(names(result$step_results), 1),
            error = result$error,
            completed_steps = result$steps_done,
            hint = "技能执行中断。已完成步骤的结果已保存，请检查失败步骤的参数是否正确。"
          )
        }
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
    "【技能 Skills】优先使用：",
    "当用户请求匹配已有技能时，必须优先调用 run_skill 而非逐个调用工具。",
    "技能是多步骤自动化流程，更可靠、更省步数。",
    "• list_skills() — 列出所有可用技能",
    "• get_skill_info(skill_id) — 查看技能步骤和参数（调用 run_skill 前必看）",
    "• run_skill(skill_id, overrides_json) — 执行技能（优先使用！）",
    "  overrides_json 用于覆盖动态参数: {\"step_id\": {\"param\": value}}",
    "",
    "【完成即停 — 不要回查】",
    "• run_tool/run_skill 成功后直接告诉用户结果已在分析页，禁止调 get_analysis_result。",
    "",
    "【过滤 — AI 也能做】",
    "• 用户可在「数据」页手动 QC 过滤（看小提琴图调阈值）",
    "• AI 也可调 run_tool('sc_preprocessing', '{\"doublet_find\":true,\"filter\":null}')",
    "  filter=NULL=自动, filter=[低UMI,高UMI,低Feature,高Feature,MT]=手动",
    "",
    "【工具 Tools】",
    "• get_state_summary() / import_data / run_skill / run_tool / get_tool_params / get_analysis_result",
    "• run_r_code — 仅数据探索，不能导入或分析",
    "",
    "【错误处理】",
    "× 同一操作连续失败 2 次 → 停止，报告用户，询问下一步。禁止换 run_r_code 绕过。",
    "",
    "【新功能】",
    "× 用户要的功能没对应工具 → 说明需要新建 R/tools/ 下的工具文件，参照现有格式生成代码。",
    "",
    "铁律：",
    "× 不暴露绝对路径。单任务 ≤5 个 tool call。失败直接报告。",
    "",
    "回复格式：",
    "• 结果概括（1-2句）→ 查看位置（分析/数据/项目）→ 可选表格预览",
    sep = "\n"
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
      "\n\n---\n[系统提示] AI 思考步数已达上限，分析可能未完成。",
      "请简化问题，或分步执行。"
    ))
  }

  result$text
}
