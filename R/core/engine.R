# R/core/engine.R
AnalysisEngine <- R6Class("AnalysisEngine",
  public = list(
    project = NULL,

    initialize = function(project = NULL) {
      self$project <- project %||% Project$new()

            # ===== 将 ImportRegistry 的导入方法注册为 ToolRegistry 工具 =====
      if (exists("ImportRegistry") && length(ImportRegistry$methods) > 0) {
        lapply(ImportRegistry$methods, function(method) {
          ToolRegistry$register(list(
            name = method$id,           # ← 用 id 作为 name（唯一标识）
            category = "import",
            schema = method$schema,
            run = function(inputs, project) {
              file_path <- inputs$file
              params <- inputs
              params$file <- NULL
              result <- method$run(file_path, params)
              if (!is.null(result$data)) {
                for (nm in names(result$data)) {
                  project$data[[nm]] <- result$data[[nm]]
                }
              }
              if (!is.null(result$meta)) {
                project$meta <- modifyList(project$meta %||% list(), result$meta)
              }
              if (!is.null(result$omics_type)) {
                project$omics_type <- result$omics_type
              }
              list(success = TRUE, message = paste("导入完成:", method$name))
            },
            outputs = list(data = TRUE, table = FALSE, plot = FALSE)
          ))
        })
      }

      # ===== 注册系统工具：get_state =====
            # ===== 注册系统工具：get_state =====
      ToolRegistry$register(list(
        name = "get_state",              # 机器标识（AI 调用的 function name）
        display_name = "获取项目状态",    # 中文显示名
        category = "system",
        schema = list(),
        run = function(inputs, project) {
          st <- project$to_list()
          wd <- tryCatch(get("APP_WORK_DIR", envir = .GlobalEnv), error = function(e) getwd())
          ws_files <- if (dir.exists(wd)) list.files(wd,
            pattern = "\\.(rds|h5|txt|csv|tsv|gz|mtx)$",
            ignore.case = TRUE, recursive = TRUE) else character(0)

          # 提取 Seurat 对象的摘要信息
          seurat_info <- list()
          for (nm in names(st$data)) {
            d <- st$data[[nm]]
            if (identical(d$type, "Seurat")) {
              seurat_info[[nm]] <- list(
                cells = d$cells,
                genes = d$genes,
                meta_cols = d$meta_cols,
                reductions = d$reductions,
                assays = d$assays,
                cluster_cols = d$cluster_cols,
                active_ident = head(d$active_ident, 30)
              )
            }
          }

          list(
            data = list(
              project = st$name,
              omics = st$omics_type,
              data_keys = names(st$data),
              meta_keys = names(st$meta),
              result_names = names(st$results),
              data_detail = st$data,
              seurat = if (length(seurat_info) > 0) seurat_info else NULL,
              work_dir = wd,
              workspace_files = head(ws_files, 30)
            ),
            meta = list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          )
        },
        outputs = list(data = TRUE, table = FALSE, plot = FALSE)
      ))
    },

    run = function(tool_id, inputs) {
      tool <- ToolRegistry$get(tool_id)
      if (is.null(tool)) stop("工具不存在: ", tool_id)

      clean_inputs <- private$validate_inputs(inputs, tool$schema)
      result <- tool$run(clean_inputs, self$project)

      # ===== 导入/系统工具：不存 results =====
      if (!is.null(tool$category) && tool$category %in% c("import", "system")) {
        return(list(success = TRUE, result = result, project_state = self$project$to_list()))
      }

      # ===== 记录分析方法（底层函数+版本+参数） =====
      if (!is.null(result$method_info)) {
        if (is.null(self$project$results$`_methods`))
          self$project$results$`_methods` <- list()
        # tool_id 为 key，重复运行自动覆盖
        self$project$results$`_methods`[[tool_id]] <- result$method_info
        # 清理 method_info，不混入结果数据
        result$method_info <- NULL
      }

      # ===== 分析工具：原有逻辑 =====
      if (!is.null(result$data)) {
        if (is.list(result$data) && !is.null(names(result$data))) {
          for (nm in names(result$data)) {
            self$project$add_result(paste0(tool_id, "_", nm), result$data[[nm]])
          }
        } else {
          self$project$add_result(paste0(tool_id, "_res"), result$data)
        }
      }

      list(success = TRUE, result = result, project_state = self$project$to_list())
    },

    list_tools = function(ui_only = FALSE) {
      ToolRegistry$list_tools(ui_only = ui_only)
    }
  ),

  private = list(
    validate_inputs = function(inputs, schema) {
      # 辅助：检测无效值（缺失 / NULL / 空字符串 / NA）
      is_blank <- function(v) {
        if (is.null(v)) return(TRUE)
        if (identical(v, "")) return(TRUE)
        (length(v) == 1 && is.na(v) && !is.logical(v))  # NA_real_ / NA_integer_ / NA_character_
      }

      cleaned <- inputs
      for (name in names(schema)) {
        def <- schema[[name]]

        # 应用默认值：参数缺失 或 为无效值时使用默认值
        if ((!name %in% names(cleaned) || is_blank(cleaned[[name]])) && !is.null(def$default)) {
          cleaned[[name]] <- def$default
        }

        if (isTRUE(def$required) && (!name %in% names(cleaned) || is_blank(cleaned[[name]]))) {
          stop("缺少必填参数: ", name)
        }
        if (!name %in% names(cleaned)) next
        val <- cleaned[[name]]

        # 跳过已为无效值的情况（没有默认值的可选参数，留空不传）
        if (is_blank(val)) next

        # file 类型原样保留（路径字符串）
        if (def$type == "file") {
          cleaned[[name]] <- val
          next
        }

        if (def$type == "boolean") {
          cleaned[[name]] <- if (is.character(val)) toupper(val) %in% c("TRUE","1","YES","T") else as.logical(val)
        } else if (def$type == "number") {
          cleaned[[name]] <- as.numeric(val)
        } else if (def$type == "integer") {
          cleaned[[name]] <- as.integer(val)
        } else if (def$type == "select") {
          cleaned[[name]] <- as.character(val)
        }

        if (def$type %in% c("number","integer")) {
          if (!is.null(def$min) && !is.na(cleaned[[name]]) && cleaned[[name]] < def$min)
            stop(name, " 必须 >= ", def$min)
          if (!is.null(def$max) && !is.na(cleaned[[name]]) && cleaned[[name]] > def$max)
            stop(name, " 必须 <= ", def$max)
        }
        # 只在 choices 非空时校验（NULL = 动态填充，跳过）
        if (!is_blank(cleaned[[name]]) &&
            !is.null(def$choices) && length(def$choices) > 0 &&
            !identical(def$choices, c("")) &&
            !cleaned[[name]] %in% def$choices) {
          stop(name, " 必须是: ", paste(def$choices, collapse = ", "))
        }
      }
      cleaned
    }
  )
)