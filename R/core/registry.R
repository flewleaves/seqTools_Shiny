# R/core/registry.R
library(R6)

ToolRegistry <- R6Class("ToolRegistry",
  public = list(
    tools = list(),

    register = function(tool_def) {
      self$tools[[tool_def$name]] <- tool_def
    },

    get = function(name) {
      self$tools[[name]]
    },

    load_directory = function(path) {
      files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
      loaded <- character(0)
      for (f in files) {
        env <- new.env()
        tryCatch({
          source(f, local = env)
          if (all(c("TOOL_NAME", "TOOL_SCHEMA", "TOOL_RUN") %in% ls(env))) {
            self$register(list(
              name         = env$TOOL_NAME,
              display_name = env$TOOL_DISPLAY_NAME %||% env$TOOL_NAME,
              category     = env$TOOL_CATEGORY %||% "未分类",
              schema       = env$TOOL_SCHEMA,
              run          = env$TOOL_RUN,
              outputs      = env$TOOL_OUTPUTS %||% list(),
              omics        = env$TOOL_OMICS %||% NULL,
              order        = env$TOOL_ORDER %||% 99
            ))
            loaded <- c(loaded, env$TOOL_NAME)
          }
        }, error = function(e) {
          message("[ToolRegistry] 加载失败: ", basename(f), " - ", conditionMessage(e))
        })
      }
      message("[ToolRegistry] 已加载 ", length(loaded), " 个工具: ", paste(loaded, collapse = ", "))
      invisible(loaded)
    },

    reload = function(path) {
      # 热加载：只清除用户分析工具，保留 import/system（由 engine 注册）
      keep_cats <- c("import", "system")
      self$tools <- Filter(function(t) t$category %in% keep_cats, self$tools)
      self$load_directory(path)
      # 联动重载 skills
      if (exists("SkillRegistry")) {
        skills_path <- file.path(dirname(path), "skills")
        if (dir.exists(skills_path)) SkillRegistry$reload(skills_path)
      }
    },

    # ui_only = TRUE  → 只返回用户可见的分析工具（排除 import/system）
    # ui_only = FALSE → 返回全部（AI 调用）
    list_tools = function(omics_type = NULL, ui_only = FALSE) {
      tools <- lapply(names(self$tools), function(n) {
        t <- self$tools[[n]]
        list(
          id           = n,
          name         = n,
          display_name = t$display_name %||% n,
          category     = t$category,
          schema       = t$schema,
          has_plot     = !is.null(t$outputs$plot),
          has_table    = !is.null(t$outputs$table),
          omics        = t$omics,
          order        = t$order %||% 99
        )
      })

      # 按 order 排序
      tools <- tools[order(sapply(tools, `[[`, "order"))]

      # UI 模式：过滤掉 import 和 system 类工具
      if (ui_only) {
        excluded <- c("import", "system")
        tools <- Filter(function(t) !t$category %in% excluded, tools)
      }

      # 按 omics 过滤
      if (!is.null(omics_type)) {
        tools <- Filter(function(t) {
          is.null(t$omics) || omics_type %in% t$omics
        }, tools)
      }

      tools
    }
  )
)

ToolRegistry <- ToolRegistry$new()
