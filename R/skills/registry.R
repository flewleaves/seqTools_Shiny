# R/skills/registry.R
# Skill = JSON 配置文件，编排多个 tool 的执行顺序
library(R6)

SkillRegistry <- R6Class("SkillRegistry",
  public = list(
    skills = list(),

    register = function(skill_def) {
      id <- skill_def$name
      if (is.null(id) || id == "") stop("Skill 缺少 name 字段")
      self$skills[[id]] <- skill_def
      invisible(self)
    },

    get = function(id) {
      self$skills[[id]]
    },

    list_skills = function(omics_type = NULL) {
      result <- unname(lapply(self$skills, function(s) {
        list(
          id           = s$name,
          display_name = s$display_name %||% s$name,
          description  = s$description %||% "",
          category     = s$category %||% "pipeline",
          version      = s$version %||% "1.0.0",
          author       = s$author %||% "",
          omics        = s$omics %||% NULL,
          step_count   = length(s$steps),
          steps        = lapply(s$steps, function(st) {
            list(id = st$id, tool = st$tool, description = st$description %||% st$tool)
          })
        )
      }))

      if (!is.null(omics_type)) {
        result <- Filter(function(s) {
          is.null(s$omics) || omics_type %in% s$omics
        }, result)
      }

      result
    },

    execute = function(skill_id, overrides = list(), project = NULL) {
      skill <- self$get(skill_id)
      if (is.null(skill)) stop("Skill 不存在: ", skill_id)

      project <- project %||% engine$project
      step_results <- list()
      all_success <- TRUE
      error_info <- NULL

      for (step in skill$steps) {
        step_id <- step$id
        tool_id <- step$tool

        # 合并默认参数 + AI 覆盖参数
        params <- step$params %||% list()
        if (!is.null(overrides[[step_id]])) {
          params <- modifyList(params, overrides[[step_id]])
        }

        result <- tryCatch(
          engine$run(tool_id, params),
          error = function(e) list(success = FALSE, error = conditionMessage(e))
        )

        step_results[[step_id]] <- list(
          tool   = tool_id,
          success = isTRUE(result$success),
          error   = result$error %||% NULL,
          message = result$message %||% result$result$messages %||% ""
        )

        if (!isTRUE(result$success)) {
          all_success <- FALSE
          error_info <- result$error %||% "未知错误"
          break
        }
      }

      list(
        success      = all_success,
        skill        = skill_id,
        steps_done   = names(step_results),
        step_results = step_results,
        error        = error_info,
        project_state = if (!is.null(project)) project$to_list() else list()
      )
    },

    load_directory = function(path) {
      if (!dir.exists(path)) {
        message("[SkillRegistry] 目录不存在: ", path)
        return(invisible(character(0)))
      }

      files <- list.files(path, pattern = "\\.json$", full.names = TRUE, ignore.case = TRUE)
      loaded <- character(0)

      for (f in files) {
        tryCatch({
          skill_def <- jsonlite::read_json(f, simplifyVector = FALSE)
          # 兼容 name/display_name 中英文字段
          if (is.null(skill_def$name)) skill_def$name <- gsub("\\.json$", "", basename(f), ignore.case = TRUE)
          self$register(skill_def)
          loaded <- c(loaded, skill_def$name)
        }, error = function(e) {
          message("[SkillRegistry] 加载失败: ", basename(f), " - ", conditionMessage(e))
        })
      }

      message("[SkillRegistry] 已加载 ", length(loaded), " 个 skill: ", paste(loaded, collapse = ", "))
      invisible(loaded)
    },

    reload = function(path) {
      self$skills <- list()
      self$load_directory(path)
    }
  )
)

SkillRegistry <- SkillRegistry$new()
