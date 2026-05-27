# R/utils/ui_helpers.R

#' 将工具/导入方法的 schema 转换为 Shiny UI 控件
schema_to_ui <- function(schema, ns, state) {
  if (length(schema) == 0) return(NULL)

  # 在调用时立即快照，避免 modal 里响应式丢失
  # 确保是 character 向量，state 为空时 names() 返回 NULL 会导致 startsWith 报错
  data_keys   <- as.character(names(state$data)   %||% character(0))
  result_keys <- as.character(names(state$res)    %||% character(0))
  vis_result_keys <- if (length(result_keys) > 0)
    result_keys[!startsWith(result_keys, "vis_")]
  else character(0)
  group_info  <- state$meta$group_info

  controls <- lapply(names(schema), function(id) {
    def     <- schema[[id]]
    type    <- def$type %||% "string"
    label   <- def$description %||% id
    choices <- def$choices

    # ---------- 动态填充 choices ----------
    if (type == "select" && (is.null(choices) || length(choices) == 0 ||
                             identical(choices, c("")) || identical(choices, ""))) {
      choices <- switch(id,
        # 数据矩阵选择
        deg_input = ,
        pca_input = ,
        normalize_input = {
          if (length(data_keys) > 0) data_keys else c("暂无数据" = "")
        },

        # 结果查看：排除 vis_ 自身结果，按类型分组显示
        vis = {
          all_keys <- c(
            setNames(data_keys,        paste0("[数据] ", data_keys)),
            setNames(vis_result_keys,  paste0("[结果] ", vis_result_keys))
          )
          if (length(all_keys) > 0) all_keys else c("暂无数据" = "")
        },

        # 对比组
        contrast = {
          if (!is.null(group_info) && length(unique(group_info)) >= 2) {
            groups <- unique(as.character(group_info))
            combos <- combn(groups, 2)
            ch <- c()
            for (k in seq_len(ncol(combos))) {
              a <- combos[1, k]; b <- combos[2, k]
              ch[paste(a, "vs", b)] <- paste(a, b, sep = "-")
              ch[paste(b, "vs", a)] <- paste(b, a, sep = "-")
            }
            ch
          } else {
            c("请先导入含分组信息的数据" = "")
          }
        },

        # 默认：保留原 choices 或空
        if (length(choices) > 0 && !identical(choices, c(""))) choices
        else c("暂无数据" = "")
      )
    }

    # ---------- 生成控件 ----------
    if (type == "select") {
      if (is.null(choices) || length(choices) == 0) choices <- c("暂无数据" = "")
      selectInput(ns(id), label,
                  choices  = choices,
                  selected = def$default %||% choices[[1]])

    } else if (type == "boolean") {
      checkboxInput(ns(id), label,
                    value = as.logical(def$default %||% FALSE))

    } else if (type %in% c("number", "integer")) {
      step <- if (type == "integer") 1 else 0.1
      args <- list(inputId = ns(id), label = label,
                   value = if (type == "integer") as.integer(def$default %||% 0)
                           else as.numeric(def$default %||% 0),
                   step = step)
      if (!is.null(def$min)) args$min <- def$min
      if (!is.null(def$max)) args$max <- def$max
      do.call(numericInput, args)

    } else if (type == "text") {
      textAreaInput(ns(id), label, value = def$default %||% "", rows = 2)

    } else if (type == "file") {
      fileInput(ns(id), label,
                accept = c(".gmt", ".csv", ".txt", ".tsv", ".gz", ".mtx"))

    } else {
      textInput(ns(id), label, value = def$default %||% "")
    }
  })

  do.call(tagList, controls)
}

#' 创建 mock session（用于 AI 工具调用）
create_mock_session <- function() {
  list(
    sendCustomMessage  = function(type, message) invisible(NULL),
    sendInputMessage   = function(inputId, message) invisible(NULL),
    sendNotification   = function(type, message) invisible(NULL),
    onSessionEnded     = function(callback) invisible(NULL),
    ns                 = function(id) id
  )
}