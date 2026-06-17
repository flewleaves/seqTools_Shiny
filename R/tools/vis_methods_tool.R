# R/tools/vis_methods_tool.R
# 查看分析方法 — 显示分析流程中所有用到的方法（底层函数 + 版本 + 参数）

TOOL_NAME         <- "vis_methods"
TOOL_CATEGORY     <- "数据查看"
TOOL_ORDER        <- 95
TOOL_DISPLAY_NAME <- "查看方法"

TOOL_SCHEMA <- list()

TOOL_RUN <- function(inputs, project) {
  methods <- project$results$`_methods`
  if (is.null(methods) || length(methods) == 0) {
    df <- data.frame(提示 = "暂无分析方法记录，请先运行分析工具",
                     stringsAsFactors = FALSE, check.names = FALSE)
    return(list(data = list(table = df), messages = "无记录"))
  }

  times <- sapply(methods, function(m) m$time %||% "")
  methods <- methods[order(times)]

  df <- data.frame(
    序号   = seq_along(methods),
    步骤   = sapply(methods, function(m) m$tool_name %||% m$tool %||% ""),
    底层函数 = sapply(methods, function(m) m$method %||% ""),
    包     = sapply(methods, function(m) paste0(m$package %||% "", " (v", m$version %||% "?", ")")),
    参数   = sapply(methods, function(m) {
      p <- m$params
      if (is.null(p) || length(p) == 0) return("")
      paste(names(p), "=", sapply(p, function(v) {
        if (is.list(v)) return("[list]")
        if (length(v) > 3) return(paste0("[", length(v), " elements]"))
        paste(as.character(v), collapse = ", ")
      }), collapse = "; ")
    }),
    时间   = sapply(methods, function(m) m$time %||% ""),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(df) <- NULL

  list(
    data     = list(table = df),
    messages = paste("共记录", nrow(df), "个分析步骤")
  )
}

TOOL_OUTPUTS <- list(
  plot  = NULL,
  table = function(result) result
)
