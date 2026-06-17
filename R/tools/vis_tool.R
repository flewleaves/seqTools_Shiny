# R/tools/vis_tool.R
TOOL_NAME         <- "vis"
TOOL_CATEGORY     <- "数据查看"
TOOL_ORDER        <- 90
TOOL_DISPLAY_NAME <- "结果查看"

# choices 留空，UI 层通过 schema_to_ui + state 动态填充
TOOL_SCHEMA <- list(
  vis = list(
    type        = "select",
    choices     = NULL,       # NULL = 动态从 state 填充
    source      = "results",  # 标记来源，schema_to_ui 识别此字段
    required    = TRUE,
    description = "选择要查看的结果或数据对象"
  )
)

TOOL_RUN <- function(inputs, project) {
  key <- inputs$vis
  obj <- project$results[[key]] %||% project$data[[key]]
  if (is.null(obj)) stop("对象不存在: ", key)

  list(
    data     = list(obj = obj, key = key),   # key 用于渲染时回查
    messages = paste("查看:", key, "| 类型:", paste(class(obj), collapse = "/"))
  )
}

TOOL_OUTPUTS <- list(
  plot = function(result) {
    if (is.null(result)) return(NULL)
    if (inherits(result, "ggplot")) return(result)
    if (inherits(result, "gseaResult")) {
      tryCatch({
        require(GseaVis)
        GseaVis::dotplotGsea(data = result, topn = 10, pajust = 0.25)$plot
      }, error = function(e) NULL)
    } else NULL
  },
  table = function(result) {
    if (is.null(result))                                                  return(NULL)
    if (is.data.frame(result))                                            return(result)
    if (is.matrix(result))                                                return(as.data.frame(result))
    if (inherits(result, c("dgCMatrix","Matrix")))                        return(as.data.frame(as.matrix(result)))
    if (inherits(result, "gseaResult"))                                   return(result@result)
    if (inherits(result, "ggplot") && is.data.frame(result$data))           return(result$data)
    # Seurat 对象提取 meta.data
    if (inherits(result, "Seurat")) {
      md <- tryCatch(result@meta.data, error = function(e) NULL)
      if (!is.null(md) && is.data.frame(md)) return(md)
    }
    # 兜底：尝试强制转换
    tryCatch(as.data.frame(result), error = function(e) NULL)
  }
)
