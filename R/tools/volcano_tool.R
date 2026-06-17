# R/tools/volcano_tool.R
TOOL_NAME         <- "volcano"
TOOL_CATEGORY     <- "数据绘图"
TOOL_OMICS        <- "bulk_rna"
TOOL_ORDER        <- 40
TOOL_DISPLAY_NAME <- "火山图"

TOOL_SCHEMA <- list(
  vol_p        = list(type = "number",  default = 0.05, min = 0, max = 1, description = "Adjust p 值阈值"),
  vol_lfc      = list(type = "number",  default = 1,    min = 0,           description = "logFC 阈值"),
  vol_color    = list(type = "string",  default = "#6a82ed;grey;#ed6f6f",  description = "下调/ns/上调颜色,分号隔开"),
  vol_al       = list(type = "number",  default = 0.3,  min = 0, max = 1,  description = "透明度"),
  vol_size     = list(type = "number",  default = 2,    min = 0,           description = "点的大小"),
  vol_labels   = list(type = "text",    default = "",                       description = "标记基因名，分号隔开（可选）"),
  vol_highlight= list(type = "text",    default = "",                       description = "高亮基因名，分号隔开（可选）"),
  vol_hcolor   = list(type = "string",  default = "#07a818",               description = "高亮颜色"),
  vol_maxo     = list(type = "integer", default = 10,   min = 1,           description = "最大标记重叠数")
)

TOOL_RUN <- function(inputs, project) {
  deg_res <- project$results[["deg_result"]]   # ← 与 deg_tool 对齐
  if (is.null(deg_res)) stop("请先进行差异分析（运行 deg 工具）")

  colors    <- strsplit(inputs$vol_color, ";")[[1]]
  labels    <- if (nzchar(inputs$vol_labels))    strsplit(inputs$vol_labels,    ";")[[1]] else NULL
  highlight <- if (nzchar(inputs$vol_highlight)) strsplit(inputs$vol_highlight, ";")[[1]] else NULL

  species <- project$meta$species %||% "Hs"
  if (!is.null(labels)) {
    labels    <- if (species == "Mm") stringr::str_to_title(labels)    else toupper(labels)
    highlight <- if (!is.null(highlight)) {
      if (species == "Mm") stringr::str_to_title(highlight) else toupper(highlight)
    } else NULL
  }

  res <- engine_volcano(
    deg_res         = deg_res,
    padj.val        = inputs$vol_p,
    logFC           = inputs$vol_lfc,
    color           = colors,
    alpha           = inputs$vol_al,
    size            = inputs$vol_size,
    label           = labels,
    highlight       = highlight,
    highlight_color = inputs$vol_hcolor,
    max.overlaps    = inputs$vol_maxo
  )

  list(data = list(plot = res$plot), messages = res$note,
       method_info = record_method("volcano", "火山图", "ggplot2", "ggplot2", inputs))
}

TOOL_OUTPUTS <- list(
  plot  = function(result) if (inherits(result, "ggplot")) result else NULL,
  table = NULL
)
