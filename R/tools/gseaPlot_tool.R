# R/tools/gseaPlot_tool.R
TOOL_NAME         <- "gseaPlot"
TOOL_CATEGORY     <- "数据绘图"
TOOL_OMICS        <- "bulk_rna"
TOOL_ORDER        <- 30
TOOL_DISPLAY_NAME <- "GSEA通路图"

TOOL_SCHEMA <- list(
  id_gsea  = list(type = "text",    required = TRUE,                      description = "目标通路名称，多个用分号隔开"),
  np_gsea  = list(type = "integer", default = 3, min = 0, max = 3,        description = "显示几张分图（0=仅富集曲线）"),
  addp_gsea= list(type = "boolean", default = TRUE,                        description = "是否添加 p 值标签"),
  x_gsea   = list(type = "number",  default = 0.95,                        description = "p 值标签 X 坐标"),
  y_gsea   = list(type = "number",  default = 0.8,                         description = "p 值标签 Y 坐标")
)

TOOL_RUN <- function(inputs, project) {
  gsea_obj <- project$results[["gsea_obj"]]   # ← 与 gsea_tool 对齐
  if (is.null(gsea_obj)) stop("请先进行 GSEA 分析（运行 gsea 工具）")

  geneSetIDs <- trimws(strsplit(inputs$id_gsea, ";")[[1]])
  if (length(geneSetIDs) == 0 || all(geneSetIDs == "")) stop("请至少提供一个通路名称")

  res <- engine_gsea_plot(
    gsea_res   = gsea_obj,
    geneSetIDs = geneSetIDs,
    subPlot    = inputs$np_gsea,
    addPval    = inputs$addp_gsea,
    pvalX      = inputs$x_gsea,
    pvalY      = inputs$y_gsea
  )

  list(data = list(plot = res$plot), messages = res$note,
       method_info = record_method("gseaPlot", "GSEA通路图", "GseaVis::gseaNb", "GseaVis", inputs))
}

TOOL_OUTPUTS <- list(
  plot  = function(result) if (inherits(result, "ggplot")) result else NULL,
  table = NULL
)
