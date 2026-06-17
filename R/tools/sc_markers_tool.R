# R/tools/sc_markers_tool.R
# 查找每个cluster的标记基因

TOOL_NAME <- "sc_markers"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "标记基因"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 20

TOOL_SCHEMA <- list(
  n = list(type = "integer", default = 10, min = 2, max = 50,
           description = "每个cluster的top标记基因数"),
  high_expr = list(type = "boolean", default = TRUE,
    description = "仅保留高表达标记基因（筛选 logFC > 0.25 且在目标组表达比例 > 0.25 的基因）"),
  sctransform = list(type = "boolean", default = FALSE,
                     description = "是否使用SCTransform数据"),
  draw = list(type = "boolean", default = TRUE,
              description = "是否生成DotPlot图")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  res <- engine_sc_markers(
    seurat_obj = seurat_obj,
    n = inputs$n,
    draw = inputs$draw,
    high_expr = inputs$high_expr,
    sctransform = inputs$sctransform
  )

  out_data <- list(markers = res$markers)
  if (!is.null(res$plot)) out_data$dotplot <- res$plot

  list(
    data = out_data,
    messages = res$note,
    method_info = record_method("sc_markers", "标记基因",
                                "Seurat::FindAllMarkers", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$dotplot },
  table = function(result) { if (is.data.frame(result)) result else result$data$markers }
)
