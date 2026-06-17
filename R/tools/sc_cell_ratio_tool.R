# R/tools/sc_cell_ratio_tool.R
# 细胞比例可视化

TOOL_NAME <- "sc_cell_ratio"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "细胞比例"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 22

TOOL_SCHEMA <- list(
  anno_by = list(type = "select", choices = NULL, required = TRUE,
                 description = "注释列名 (meta.data中的列，如 seurat_clusters, cell_type)"),
  group_by = list(type = "select", choices = NULL, required = FALSE,
                  description = "分组列名 (meta.data中的列，如 orig.ident, treatment)。留空则所有细胞一组"),
  y_axis = list(type = "text", default = "细胞比例",
                description = "Y轴标题"),
  width = list(type = "number", default = 0.5, min = 0.1, max = 1,
               description = "柱子宽度"),
  return_table = list(type = "boolean", default = FALSE,
                      description = "是否同时返回数据表")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  group_by <- inputs$group_by
  if (is.null(group_by) || !nzchar(group_by)) group_by <- "orig.ident"

  res <- engine_sc_cell_ratio(
    seurat_obj = seurat_obj,
    anno_by = inputs$anno_by %||% "seurat_clusters",
    group_by = group_by,
    return_table = inputs$return_table
  )

  out_data <- list(ratio_plot = res$plot)
  if (!is.null(res$table)) out_data$ratio_table <- res$table

  list(
    data = out_data,
    messages = res$note,
    method_info = record_method("sc_cell_ratio", "细胞比例",
                                "prop.table + ggplot2::geom_bar", "ggplot2", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$ratio_plot },
  table = function(result) { if (is.data.frame(result)) result else result$data$ratio_table }
)
