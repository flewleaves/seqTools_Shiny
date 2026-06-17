# R/tools/sc_dimplot_tool.R
# 单细胞 DimPlot — Seurat::DimPlot

TOOL_NAME <- "sc_dimplot"
TOOL_CATEGORY <- "数据绘图"
TOOL_DISPLAY_NAME <- "DimPlot (聚类图)"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 30

TOOL_SCHEMA <- list(
  group_by = list(type = "select", choices = NULL, default = "seurat_clusters",
    description = "分组列名。默认 seurat_clusters。下拉框第一项=所有细胞归一组"),
  split_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分面列名。留空或选第一项=不分面。例: orig.ident"),
  reduction = list(type = "select", required = TRUE,
    choices = c("umap","tsne","pca"), default = "umap",
    description = "降维方法"),
  label = list(type = "boolean", default = TRUE,
    description = "是否显示聚类标签"),
  repel = list(type = "boolean", default = FALSE,
    description = "是否排斥标签重叠"),
  pt_size = list(type = "number", required = FALSE,
    description = "点大小。留空使用 Seurat 默认（约 0.5）。建议 0.5-2")
)

TOOL_RUN <- function(inputs, project) {
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  group_by <- if (!is.null(inputs$group_by) && nzchar(inputs$group_by))
    inputs$group_by else "seurat_clusters"

  reduction <- if (!is.null(inputs$reduction) && nzchar(inputs$reduction))
    inputs$reduction else "umap"

  args <- list(
    object = seurat_obj,
    reduction = reduction,
    group.by = group_by,
    label = isTRUE(inputs$label),
    repel = isTRUE(inputs$repel)
  )

  if (!is.null(inputs$split_by) && nzchar(inputs$split_by) && inputs$split_by != "All_Cells")
    args$split.by <- inputs$split_by

  if (!is.null(inputs$pt_size) && is.numeric(inputs$pt_size) && !is.na(inputs$pt_size))
    args$pt.size <- inputs$pt_size

  p <- do.call(Seurat::DimPlot, args) + ggplot2::ggtitle(paste("DimPlot:", group_by))

  list(data = list(plot = p),
       messages = paste("DimPlot 完成:", group_by,
                        if (!is.null(inputs$split_by) && nzchar(inputs$split_by))
                          paste("(分面:", inputs$split_by, ")")),
       method_info = record_method("sc_dimplot", "DimPlot (聚类图)",
         "Seurat::DimPlot", "Seurat", inputs))
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$plot },
  table = NULL
)
