# R/tools/sc_dotplot_tool.R
# 单细胞 DotPlot — Seurat::DotPlot

TOOL_NAME <- "sc_dotplot"
TOOL_CATEGORY <- "数据绘图"
TOOL_DISPLAY_NAME <- "DotPlot (点图)"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 34

TOOL_SCHEMA <- list(
  genes = list(type = "text", required = TRUE,
    description = "基因名，英文逗号分隔。例: Slc3a2,Slc7a5。大小写自动纠正。"),
  group_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分组列名。下拉框第一项=所有细胞归一组。也可选 seurat_clusters/cell_type"),
  split_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分面列名。留空或选第一项=不分面。例: orig.ident"),
  dot_cols = list(type = "text", default = "#9E9AC8,#3F007D", required = FALSE,
    description = "颜色渐变，英文逗号分隔两个十六进制色号，如 #9E9AC8,#3F007D"),
  col_min = list(type = "number", required = FALSE,
    description = "颜色轴最小值。留空=Seurat 自动确定。例: 0 或 -1"),
  dot_scale = list(type = "number", default = 6, min = 1, max = 30,
    description = "点大小缩放 (越大点越大)"),
  flip = list(type = "boolean", default = TRUE,
    description = "翻转坐标轴 (RotatedAxis + coord_flip)")
)

TOOL_RUN <- function(inputs, project) {
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  gene_vec <- trimws(strsplit(inputs$genes, ",")[[1]])
  gene_vec <- gene_vec[nzchar(gene_vec)]
  if (length(gene_vec) == 0) stop("未提供有效的基因名")

  res <- sc_match_genes(gene_vec, seurat_obj)
  if (!is.null(res$msg)) message(res$msg)
  gene_vec <- res$genes

  # 解析颜色
  dot_cols <- if (!is.null(inputs$dot_cols) && nzchar(inputs$dot_cols)) {
    trimws(strsplit(inputs$dot_cols, ",")[[1]])
  } else c("#9E9AC8", "#3F007D")

  args <- list(object = seurat_obj, features = gene_vec,
               cols = dot_cols,
               dot.scale = inputs$dot_scale %||% 6)
  # col.min：用户留空时不给 Seurat 传值，让 Seurat 自动确定范围
  if (!is.null(inputs$col_min) && !is.na(inputs$col_min))
    args$col.min <- inputs$col_min

  # group_by："All_Cells" 列由 sc_load_from_disk 保证始终存在
  group_by <- if (!is.null(inputs$group_by) && nzchar(inputs$group_by))
    inputs$group_by else "All_Cells"
  args$group.by <- group_by

  if (!is.null(inputs$split_by) && nzchar(inputs$split_by) && inputs$split_by != "All_Cells")
    args$split.by <- inputs$split_by

  p <- do.call(Seurat::DotPlot, args)
  if (isTRUE(inputs$flip)) p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::theme(
    panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA)
  )

  list(data = list(plot = p),
       messages = paste("DotPlot 完成:", paste(gene_vec, collapse=", ")),
       method_info = record_method("sc_dotplot", "DotPlot (点图)",
         "Seurat::DotPlot", "Seurat", inputs))
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$plot },
  table = NULL
)
