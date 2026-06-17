# R/tools/sc_vlnplot_tool.R
# 单细胞 VlnPlot — Seurat::VlnPlot

TOOL_NAME <- "sc_vlnplot"
TOOL_CATEGORY <- "数据绘图"
TOOL_DISPLAY_NAME <- "VlnPlot (小提琴图)"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 32

TOOL_SCHEMA <- list(
  genes = list(type = "text", required = TRUE,
    description = "基因名，英文逗号分隔。例: Slc3a2,Slc7a5（鼠: 首字母大写其余小写；人: 全大写）。大小写自动纠正。"),
  group_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分组列名。下拉框第一项「全部(不分组)」=所有细胞归一组。也可选 seurat_clusters/cell_type 等分类列"),
  split_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分面列名。下拉框第一项=不分面。留空或选第一项效果相同。例: orig.ident"),
  pt_size = list(type = "number", required = FALSE,
    description = "点大小。留空使用 Seurat 默认值（通常 0.5-1），建议 0.5-2")
)

TOOL_RUN <- function(inputs, project) {
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  gene_vec <- trimws(strsplit(inputs$genes, ",")[[1]])
  gene_vec <- gene_vec[nzchar(gene_vec)]
  if (length(gene_vec) == 0) stop("未提供有效的基因名")

  # 验证基因存在于 Seurat 对象中
  res <- sc_match_genes(gene_vec, seurat_obj)
  if (!is.null(res$msg)) message(res$msg)
  gene_vec <- res$genes

  args <- list(object = seurat_obj, features = gene_vec)

  group_by <- if (!is.null(inputs$group_by) && nzchar(inputs$group_by))
    inputs$group_by else "All_Cells"
  args$group.by <- group_by

  if (!is.null(inputs$split_by) && nzchar(inputs$split_by) && inputs$split_by != "All_Cells")
    args$split.by <- inputs$split_by
  if (!is.null(inputs$pt_size) && is.numeric(inputs$pt_size) && inputs$pt_size > 0)
    args$pt.size <- inputs$pt_size

  p <- do.call(Seurat::VlnPlot, args)

  list(data = list(plot = p),
       messages = paste("VlnPlot 完成:", paste(gene_vec, collapse=", ")),
       method_info = record_method("sc_vlnplot", "VlnPlot (小提琴图)",
         "Seurat::VlnPlot", "Seurat", inputs))
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$plot },
  table = NULL
)
