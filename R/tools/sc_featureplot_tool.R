# R/tools/sc_featureplot_tool.R
# 单细胞 FeaturePlot — Seurat::FeaturePlot

TOOL_NAME <- "sc_featureplot"
TOOL_CATEGORY <- "数据绘图"
TOOL_DISPLAY_NAME <- "FeaturePlot (特征图)"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 36

TOOL_SCHEMA <- list(
  genes = list(type = "text", required = TRUE,
    description = "基因名，英文逗号分隔。例: Slc3a2,Slc7a5。大小写自动纠正。"),
  split_by = list(type = "select", choices = NULL, required = FALSE,
    description = "分面列名。留空或选第一项=不分面。例: orig.ident"),
  reduction = list(type = "select",
    choices = c("umap","tsne","pca"), default = "umap",
    description = "降维方法"),
  pt_size = list(type = "number", required = FALSE,
    description = "点大小。留空使用 Seurat 默认。建议 0.5-2")
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

  args <- list(object = seurat_obj, features = gene_vec,
               reduction = if (!is.null(inputs$reduction) && nzchar(inputs$reduction))
                           inputs$reduction else "umap")
  if (!is.null(inputs$split_by) && nzchar(inputs$split_by) && inputs$split_by != "All_Cells")
    args$split.by <- inputs$split_by
  if (!is.null(inputs$pt_size) && is.numeric(inputs$pt_size) && inputs$pt_size > 0)
    args$pt.size <- inputs$pt_size

  p <- do.call(Seurat::FeaturePlot, args)

  list(data = list(plot = p),
       messages = paste("FeaturePlot 完成:", paste(gene_vec, collapse=", ")),
       method_info = record_method("sc_featureplot", "FeaturePlot (特征图)",
         "Seurat::FeaturePlot", "Seurat", inputs))
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$plot },
  table = NULL
)
