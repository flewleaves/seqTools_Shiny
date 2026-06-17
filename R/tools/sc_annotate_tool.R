# R/tools/sc_annotate_tool.R
# 细胞类群手动注释 — quick_manual_annotation 交互式弹窗

TOOL_NAME <- "sc_annotate"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "细胞注释"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 28

TOOL_SCHEMA <- list(
  cluster_col = list(type = "select", choices = NULL, default = "seurat_clusters",
    description = "聚类列名。下拉选择 meta.data 中的分类列。默认 seurat_clusters，也可选 cell_type 等"),
  reduction = list(type = "select",
    choices = c("umap","tsne","pca"), default = "umap",
    description = "降维方法")
)

TOOL_RUN <- function(inputs, project) {
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  cluster_col <- inputs$cluster_col %||% "seurat_clusters"
  if (!cluster_col %in% colnames(seurat_obj@meta.data))
    stop("meta.data 中不存在列: ", cluster_col)

  # 确保 Idents 对
  Idents(seurat_obj) <- cluster_col
  if (!"cell_type" %in% colnames(seurat_obj@meta.data))
    seurat_obj$cell_type <- as.character(Idents(seurat_obj))

  # 存引用
  project$data[["_annotate_seurat"]] <- seurat_obj
  project$meta$sc_annotate_cluster_col <- cluster_col

  clusters <- sort(unique(seurat_obj@meta.data[[cluster_col]]))
  existing <- setNames(seurat_obj$cell_type, seurat_obj@meta.data[[cluster_col]])
  existing <- existing[!duplicated(names(existing))]

  list(
    data = list(
      clusters = as.character(clusters),
      existing_annotations = as.list(existing),
      reduction = inputs$reduction %||% "umap",
      cluster_col = cluster_col
    ),
    messages = "注释工具已启动，请在弹窗中操作",
    method_info = record_method("sc_annotate", "细胞注释",
      "Seurat::AddMetaData (手动赋值 cell_type)", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(plot = NULL, table = NULL)
