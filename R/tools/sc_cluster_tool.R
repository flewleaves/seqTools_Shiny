# R/tools/sc_cluster_tool.R
# 聚类 + 降维可视化 (UMAP/TSNE)

TOOL_NAME <- "sc_cluster"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "聚类+UMAP"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 18

TOOL_SCHEMA <- list(
  PCs = list(type = "text", required = FALSE,
    description = "PC 数量范围。格式: 起始:结束，如 1:30 表示用前30个PC。留空自动选（覆盖90%方差）"),
  resolution = list(type = "text", default = "0.2,0.4,0.6,0.8,1.0",
                    description = "聚类分辨率，逗号分隔"),
  reduction = list(type = "select",
                   choices = c("pca", "harmony"),
                   default = "pca",
                   description = "降维方法（单样本用pca）"),
  sctransform = list(type = "boolean", default = FALSE,
                     description = "是否使用SCTransform数据"),
  method = list(type = "select",
                choices = c("UMAP", "TSNE"),
                default = "UMAP",
                description = "可视化方法"),
  k_param = list(type = "integer", default = 20, min = 5, max = 100,
                 description = "KNN参数"),
  n_neighbors = list(type = "integer", default = 30, min = 5, max = 100,
                     description = "UMAP邻居数"),
  min_dist = list(type = "number", default = 0.3, min = 0, max = 1,
                  description = "UMAP最小距离"),
  save_to_disk = list(type = "boolean", default = FALSE,
                      description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  res <- engine_sc_cluster(
    seurat_obj = seurat_obj,
    PCs = sc_parse_PCs(inputs$PCs),
    resolution = sc_parse_resolution(inputs$resolution),
    reduction = inputs$reduction %||% "harmony",
    sctransform = inputs$sctransform,
    k_param = inputs$k_param,
    n_neighbors = inputs$n_neighbors,
    min_dist = inputs$min_dist,
    method = inputs$method %||% "UMAP"
  )

  project$data[["clustered_seurat"]] <- res$seurat
  if (inputs$save_to_disk) sc_save_to_disk(project, "clustered_seurat")

  # 保存 UMAP 图为结果
  umap_plot <- DimPlot(res$seurat, reduction = if (inputs$method == "TSNE") "tsne" else "umap",
                       label = TRUE, repel = TRUE) + ggplot2::ggtitle("Clustering")

  list(
    data = list(umap = umap_plot),
    messages = res$note,
    method_info = record_method("sc_cluster", "聚类+UMAP",
                                "Seurat::FindNeighbors → FindClusters → RunUMAP", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$umap },
  table = NULL
)
