# R/tools/sc_quick_pipeline_tool.R
# 一键快速单细胞分析 pipeline

TOOL_NAME <- "sc_quick_pipeline"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "一键全流程分析"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 24

TOOL_SCHEMA <- list(
  species = list(type = "select", choices = c("Hs", "Mm"), default = "Hs"),
  sctransform = list(type = "boolean", default = FALSE,
                     description = "使用SCTransform (推荐单批次)"),
  cellcycle_scoring = list(type = "boolean", default = FALSE,
                           description = "回归细胞周期"),
  vars_to_regress = list(type = "text", default = "percent.mt",
                         description = "需要回归的变量，逗号分隔"),
  PCs = list(type = "text", required = FALSE,
    description = "PC 数量范围。格式: 起始:结束，如 1:30。留空自动选（覆盖90%方差）"),
  resolution = list(type = "text", default = "0.2,0.4,0.6,0.8,1.0",
                    description = "聚类分辨率，逗号分隔"),
  save_to_disk = list(type = "boolean", default = FALSE,
                      description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(Seurat)

  if (!sc_has_dataLists(project)) stop("无单细胞数据，请先导入")

  dataLists <- sc_get_dataLists(project)
  species <- inputs$species %||% project$meta$species %||% "Hs"

  res <- engine_sc_quick_pipeline(
    dataLists = dataLists,
    species = species,
    sctransform = inputs$sctransform,
    cellcycle_scoring = inputs$cellcycle_scoring,
    vars_to_regress = inputs$vars_to_regress %||% "percent.mt",
    PCs = sc_parse_PCs(inputs$PCs),
    resolution = sc_parse_resolution(inputs$resolution)
  )

  project$data[["clustered_seurat"]] <- res$seurat
  # 始终缓存到磁盘（保留内存），后续工具无需反复 readRDS
  sc_save_to_disk(project, "clustered_seurat")
  # 清理原始 dataList，释放内存（原始数据已备份到磁盘）
  for (key in sc_list_dataList_keys(project)) {
    sc_save_to_disk(project, key, release = TRUE)
  }

  # 取默认分辨率（中间值）展示 DimPlot
  resolutions <- sc_parse_resolution(inputs$resolution)
  display_res <- resolutions[ceiling(length(resolutions)/2)]
  res_col <- paste0("RNA_snn_res.", display_res)
  if (res_col %in% colnames(res$seurat@meta.data)) {
    Idents(res$seurat) <- res_col
  }

  umap_plot <- DimPlot(res$seurat, reduction = "umap",
                       label = TRUE, repel = TRUE) +
    ggplot2::ggtitle(paste0("Clustering (res=", display_res, ")"))

  # clustree 分辨率选择图
  prefix <- if (inputs$sctransform) "SCT_snn_res." else "RNA_snn_res."
  res_cols <- grep(paste0("^", prefix), colnames(res$seurat@meta.data), value = TRUE)
  clustree_plot <- if (length(res_cols) >= 2) {
    tryCatch(clustree::clustree(res$seurat, prefix = prefix), error = function(e) NULL)
  } else NULL

  list(
    data = list(umap = umap_plot, clustree = clustree_plot),
    messages = paste(res$note,
      "| DimPlot使用分辨率", display_res,
      "| 查看clustree选择合适分辨率后，可用「聚类+UMAP」工具重新运行"),
    method_info = record_method("sc_quick_pipeline", "一键全流程分析",
      "Seurat::LogNormalize → FindVariableFeatures → ScaleData → RunPCA → (Harmony) → FindNeighbors → FindClusters → RunUMAP",
      "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = list(
    umap = function(objs) {
      for (nm in names(objs)) if (grepl("umap$", nm)) return(objs[[nm]]); NULL
    },
    clustree = function(objs) {
      for (nm in names(objs)) if (grepl("clustree$", nm)) return(objs[[nm]]); NULL
    }
  ),
  table = NULL
)
