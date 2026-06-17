# R/engines/sc_engine.R
# 单细胞分析引擎 — seqTools 的薄封装层

engine_sc_preprocessing <- function(dataList, species = "Hs", doublet_find = TRUE,
                                     filter = NULL, samples = NULL) {
  require(seqTools)
  require(Seurat)
  require(SummarizedExperiment)
  require(BiocParallel)
  result <- seqTools::scRNA_preprocessing(
    dataList = dataList, species = species,
    DoubletFind = doublet_find, filter = filter,
    progress_saving = FALSE, samples = samples %||% "orig.ident"
  )
  n_cells <- sapply(result, ncol)
  list(dataList = result,
       note = paste0("预处理完成: ", length(result), "个样本, ",
                     sum(n_cells), "个细胞 (",
                     if (doublet_find) "含双细胞去除" else "未去除双细胞", ")"))
}

engine_sc_normalize_reduce <- function(dataLists, species = "Hs",
                                        cellcycle_scoring = FALSE,
                                        vars_to_regress = "percent.mt",
                                        nfeatures = 2000) {
  require(seqTools)
  require(Seurat)
  vars <- sc_parse_vars(vars_to_regress)
  merged <- seqTools::scRNA_Normalization_Reduction(
    dataLists = dataLists, species = species,
    CellCycleScoring = cellcycle_scoring,
    vars.to.regress = vars,
    nfeatures = nfeatures,
    progress_saving = FALSE
  )
  list(seurat = merged,
       note = paste0("标准化+PCA完成: ", ncol(merged), "个细胞, ",
                     nrow(merged), "个基因, ",
                     if (cellcycle_scoring) "已回归细胞周期" else "未回归细胞周期"))
}

engine_sc_sctransform <- function(dataList, vars_to_regress = "percent.mt",
                                   nfeatures = 3000) {
  require(seqTools)
  require(Seurat)
  vars <- sc_parse_vars(vars_to_regress)
  result <- seqTools::scRNA_SCTransform(
    dataList = dataList,
    vars.to.regress = vars,
    nfeatures = nfeatures,
    progress_saving = FALSE
  )
  list(seurat = result,
       note = paste0("SCTransform完成: ", ncol(result), "个细胞, ", nrow(result), "个基因"))
}

engine_sc_integrate <- function(merged_seurat, sctransform = FALSE, theta = NULL) {
  require(seqTools)
  require(Seurat)
  integrated <- seqTools::scRNA_Integration(
    Merge.Seurat    = merged_seurat,
    SCTransform     = sctransform,
    theta           = theta,
    progress_saving = FALSE
  )
  list(seurat = integrated, note = "Harmony整合完成")
}

engine_sc_cluster <- function(seurat_obj, PCs = NULL,
                               resolution = c(0.2, 0.4, 0.6, 0.8, 1.0),
                               reduction = "harmony", sctransform = FALSE,
                               k_param = 20, n_neighbors = 30L, min_dist = 0.3,
                               method = "UMAP") {
  require(seqTools)
  require(Seurat)
  clustered <- seqTools::scRNA_clustering(
    scRNA           = seurat_obj,
    PCs             = PCs,
    resolution      = resolution,
    reduction       = reduction,
    SCTransform     = sctransform,
    k.param         = k_param,
    n.neighbors     = n_neighbors,
    min.dist        = min_dist,
    method          = method,
    progress_saving = FALSE
  )
  last_res <- tail(resolution, 1)
  n_clusters <- length(unique(clustered@meta.data[[
    paste0(if (sctransform) "SCT" else "RNA", "_snn_res.", last_res)]]))
  list(seurat = clustered,
       note = paste0("聚类完成 (", method, "), ", n_clusters, "个cluster"))
}

engine_sc_markers <- function(seurat_obj, n = 10, draw = TRUE,
                               high_expr = TRUE, sctransform = FALSE) {
  require(seqTools)
  require(Seurat)
  markers <- seqTools::Find_topn_markers(
    scRNA = seurat_obj, n = n, draw = FALSE,
    high_expr = high_expr, SCTransform = sctransform
  )
  plot <- NULL
  if (draw && length(markers) > 0) {
    plot <- Seurat::DotPlot(seurat_obj, features = unique(markers)) +
      ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(
        hjust = 1, vjust = 1, angle = 45))
  }
  list(markers = markers, plot = plot,
       note = paste0("标记基因: ", length(unique(markers)), "个"))
}

engine_sc_cell_ratio <- function(seurat_obj, anno_by, group_by,
                                  return_table = FALSE) {
  require(seqTools)
  if (is.null(seurat_obj@meta.data[[anno_by]]))
    stop("meta.data 中不存在列: ", anno_by)
  anno_vec  <- seurat_obj@meta.data[[anno_by]]
  group_vec <- if (!is.null(group_by) && nzchar(group_by) &&
                    !is.null(seurat_obj@meta.data[[group_by]]))
    seurat_obj@meta.data[[group_by]] else rep("All", ncol(seurat_obj))
  ratio_df <- as.data.frame(prop.table(table(anno_vec, group_vec), margin = 2))
  colnames(ratio_df)[1:2] <- c("cluster", "group")
  plot <- seqTools::draw_ratio(anno.by = anno_vec, group.by = group_vec,
                                return_table = FALSE)
  list(plot = plot, table = if (return_table) ratio_df else NULL,
       note = "细胞比例计算完成")
}

engine_sc_quick_pipeline <- function(dataLists, species = "Hs",
                                      sctransform = FALSE,
                                      cellcycle_scoring = FALSE,
                                      vars_to_regress = "percent.mt",
                                      PCs = NULL, resolution = c(0.2, 0.4, 0.6, 0.8, 1.0)) {
  require(seqTools)
  require(Seurat)
  require(BiocParallel)
  vars <- sc_parse_vars(vars_to_regress)
  result <- seqTools::quick_single_cell(
    dataLists = dataLists, species = species,
    SCTransform = sctransform,
    progress_saving = FALSE,
    CellCycleScoring = cellcycle_scoring,
    vars.to.regress = vars, PCs = PCs, resolution = resolution
  )
  list(seurat = result,
       note = paste0("快速分析完成: ", ncol(result), "个细胞, ",
                     length(unique(Idents(result))), "个cluster"))
}
