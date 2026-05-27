# R/engines/gsea.R
# 纯计算层：GSEA 引擎

#' 构建基因排序向量
#' @param deg_res    差异分析结果
#' @param method     logFC / Signed-p / Signal-to-noise
#' @param tpm_mat    表达矩阵（Signal-to-noise 必需）
#' @param group      分组向量（Signal-to-noise 必需）
#' @param contrast   对比组（Signal-to-noise 必需）
#' @return 命名向量（基因名=排序值）
engine_build_rank <- function(deg_res, method = "logFC",
                              tpm_mat = NULL, group = NULL, contrast = NULL) {
  geneList <- rownames(deg_res)
  rg <- NULL

  if (method == "logFC") {
    rg <- deg_res$log2FoldChange
    names(rg) <- geneList
  } else if (method == "Signed-p") {
    rg <- -log(deg_res$p.value) * sign(deg_res$log2FoldChange)
    names(rg) <- geneList
  } else if (method == "Signal-to-noise") {
    if (is.null(tpm_mat)) stop("Signal-to-noise 需要表达矩阵")
    if (is.null(group)) stop("Signal-to-noise 需要分组信息")
    if (is.null(contrast)) stop("Signal-to-noise 需要对比组信息")
    require(seqTools)
    rg <- seqTools::s2nCalulator(tpm_mat, group = group, contrast = contrast)
    rg <- na.omit(rg)
    rg <- rg[names(rg) %in% geneList]
  }

  if (is.null(rg)) stop("基因排序向量构建失败")
  rg[order(rg, decreasing = TRUE)]
}

#' GSEA 分析引擎
#' @param rank_vec   排序向量
#' @param genesets   基因集名称或自定义gmt路径
#' @param species    物种
#' @param from       ID类型
#' @param p.value    p值阈值
#' @param minGSSize  最小基因集
#' @param maxGSSize  最大基因集
#' @return list(result=gseaResult, note=日志)
engine_gsea <- function(rank_vec, genesets = "GO", species = "Hs", from = "SYMBOL",
                        p.value = 1, minGSSize = 10, maxGSSize = 500) {
  require(seqTools)
  require(clusterProfiler)

  # 处理自定义基因集
  if (genesets != "GO" && genesets != "KEGG" && genesets != "Reactome" && file.exists(genesets)) {
    genesets <- clusterProfiler::read.gmt(genesets)
  }

  gsea_res <- seqTools::quick_GSEA(
    rank_vec,
    genesets = genesets,
    species = species,
    from = from,
    p.value = p.value,
    minGSSize = minGSSize,
    maxGSSize = maxGSSize
  )

  list(
    result = gsea_res,
    note = paste("GSEA 完成，共", nrow(gsea_res@result), "个通路")
  )
}

#' GSEA 单通路可视化引擎
#' @param gsea_res   gseaResult 对象
#' @param geneSetIDs 通路名称向量
#' @param subPlot    分图数量
#' @param addPval    是否添加p值标签
#' @param pvalX      p值标签X坐标
#' @param pvalY      p值标签Y坐标
#' @return list(plot=ggplot对象, note=日志)
engine_gsea_plot <- function(gsea_res, geneSetIDs, subPlot = 3,
                             addPval = TRUE, pvalX = 0.95, pvalY = 0.8) {
  require(GseaVis)

  p <- GseaVis::gseaNb(
    object = gsea_res,
    geneSetID = geneSetIDs,
    subPlot = as.integer(subPlot),
    addPval = as.logical(addPval),
    pvalX = pvalX,
    pvalY = pvalY
  )

  list(
    plot = p,
    note = paste("GSEA 通路图绘制完成:", paste(geneSetIDs, collapse = ", "))
  )
}

#' GSEA dotplot 引擎
#' @param gsea_res   gseaResult 对象
#' @param topn       展示前N个
#' @param pajust     绘图p值阈值
#' @return list(plot=ggplot对象, note=日志)
engine_gsea_dotplot <- function(gsea_res, topn = 10, pajust = 0.05) {
  require(GseaVis)

  p <- GseaVis::dotplotGsea(
    data = gsea_res,
    topn = topn,
    pajust = pajust
  )$plot

  list(
    plot = p,
    note = paste("GSEA dotplot 完成，展示前", topn, "个")
  )
}
