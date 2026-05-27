# R/engines/pca.R
# 纯计算层：PCA 引擎

#' PCA 分析引擎
#' @param mat        表达矩阵
#' @param group      分组向量
#' @param ncp        保留维度
#' @param label      是否添加标签 ("ind"/"none")
#' @param addEllipses 是否椭圆框选
#' @param palette    颜色向量
#' @return list(plot=ggplot对象, note=日志)
engine_pca <- function(mat, group = NULL, ncp = 2,
                       label = "none", addEllipses = TRUE,
                       palette = c("#00AFBB", "#E7B800", "#FC4E07")) {
  require(seqTools)

  p <- seqTools::draw_pca(
    mat,
    group = group,
    ncp = ncp,
    label = label,
    addEllipses = addEllipses,
    palette = palette
  )

  list(
    plot = p,
    note = paste("PCA 完成，维度:", ncp)
  )
}
