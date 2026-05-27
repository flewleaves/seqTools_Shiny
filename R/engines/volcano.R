# R/engines/volcano.R
# 纯计算层：火山图引擎

#' 火山图引擎
#' @param deg_res      差异分析结果数据框
#' @param padj.val     Adjust p 值阈值
#' @param logFC        logFC 阈值
#' @param color        颜色向量（下调/ns/上调）
#' @param alpha        透明度
#' @param size         点大小
#' @param label        标记基因名向量
#' @param highlight    高亮基因名向量
#' @param highlight_color 高亮颜色
#' @param max.overlaps 最大标记重叠数
#' @return list(plot=ggplot对象, note=日志)
engine_volcano <- function(deg_res, padj.val = 0.05, logFC = 1,
                           color = c("#6a82ed", "grey", "#ed6f6f"),
                           alpha = 0.3, size = 2,
                           label = NULL, highlight = NULL,
                           highlight_color = "#07a818", max.overlaps = 10) {
  require(seqTools)

  p <- seqTools::draw_Volcano(
    deg_res,
    padj.val = padj.val,
    logFC = logFC,
    color = color,
    alpha = alpha,
    size = size,
    label = label,
    highlight = highlight,
    highlight_color = highlight_color,
    max.overlaps = max.overlaps
  )

  list(
    plot = p,
    note = "火山图绘制完成"
  )
}
