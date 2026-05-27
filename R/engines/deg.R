# R/engines/deg.R
# 纯计算层：差异分析引擎

#' 差异分析引擎
#' @param count_mat  表达矩阵
#' @param group      分组向量
#' @param contrast   对比组格式 "group1-group2"
#' @param method     DESeq2/limma/edgeR/wilcox/t
#' @param p.value    p值阈值
#' @param logFC      logFC阈值
#' @param FilterGene 是否过滤低表达
#' @param log_transformed 数据是否已log化
#' @return list(result=差异结果df, contrast=对比组, note=日志)
engine_deg <- function(count_mat, group, contrast,
                       method = "DESeq2", p.value = 1, logFC = 0,
                       FilterGene = TRUE, log_transformed = FALSE) {
  require(seqTools)

  result <- seqTools::DEG_analysis_v2(
    Input = count_mat,
    group = group,
    contrast = contrast,
    method = method,
    p.value = p.value,
    logFC = logFC,
    FilterGene = FilterGene,
    log_transformed = log_transformed
  )
  result <- na.omit(result)

  list(
    result = result,
    contrast = contrast,
    note = paste("差异分析完成:", nrow(result), "个基因 | 方法:", method, "| 对比:", contrast)
  )
}
