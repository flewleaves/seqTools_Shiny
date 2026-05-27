# R/engines/normalize.R
# 纯计算层：标准化引擎，与 project/state 零耦合

#' 标准化表达矩阵
#' @param count_mat  count 矩阵（matrix/data.frame）
#' @param method     方法名：cpm, rpkm, tpm, tmm, vst, rlog
#' @param group      分组向量（可选）
#' @param gene.length 基因长度向量（TPM/RPKM 必需）
#' @param species    物种，用于自动补全基因长度
#' @param id_type    基因 ID 类型
#' @return list(matrix=标准化矩阵, method=方法, note=日志)
engine_normalize <- function(count_mat, method = "cpm",
                             group = NULL, gene.length = NULL,
                             species = "Hs", id_type = "SYMBOL") {
  require(seqTools)

  method <- tolower(method)
  allowed <- c("cpm", "rpkm", "tpm", "tmm", "vst", "rlog")
  if (!method %in% allowed) {
    stop("标准化方法必须是: ", paste(allowed, collapse = ", "))
  }

  # TPM/RPKM 缺基因长度时自动补全
  if (method %in% c("tpm", "rpkm") && is.null(gene.length)) {
    message("[engine_normalize] 使用标准基因长度补全...")
    gene.length <- seqTools::get_standard_gene_length(
      count_mat, species = species, from = id_type
    )
  }

  mat <- seqTools::normalization(
    count_mat,
    method = method,
    group = group,
    gene.length = gene.length
  )

  list(
    matrix = mat,
    method = method,
    note = paste(method, "标准化完成，维度:", paste(dim(mat), collapse = " x "))
  )
}
