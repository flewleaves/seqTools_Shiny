# R/utils/tool_helpers.R
# 公共工具辅助函数

#' 解析分组信息文本
#' @param text 格式如 "组名,样本数;..."
#' @return 字符向量，每个元素是一个样本所属组
parse_group_info <- function(text) {
  if (is.null(text) || text == "") return(NULL)
  
  groups <- strsplit(gsub(" ", "", text), ";")[[1]]
  res <- c()
  for (g in groups) {
    parts <- strsplit(g, ",")[[1]]
    if (length(parts) >= 2) {
      n <- suppressWarnings(as.numeric(parts[2]))
      if (!is.na(n) && n > 0) {
        res <- c(res, rep(parts[1], n))
      }
    }
  }
  return(res)
}

#' 自动判断表达矩阵的数据类型、物种、ID类型、log状态
#' @param exp 数据框（含基因列）
#' @param meta 列表，会被补充/覆盖检测字段
#' @return 补充后的 meta 列表
detect_data <- function(exp, meta) {
  t <- 0.98
  gene_col <- 1
  sample_col <- 2:ncol(exp)
  size <- round(sqrt(nrow(exp)))
  
  # 基因检测 -> 物种
  genes <- exp[, gene_col, drop = TRUE]
  sg <- genes[1:size]
  meta[["species"]] <- ifelse(sum(stringr::str_to_title(sg) == sg) >= t * size, "Mm", "Hs")
  
  # ID类型检测
  if (sum(stringr::str_detect(sg, "ENSG") | stringr::str_detect(sg, "ENSMUSG")) >= t * size) {
    meta[["id_type"]] <- "ENSEMBL"
  } else if (sum(stringr::str_detect(sg, "^\\d+$")) >= t * size) {
    meta[["id_type"]] <- "ENTREZID"
  } else if (sum(stringr::str_detect(sg, "^[a-zA-Z][-a-zA-Z0-9]+$")) >= t * size) {
    meta[["id_type"]] <- "SYMBOL"
  } else {
    meta[["id_type"]] <- NULL
  }
  
  # 只取数值列
  ex <- as.matrix(exp[, sample_col])
  if (ncol(ex) == 0) return(meta)
  
  # log状态检测
  qx <- as.numeric(quantile(ex, c(0.5, 0.99), na.rm = TRUE))
  LogC <- !((qx[2] > 50) && qx[1] > 20)
  meta[["log_state"]] <- LogC
  if (LogC) ex <- 2^ex - 1
  
  # data_type检测
  if (sum(abs(ex - round(ex)) < 1e-6) > t * nrow(ex) * ncol(ex)) {
    meta[["data_type"]] <- "count"
  } else if (mean(colSums(ex)) > (1 - t) * 1e6 && mean(colSums(ex)) < (1 + t) * 1e6) {
    meta[["data_type"]] <- ifelse(stringr::str_detect(tolower(meta[["filename"]]), "tpm"), "tpm", "cpm")
  } else if (stringr::str_detect(tolower(meta[["filename"]]), "fpkm")) {
    meta[["data_type"]] <- "fpkm"
  } else {
    meta[["data_type"]] <- NULL
  }
  
  return(meta)
}