# R/utils/tool_helpers.R
# 公共工具辅助函数

#' 构建相对于 APP_ROOT 的路径
app_root <- function(...) file.path(APP_ROOT, ...)

#' 记录分析方法信息（底层函数 + 版本 + 参数）
#' @param tool_id      工具标识
#' @param tool_display 工具显示名
#' @param underlying   底层函数/方法名（如 DESeq2、limma、prcomp）
#' @param pkg          底层函数所属 R 包
#' @param params       实际使用的参数列表
#' @return method_info 列表，供 engine$run 存入 project$results$_methods
record_method <- function(tool_id, tool_display, underlying, pkg, params) {
  # 去除非参数字段（project、data 等引用对象不记录）
  clean_params <- params
  clean_params$project <- NULL
  clean_params$count_mat <- NULL
  clean_params$mat <- NULL
  clean_params$dataList <- NULL
  clean_params$dataLists <- NULL
  clean_params$merged_seurat <- NULL
  clean_params$seurat_obj <- NULL

  ver <- tryCatch(as.character(packageVersion(pkg)), error = function(e) "未知")

  list(
    tool      = tool_id,
    tool_name = tool_display,
    method    = underlying,
    package   = pkg,
    version   = ver,
    params    = clean_params,
    time      = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

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