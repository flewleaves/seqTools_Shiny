

#' 自动判断表达矩阵的数据类型
#' @param exp 数据框
#' @return 字符串：
detect_data <- function(exp, meta) {
  
  t = 0.98
  gene_col = 1
  sample_col = 2:ncol(exp)
  size = round(sqrt(nrow(exp)))
  #基因检测
  genes = exp[,gene_col, drop = TRUE]
  sg = genes[1:size] 
  meta[["species"]] = ifelse(sum(stringr::str_to_title(sg) == sg) >= t*size, "Mm", "Hs")
  
  if(sum(stringr::str_detect(sg, "ENSG") | stringr::str_detect(sg, "ENSMUSG")) >= t*size){
    meta[["id_type"]] = "ENSEMBL"
  }else if(sum(stringr::str_detect(sg, "\\d{4}"))>= t*size){
    meta[["id_type"]] = "ENTREZID"
  }else if(sum(stringr::str_detect(sg, "^[a-zA-Z][-a-zA-Z0-9]+$"))>= t*size){
    meta[["id_type"]] = "SYMBOL"
  }else{
    meta[["id_type"]] = NULL
  }

  #只取数值列
  ex <- as.matrix(exp[, sample_col])
  if (ncol(ex) == 0) return("样本数据有误")
  
  #log_state
  qx <- as.numeric(quantile(ex, c(0.5, 0.99), na.rm=T))
  LogC <- !((qx[2] > 50) && qx[1] > 20)
  meta[["log_state"]] = LogC
  
  if(LogC) ex <- 2^ex - 1  

  #data_type
  if(sum(abs(ex - round(ex)) < 1e-6) > t*nrow(ex)*ncol(ex)){
    meta[["data_type"]] = 'count'
  }else if(mean(colSums(ex)) > (1-t)*1e6 && mean(colSums(ex)) < (1+t)*1e6){
    meta[["data_type"]] = ifelse(stringr::str_detect(tolower(meta[["filename"]]), "tpm"), "TPM", "CPM")
  }else if(stringr::str_detect(tolower(meta[["filename"]]), "fpkm")){
    meta[["data_type"]] = 'FPKM'
  }else{
    meta[["data_type"]] = NULL
  }

  return(meta)
 
}


parse_group_info <- function(text) {
  if (is.null(text) || text == "") return(NULL)
  
  # 按 ; 分割各组
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