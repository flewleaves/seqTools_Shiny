# R/imports/bulk_rna_import.R

IMPORT_ID   <- "bulk_rna"
IMPORT_NAME <- "RNA-seq"

IMPORT_SCHEMA <- list(
  file        = list(type = "file",   required = TRUE,  description = "基因表达矩阵 CSV/TXT"),
  gene_col    = list(type = "text",   required = FALSE, description = "基因ID列名，默认第1列"),
  sample_cols = list(type = "text",   required = FALSE, description = "样本列名，逗号分隔，默认除基因列外所有列"),
  gene_length = list(type = "text",   required = FALSE, description = "基因长度列名（可选，用于 RPKM/FPKM 计算）"),
  data_type   = list(type = "select", choices = c("auto","count","tpm","fpkm","cpm"), default = "auto"),
  log_state   = list(type = "select", choices = c("auto","TRUE","FALSE"), default = "auto"),
  species     = list(type = "select", choices = c("auto","Hs","Mm"), default = "auto"),
  id_type     = list(type = "select", choices = c("auto","SYMBOL","ENSEMBL","ENTREZID"), default = "auto"),
  id_convert  = list(type = "select", choices = c("no","to_SYMBOL"), default = "no",
                     description = "是否将基因ID转换为SYMBOL"),
  dup_method  = list(type = "select", choices = c("settings","mean","max","kmax"), default = "settings",
                     description = "去重策略，settings=使用系统设置"),
  group_info  = list(type = "text",   required = TRUE,  description = "格式: 组名,样本数;...  例: Control,3;Treat,3")
)

IMPORT_RUN <- function(file_path, params) {
  require(data.table); require(seqTools); require(stringr)

  # ---------- 读文件 ----------
  df <- data.table::fread(file_path, header = TRUE, data.table = FALSE)
  df <- na.omit(df)
  all_cols <- names(df)

  # ---------- 确定基因列和样本列 ----------
  gene_col <- if (!is.null(params$gene_col) && nzchar(params$gene_col) && params$gene_col %in% all_cols) {
    params$gene_col
  } else {
    all_cols[1]
  }

  sample_cols <- if (!is.null(params$sample_cols) && nzchar(params$sample_cols)) {
    cols <- trimws(strsplit(params$sample_cols, ",")[[1]])
    cols <- cols[cols %in% all_cols & cols != gene_col]
    if (length(cols) == 0) setdiff(all_cols, gene_col) else cols
  } else {
    setdiff(all_cols, gene_col)
  }

  gene_length_col <- if (!is.null(params$gene_length) && nzchar(params$gene_length) &&
                         params$gene_length %in% all_cols) params$gene_length else NULL

  # 只保留基因列 + 样本列
  df <- df[, c(gene_col, sample_cols), drop = FALSE]

  # ---------- 自动检测 ----------
  meta <- list(filename = basename(file_path))
  meta <- detect_data(df, meta)

  # ---------- 用户参数覆盖 ----------
  if (!is.null(params$data_type) && params$data_type != "auto") meta$data_type <- params$data_type
  if (!is.null(params$species)   && params$species   != "auto") meta$species   <- params$species
  if (!is.null(params$id_type)   && params$id_type   != "auto") meta$id_type   <- params$id_type
  if (!is.null(params$log_state) && params$log_state != "auto") {
    meta$log_state <- as.logical(params$log_state)
  }

  # ---------- 基因ID转换 ----------
  if (!is.null(params$id_convert) && params$id_convert == "to_SYMBOL" &&
      !is.null(meta$id_type) && meta$id_type != "SYMBOL") {
    tryCatch({
      colnames(df)[1] <- "Geneid"
      df <- seqTools::Quick_ID_conversion(df, species = meta$species,
                                          from = meta$id_type, to = "SYMBOL")
      meta$id_type <- "SYMBOL"
    }, error = function(e) {
      warning("基因ID转换失败: ", e$message)
    })
  }

  # ---------- 去重（dup_method = "settings" 时用 params$dup_from_settings） ----------
  dup_method <- if (!is.null(params$dup_method) && params$dup_method != "settings") {
    params$dup_method
  } else {
    params$dup_from_settings %||% "mean"   # UI 层把 settings 里的值通过这个字段传入
  }
  df <- seqTools::remove_dup(df, 1, method = dup_method)

  row.names(df) <- df[, 1]
  df <- df[, -1, drop = FALSE]
  if (!is.null(meta$data_type) && meta$data_type == "count") df <- round(df)

  # ---------- 解析分组 ----------
  group_vec <- parse_group_info(params$group_info %||% "")
  if (!is.null(group_vec) && length(group_vec) != ncol(df)) {
    stop("分组数(", length(group_vec), ") 与样本数(", ncol(df), ") 不匹配")
  }

  list(
    data       = setNames(list(as.matrix(df)), meta$data_type %||% "count"),
    meta       = list(
      data_type   = meta$data_type,
      species     = meta$species,
      id_type     = meta$id_type,
      log_state   = meta$log_state %||% FALSE,
      gene.length = gene_length_col,
      group_info  = group_vec,
      filename    = meta$filename
    ),
    omics_type = "bulk_rna"
  )
}
