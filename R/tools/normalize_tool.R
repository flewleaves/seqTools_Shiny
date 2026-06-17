# R/tools/normalize_tool.R
# 标准化工具：参数校验 + 调用 engine_normalize

TOOL_NAME <- "normalize"
TOOL_CATEGORY <- "数据处理"
TOOL_OMICS <- "bulk_rna"
TOOL_ORDER <- 15
TOOL_DISPLAY_NAME <- "标准化"

TOOL_SCHEMA <- list(
  method = list(type = "select", choices = c("cpm","rpkm","tpm","tmm","vst","rlog"), required = TRUE, default = "cpm"),
  use_standard_length = list(type = "boolean", default = FALSE, description = "TPM缺少基因长度时，是否使用标准长度"),
  log_transform = list(type = "boolean", default = FALSE, description = "是否 log(x+1) 转化"),
  log_base = list(type = "select", choices = c("log2","log10","ln"), default = "log2", description = "log 底数（仅 log_transform=TRUE 时生效）")
)

TOOL_RUN <- function(inputs, project) {
  # 参数校验与准备
  count_mat <- project$data[["count"]]
  if (is.null(count_mat)) stop("缺少 count 矩阵，请先导入数据")

  gene_length <- project$meta$gene.length
  if (is.null(gene_length) && isTRUE(inputs$use_standard_length)) {
    gene_length <- seqTools::get_standard_gene_length(
      count_mat,
      species = project$meta$species %||% "Hs",
      from = project$meta$id_type %||% "SYMBOL"
    )
    project$meta$gene.length <- gene_length  # 缓存
  }

  # 调用纯计算引擎
  res <- engine_normalize(
    count_mat = count_mat,
    method = inputs$method,
    group = project$meta$group_info,
    gene.length = gene_length,
    species = project$meta$species %||% "Hs",
    id_type = project$meta$id_type %||% "SYMBOL"
  )

  # 可选 log 转化
  store_key <- inputs$method
  if (isTRUE(inputs$log_transform)) {
    lb <- inputs$log_base %||% "log2"
    if (lb == "log2")   res$matrix <- log2(res$matrix + 1)
    else if (lb == "log10") res$matrix <- log10(res$matrix + 1)
    else                res$matrix <- log(res$matrix + 1)
    store_key <- paste0("log_", store_key)
  }

  # 结果存入 project（key 为方法名或 log_方法名）
  project$data[[store_key]] <- res$matrix

  list(
    data = res$matrix,
    messages = c(res$note, if (isTRUE(inputs$log_transform)) paste("log转化:", lb)),
    method_info = record_method("normalize", "标准化", paste0("Seurat::", inputs$method), "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = NULL,
  table = NULL
)
