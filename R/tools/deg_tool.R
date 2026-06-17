# R/tools/deg_tool.R
TOOL_NAME         <- "deg"
TOOL_CATEGORY     <- "差异/富集分析"
TOOL_OMICS        <- "bulk_rna"
TOOL_ORDER        <- 20
TOOL_DISPLAY_NAME <- "差异分析"

TOOL_SCHEMA <- list(
  deg_input       = list(type = "select", choices = NULL, required = TRUE,  description = "输入矩阵名（如 count）"),
  contrast        = list(type = "select", choices = NULL, required = TRUE,  description = "选择比较组"),
  method          = list(type = "select", choices = c("DESeq2","limma","edgeR","wilcox","t"), default = "DESeq2"),
  logFC_threshold = list(type = "number", min = 0, default = 0,            description = "logFC阈值(0保留全部)"),
  padj_threshold  = list(type = "number", min = 0, max = 1, default = 1,   description = "Adjusted p阈值(1保留全部)"),
  filter          = list(type = "boolean", default = TRUE,                  description = "自动过滤低表达基因")
)

TOOL_RUN <- function(inputs, project) {
  if (is.null(inputs$contrast)  || inputs$contrast  == "") stop("请先选择对比组（contrast）")
  if (is.null(inputs$deg_input) || inputs$deg_input == "") stop("请先选择输入矩阵")

  count_mat <- project$data[[inputs$deg_input]]
  group     <- project$meta$group_info

  if (is.null(count_mat)) stop("数据矩阵不存在: ", inputs$deg_input)
  if (is.null(group))     stop("缺少分组信息")
  if (length(group) != ncol(count_mat))
    stop("分组数(", length(group), ") 与样本数(", ncol(count_mat), ") 不匹配")

  res <- engine_deg(
    count_mat       = count_mat,
    group           = group,
    contrast        = inputs$contrast,
    method          = inputs$method,
    p.value         = inputs$padj_threshold,
    logFC           = inputs$logFC_threshold,
    FilterGene      = inputs$filter,
    log_transformed = project$meta$log_state %||% FALSE
  )

  project$meta$deg_contrast <- inputs$contrast

  # 底层包映射
  deg_pkg <- switch(inputs$method,
    DESeq2 = "DESeq2", limma = "limma", edgeR = "edgeR",
    wilcox = "stats", t = "stats", "unknown"
  )

  # 返回 named list，engine$run 会存为 deg_result（key = "deg_result"）
  list(
    data     = list(result = res$result),
    messages = res$note,
    method_info = record_method("deg", "差异分析", inputs$method, deg_pkg, inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot  = NULL,
  # result 是 data.frame，直接返回给 renderDataTable
  table = function(result) {
    if (is.data.frame(result)) return(result)
    NULL
  }
)
