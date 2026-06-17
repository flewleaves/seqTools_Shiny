# R/tools/filter_tool.R
# 数据过滤工具

TOOL_NAME <- "filter"
TOOL_CATEGORY <- "数据处理"
TOOL_OMICS <- "bulk_rna"
TOOL_ORDER <- 10
TOOL_DISPLAY_NAME <- "数据过滤"

TOOL_SCHEMA <- list(
  min_count = list(type = "integer", default = 10, min = 0, description = "每个基因在不同样本中表达量之和至少大于"),
  exclude_noncoding = list(type = "boolean", default = FALSE, description = "排除常见非编码基因"),
  exclude_unknown = list(type = "boolean", default = FALSE, description = "排除未知基因（未被注释的基因）")
)

TOOL_RUN <- function(inputs, project) {
  if (length(project$data) == 0) stop("无可用数据矩阵")

  res <- engine_filter(
    mats = project$data,
    min_count = inputs$min_count %||% 10,
    exclude_noncoding = inputs$exclude_noncoding %||% FALSE,
    exclude_unknown = inputs$exclude_unknown %||% FALSE,
    species = project$meta$species %||% "Hs",
    id_type = project$meta$id_type %||% "SYMBOL"
  )

  # 批量更新 project$data
  project$set_data(res$mats)

  list(
    data = res$mats,
    messages = c(res$note, res$stats),
    method_info = record_method("filter", "数据过滤", "手动过滤", "base", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = NULL,
  table = NULL
)
