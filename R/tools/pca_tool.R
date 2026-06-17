# R/tools/pca_tool.R
# PCA 分析工具

TOOL_NAME <- "pca"
TOOL_CATEGORY <- "数据绘图"
TOOL_OMICS <- "bulk_rna"
TOOL_ORDER <- 35
TOOL_DISPLAY_NAME <- "PCA图"

TOOL_SCHEMA <- list(
  pca_input = list(type = "select", choices = NULL, required = TRUE, description = "输入数据名（如 count, log_cpm, vst）"),
  pca_dims = list(type = "integer", default = 2, min = 2, description = "保留维度"),
  pca_label = list(type = "select", choices = c("ind","none"), default = "none", description = "是否添加标签"),
  pca_eclip = list(type = "boolean", default = TRUE, description = "是否使用椭圆框选"),
  pca_color = list(type = "string", default = "#00AFBB;#E7B800;#FC4E07", description = "分组颜色(HEX码),分号隔开")
)

TOOL_RUN <- function(inputs, project) {
  mat <- project$data[[inputs$pca_input]]
  if (is.null(mat)) stop("输入数据不存在: ", inputs$pca_input)

  colors <- strsplit(inputs$pca_color, ";")[[1]]

  res <- engine_pca(
    mat = mat,
    group = project$meta$group_info,
    ncp = inputs$pca_dims,
    label = inputs$pca_label,
    addEllipses = inputs$pca_eclip,
    palette = colors
  )

  list(
    data = list(plot = res$plot),
    messages = res$note,
    method_info = record_method("pca", "PCA图", "stats::prcomp", "stats", inputs)
  )
}

TOOL_OUTPUTS <- list(
  plot = function(result) result,
  table = NULL
)