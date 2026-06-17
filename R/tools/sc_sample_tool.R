# R/tools/sc_sample_tool.R
# 单细胞随机抽样 — seqTools::sample_object

TOOL_NAME <- "sc_sample"
TOOL_CATEGORY <- "数据预处理"
TOOL_DISPLAY_NAME <- "细胞抽样"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 22

TOOL_SCHEMA <- list(
  proportion = list(type = "number", required = TRUE,
    default = 0.5, min = 0.01, max = 1.0,
    description = "抽样比例（0-1，如 0.5 = 保留 50% 细胞）")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  proportion <- inputs$proportion
  if (is.null(proportion) || proportion <= 0 || proportion > 1)
    stop("抽样比例必须在 0-1 之间")

  n_before <- ncol(seurat_obj)
  seurat_obj <- seqTools::sample_object(seurat_obj, proportion)
  n_after <- ncol(seurat_obj)

  # 保存抽样后的对象
  sc_save_to_disk(project, key)

  list(data = list(),
       messages = paste("细胞抽样完成:", n_before, "→", n_after,
                        "(", round(100 * n_after / n_before, 1), "%)"),
       method_info = record_method("sc_sample", "细胞抽样",
         "seqTools::sample_object", "seqTools", inputs))
}

TOOL_OUTPUTS <- list(
  plot = NULL,
  table = NULL
)
