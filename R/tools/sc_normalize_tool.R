# R/tools/sc_normalize_tool.R
# 单细胞标准化 + 降维：合并多批次 → LogNormalize → 高变基因 → PCA

TOOL_NAME <- "sc_normalize"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "标准化+PCA"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 12

TOOL_SCHEMA <- list(
  cellcycle_scoring = list(type = "boolean", default = FALSE,
                           description = "是否回归细胞周期"),
  vars_to_regress = list(type = "text", default = "percent.mt",
                         description = "需要回归的变量，逗号分隔"),
  nfeatures = list(type = "integer", default = 2000, min = 500, max = 10000,
                   description = "高变基因数量"),
  save_to_disk = list(type = "boolean", default = FALSE,
                      description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  if (!sc_has_dataLists(project)) stop("无单细胞数据，请先导入")

  dataLists <- sc_get_dataLists(project)
  species <- project$meta$species %||% "Hs"

  res <- engine_sc_normalize_reduce(
    dataLists = dataLists,
    species = species,
    cellcycle_scoring = inputs$cellcycle_scoring,
    vars_to_regress = inputs$vars_to_regress %||% "percent.mt",
    nfeatures = inputs$nfeatures
  )

  project$data[["merged_seurat"]] <- res$seurat

  if (inputs$save_to_disk) {
    sc_save_to_disk(project, "merged_seurat")
  }

  list(
    data = list(),
    messages = res$note,
    method_info = record_method("sc_normalize", "标准化+PCA",
                                "Seurat::LogNormalize → FindVariableFeatures → ScaleData → RunPCA", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(plot = NULL, table = NULL)
