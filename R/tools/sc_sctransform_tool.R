# R/tools/sc_sctransform_tool.R
# SCTransform 标准化替代方案

TOOL_NAME <- "sc_sctransform"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "SCTransform标准化"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 14

TOOL_SCHEMA <- list(
  dataList_key = list(type = "text", required = FALSE,
                      description = "指定批次，如 dataList1。留空则使用第一个"),
  vars_to_regress = list(type = "text", default = "percent.mt",
                         description = "需要回归的变量，逗号分隔"),
  nfeatures = list(type = "integer", default = 3000, min = 500, max = 10000,
                   description = "高变基因数量"),
  save_to_disk = list(type = "boolean", default = FALSE,
                      description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  if (!sc_has_dataLists(project)) stop("无单细胞数据，请先导入")

  # 选择指定批次或第一个
  dataLists <- sc_get_dataLists(project)
  key <- inputs$dataList_key
  if (is.null(key) || !nzchar(key) || !key %in% names(dataLists)) {
    key <- names(dataLists)[1]
  }
  dataList <- dataLists[[key]]

  res <- engine_sc_sctransform(
    dataList = dataList,
    vars_to_regress = inputs$vars_to_regress %||% "percent.mt",
    nfeatures = inputs$nfeatures
  )

  project$data[["sct_seurat"]] <- res$seurat

  if (inputs$save_to_disk) {
    sc_save_to_disk(project, "sct_seurat")
  }

  list(
    data = list(),
    messages = res$note,
    method_info = record_method("sc_sctransform", "SCTransform标准化",
                                "Seurat::SCTransform → RunPCA", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(plot = NULL, table = NULL)
