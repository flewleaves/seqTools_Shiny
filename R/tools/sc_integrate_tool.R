# R/tools/sc_integrate_tool.R
# Harmony 多批次整合。单批次时自动跳过。

TOOL_NAME <- "sc_integrate"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "Harmony整合"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 16

TOOL_SCHEMA <- list(
  sctransform = list(type = "boolean", default = FALSE,
                     description = "是否使用SCTransform数据"),
  theta = list(type = "number", required = FALSE,
    description = "Harmony theta 参数（0-10，默认 2）。越大批次校正越强，但可能过度校正丢失生物差异。留空用默认"),
  save_to_disk = list(type = "boolean", default = FALSE,
                      description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  dataLists <- sc_get_dataLists(project)

  # 单批次：无需整合
  if (length(dataLists) <= 1) {
    # 把 merged_seurat 直接复制为 integrated_seurat
    merged <- if (!is.null(project$data[["merged_seurat"]])) {
      sc_load_from_disk(project, "merged_seurat")
    } else if (!is.null(project$data[["sct_seurat"]])) {
      sc_load_from_disk(project, "sct_seurat")
    } else {
      stop("请先运行标准化(sc_normalize 或 sc_sctransform)")
    }
    project$data[["integrated_seurat"]] <- merged
    if (inputs$save_to_disk) sc_save_to_disk(project, "integrated_seurat")
    return(list(data = list(), messages = "单批次数据，无需整合，已直接传递",
                method_info = record_method("sc_integrate", "Harmony整合", "跳过(单批次)", "harmony", inputs)))
  }

  # 加载合并后的 Seurat
  if (inputs$sctransform) {
    if (is.null(project$data[["sct_seurat"]]))
      stop("请先运行 sc_sctransform")
    merged <- sc_load_from_disk(project, "sct_seurat")
  } else {
    if (is.null(project$data[["merged_seurat"]]))
      stop("请先运行 sc_normalize")
    merged <- sc_load_from_disk(project, "merged_seurat")
  }

  res <- engine_sc_integrate(
    merged_seurat = merged,
    sctransform = inputs$sctransform,
    theta = inputs$theta
  )

  project$data[["integrated_seurat"]] <- res$seurat

  if (inputs$save_to_disk) {
    sc_save_to_disk(project, "integrated_seurat")
  }

  list(
    data = list(),
    messages = res$note,
    method_info = record_method("sc_integrate", "Harmony整合",
                                "harmony::RunHarmony", "harmony", inputs)
  )
}

TOOL_OUTPUTS <- list(plot = NULL, table = NULL)
