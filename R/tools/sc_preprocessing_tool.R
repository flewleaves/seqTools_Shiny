# R/tools/sc_preprocessing_tool.R
# 单细胞预处理：过滤 + 去双细胞

TOOL_NAME <- "sc_preprocessing"
TOOL_CATEGORY <- "单细胞处理"
TOOL_DISPLAY_NAME <- "预处理(过滤+去双细胞)"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 10

TOOL_SCHEMA <- list(
  doublet_find = list(type = "boolean", default = TRUE,
                      description = "去除双细胞(doublet)"),
  filter_mode = list(type = "select",
                     choices = c("auto", "manual", "none"),
                     default = "auto",
                     description = "auto=sc_filter算法, manual=手动输入阈值, none=跳过(导入时已过滤)"),
  filter_nCount_lower  = list(type = "integer", default = 200, min = 0,
    description = "UMI 总数下限（仅 filter_mode=manual 时生效，auto 模式自动计算）"),
  filter_nCount_upper  = list(type = "integer", default = 50000, min = 0,
    description = "UMI 总数上限"),
  filter_nFeature_lower = list(type = "integer", default = 200, min = 0,
    description = "基因数下限（nFeature_RNA）"),
  filter_nFeature_upper = list(type = "integer", default = 10000, min = 0,
    description = "基因数上限"),
  filter_mt_upper       = list(type = "integer", default = 20, min = 0, max = 100,
    description = "线粒体基因比例上限（百分比，0-100）"),
  save_to_disk = list(type = "boolean", default = FALSE,
    description = "处理完后保存到硬盘释放内存")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)
  require(Seurat)

  if (!sc_has_dataLists(project)) stop("无单细胞数据，请先导入")

  dataLists <- sc_get_dataLists(project)
  species <- project$meta$species %||% "Hs"

  # 构建 filter 向量
  filter_vec <- NULL
  if (inputs$filter_mode == "manual") {
    filter_vec <- c(
      inputs$filter_nCount_lower,
      inputs$filter_nCount_upper,
      inputs$filter_nFeature_lower,
      inputs$filter_nFeature_upper,
      inputs$filter_mt_upper
    )
  } else if (inputs$filter_mode == "none") {
    filter_vec <- NULL
  }
  # auto: filter_vec = NULL, scRNA_preprocessing 内部调用 sc_filter

  results <- list()
  dl_keys <- names(dataLists)

  for (i in seq_along(dataLists)) {
    key <- dl_keys[i]
    res <- engine_sc_preprocessing(
      dataLists[[key]],
      species = species,
      doublet_find = inputs$doublet_find,
      filter = filter_vec,
      samples = "orig.ident"
    )
    project$data[[key]] <- res$dataList
    results[[key]] <- res$note

    if (inputs$save_to_disk) {
      sc_save_to_disk(project, key)
    }
  }

  project$meta$sc_preprocessed <- TRUE

  list(
    data = list(),
    messages = unlist(results, use.names = FALSE),
    method_info = record_method("sc_preprocessing", "预处理(过滤+去双细胞)",
                                "Seurat::PercentageFeatureSet + scDblFinder", "Seurat", inputs)
  )
}

TOOL_OUTPUTS <- list(plot = NULL, table = NULL)
