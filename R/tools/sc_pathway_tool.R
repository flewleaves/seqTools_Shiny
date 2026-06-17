# R/tools/sc_pathway_tool.R
# 单细胞通路活性打分 — seqTools::score_geneset_activity (AUCell / ModuleScore)

TOOL_NAME <- "sc_pathway"
TOOL_CATEGORY <- "数据分析"
TOOL_DISPLAY_NAME <- "通路活性打分"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 39

TOOL_SCHEMA <- list(
  geneset_file = list(type = "file", required = TRUE,
    description = "基因集文件，点击上传。支持 CSV（表头 term,gene）或 GMT 格式（每行 tab 分隔: 通路名\\t描述\\t基因1\\t基因2...）"),
  method = list(type = "select", required = TRUE,
    choices = c("AUCell","ModuleScore"), default = "AUCell",
    description = "评分方法：AUCell（需安装 AUCell 包）或 ModuleScore（Seurat 内置）")
)

# 解析基因集文件（CSV 或 GMT）
parse_geneset_file <- function(filepath) {
  first <- readLines(filepath, n = 1, warn = FALSE)
  # 判断格式：CSV 有 "term,gene" 头，GMT 无头且 tab 分隔
  if (grepl("^term[\t,]gene", first, ignore.case = TRUE)) {
    # CSV 格式
    df <- read.csv(filepath, stringsAsFactors = FALSE, header = TRUE)
    if (!all(c("term", "gene") %in% colnames(df)))
      stop("CSV 文件必须包含 term 和 gene 两列")
    return(df[, c("term", "gene")])
  }
  # GMT 格式：每行 term\tdescription\tgene1\tgene2...
  lines <- readLines(filepath, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) stop("文件为空")
  result <- do.call(rbind, lapply(lines, function(line) {
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) < 3) return(NULL)
    term <- parts[1]
    genes <- parts[3:length(parts)]
    genes <- genes[nzchar(genes)]
    if (length(genes) == 0) return(NULL)
    data.frame(term = term, gene = genes, stringsAsFactors = FALSE)
  }))
  if (is.null(result) || nrow(result) == 0)
    stop("无法从 GMT 文件中解析出有效的基因集")
  return(result)
}

TOOL_RUN <- function(inputs, project) {
  require(seqTools)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  filepath <- inputs$geneset_file
  if (is.null(filepath) || !file.exists(filepath))
    stop("基因集文件不存在")

  geneSets <- parse_geneset_file(filepath)
  n_terms <- length(unique(geneSets$term))
  message("[sc_pathway] 解析到 ", n_terms, " 个基因集，共 ", nrow(geneSets), " 条基因记录")

  method <- inputs$method %||% "AUCell"
  old_cols <- colnames(seurat_obj@meta.data)

  scRNA <- seqTools::score_geneset_activity(
    scRNA    = seurat_obj,
    geneSets = geneSets,
    method   = method
  )

  # 保存更新后的 Seurat 对象
  sc_save_to_disk(project, key)
  # 更新缓存的 meta.data 列名
  project$meta$sc_meta_cols <- as.character(colnames(scRNA@meta.data))

  new_cols <- setdiff(colnames(scRNA@meta.data), old_cols)
  msg <- paste("通路活性打分完成 (", method, "):", n_terms, "个基因集",
               if (length(new_cols) > 0) paste0("\n新增列: ", paste(new_cols, collapse = ", ")))

  list(data = list(),
       messages = msg,
       method_info = record_method("sc_pathway", "通路活性打分",
         paste("seqTools::score_geneset_activity (", method, ")", sep = ""),
         "seqTools", inputs))
}

TOOL_OUTPUTS <- list(
  plot = NULL,
  table = NULL
)
