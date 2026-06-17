# R/imports/custom_table_import.R
# 自定义表格导入：读 CSV/TXT，按自定义名称存入 project$data

IMPORT_ID   <- "custom_table"
IMPORT_NAME <- "自定义表格"

IMPORT_SCHEMA <- list(
  file = list(type = "file", required = TRUE,
              description = "CSV 或 TXT 表格文件"),
  name = list(type = "text", required = TRUE,
              description = "数据名称（用于 project$data 中的 key）")
)

IMPORT_RUN <- function(file_path, params) {
  name <- params$name %||% "custom_data"
  if (!nzchar(name)) name <- "custom_data"

  # 读取表格
  ext <- tolower(tools::file_ext(file_path))
  if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    df <- read.table(file_path, header = TRUE, sep = "\t",
                     stringsAsFactors = FALSE, check.names = FALSE)
  }

  if (!is.data.frame(df) || nrow(df) == 0)
    stop("文件为空或无法解析为表格")

  rownames(df) <- make.unique(as.character(df[[1]]))
  df <- df[, -1, drop = FALSE]

  list(
    data = setNames(list(df), name),
    meta = list(filename = basename(file_path)),
    omics_type = NULL
  )
}
