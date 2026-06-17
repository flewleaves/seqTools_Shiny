# utils/config.R

CONFIG_FILE <- "seqTools_config.ini"

default_config <- function(work_dir = NULL) {
  if (is.null(work_dir)) {
    work_dir <- if (exists("APP_WORK_DIR")) get("APP_WORK_DIR") else getwd()
  }
  list(
    AI = list(
      provider = "deepseek",
      model = "deepseek-chat",
      key = "",
      base_url = "https://api.deepseek.com"
    ),
    analysis = list(
      norm_method = "log2",
      pval_cut = 0.05,
      logfc_cut = 1,
      seed = 42,
      dup = "mean"
    ),
    system = list(
      work_dir = work_dir  # 工作目录可变
    )
  )
}

# 读取配置
load_config <- function() {
  path <- file.path(APP_ROOT, CONFIG_FILE)
  if (!file.exists(path)) return(default_config())
  
  # 用 ini 包或手动解析，避开 configr 的 bug
  lines <- readLines(path, encoding = "UTF-8")
  config <- list()
  section <- NULL
  for (line in lines) {
    line <- trimws(line)
    if (grepl("^\\[.+\\]$", line)) {
      section <- sub("^\\[(.+)\\]$", "\\1", line)
      config[[section]] <- list()
    } else if (grepl("=", line) && !is.null(section) && !grepl("^[#;]", line)) {
      key <- trimws(sub("=.*", "", line))
      val <- trimws(sub("^[^=]+=", "", line))
      config[[section]][[key]] <- val
    }
  }
  config
}

save_config <- function(config) {
  path <- file.path(APP_ROOT, CONFIG_FILE)
  lines <- c()
  for (section in names(config)) {
    lines <- c(lines, paste0("[", section, "]"))
    for (key in names(config[[section]])) {
      lines <- c(lines, paste0(key, "=", config[[section]][[key]]))
    }
    lines <- c(lines, "")
  }
  writeLines(lines, path, useBytes = FALSE)
}