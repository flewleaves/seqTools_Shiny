# utils/config.R

CONFIG_FILE <- "seqTools_config.ini"

# 默认配置
default_config <- function(work_dir = getwd()) {
  list(
    ai = list(
      provider = "deepseek",
      model = "deepseek-chat",
      key = "",
      base_url = ""
    ),
    analysis = list(
      norm_method = "log2",
      pval_cut = 0.05,
      logfc_cut = 1,
      seed = 42
    ),
    system = list(
      work_dir = work_dir
    )
  )
}

# 读取配置
load_config <- function(work_dir = getwd()) {
  path <- file.path(work_dir, CONFIG_FILE)
  
  if (file.exists(path)) {
    configr::read.config(path)
  } else {
    default_config(work_dir)
  }
}

# 保存配置
save_config <- function(config, work_dir = getwd()) {
  path <- file.path(work_dir, CONFIG_FILE)
  configr::write.config(config, file.path = path)
}