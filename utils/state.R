# 全局状态：所有模块共享这一个 reactiveValues
create_state <- function() {
  reactiveValues(
    name = NULL,
    omics_type = NULL,
    meta = list(),
    data = list(),
    res = list(),
    settings = list(
      work_dir = getwd(),
      norm_method = "log2",
      pval_cut = 0.05,
      logfc_cut = 1,
      seed = 42
    )
  )
}