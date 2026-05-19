# 全局状态：所有模块共享这一个 reactiveValues
create_state <- function() {
  reactiveValues(
    name = NULL,
    meta = list(),
    data = list(),
    gene.length = NULL,
    pca = NULL,
    deg_res = NULL,
    settings = list(
      work_dir = getwd(),
      norm_method = "log2",
      pval_cut = 0.05,
      logfc_cut = 1,
      seed = 42
    )
  )
}