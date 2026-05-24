# 全局状态：所有模块共享这一个 reactiveValues
create_state <- function(config = default_config()) {
  reactiveValues(
    name = NULL,
    omics_type = NULL,
    meta = list(),
    data = list(),
    res = list(),
    settings = config
  )
}