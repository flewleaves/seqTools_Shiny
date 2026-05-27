# utils/state.R
# 全局状态：reactiveValues 仅作 UI 渲染缓存，唯一状态源为 engine$project (R6)

create_state <- function(config) {
  reactiveValues(
    name = NULL,
    omics_type = NULL,
    meta = list(),
    data = list(),
    res = list(),
    settings = config
  )
}