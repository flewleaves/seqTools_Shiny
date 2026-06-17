# utils/state.R
# 全局状态：reactiveValues 仅作 UI 渲染缓存，唯一状态源为 engine$project (R6)

create_state <- function(config) {
  reactiveValues(
    name = NULL,
    omics_type = NULL,
    meta = list(),
    data = list(),       # 仅 do_run_tool / import 时赋值，不在轮询中触碰
    res = list(),        # 同上
    data_keys = "",      # 轻量快照，每次轮询更新
    res_keys = "",       # 同上
    settings = config
  )
}