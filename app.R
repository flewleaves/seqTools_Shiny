# app.R
# global.R 已完成所有 source，此处仅保留 shinyApp 入口
source("global.R")
ui <- page_navbar(
  title = "seqTools Shiny",
  id = "main_nav",
  nav_panel("项目", project_ui("project")),
  nav_panel("数据", data_ui("data")),
  nav_panel("分析", analysis_ui("analysis")),
  nav_panel("AI 助手", ai_console_ui("ai")),
  nav_panel("设置", settings_ui("settings"))
)

server <- function(input, output, session) {
  config <- load_config()
  state <- create_state(config)
  engine$project$settings <- config$analysis

  project_server("project", state)
  data_server("data", state, session)
  analysis_server("analysis", state)
  ai_console_server("ai", state)
  settings_server("settings", state)

  # 双向同步：R6 Project → Shiny reactiveValues（供 UI 渲染）
  # 只同步轻量的 keys + meta，不复制大矩阵——避免每 5s 触发 GC 和响应式级联
  observe({
    invalidateLater(5000)
    isolate({
      r6_name  <- engine$project$name
      r6_omics <- engine$project$omics_type
      r6_meta  <- engine$project$meta

      new_name <- state$name %||% r6_name
      if (!identical(state$name, new_name)) state$name <- new_name

      # 只同步名字快照，不碰 data/res 本体
      data_key <- paste(names(engine$project$data), collapse = ",")
      if (!identical(state$data_keys, data_key))
        state$data_keys <- data_key

      res_key <- paste(names(engine$project$results), collapse = ",")
      if (!identical(state$res_keys, res_key))
        state$res_keys <- res_key

      state$meta <- r6_meta

      if (!identical(state$omics_type, r6_omics))
        state$omics_type <- r6_omics
    })
  })
}

shinyApp(ui, server)