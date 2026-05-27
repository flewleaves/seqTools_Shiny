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
  observe({
    invalidateLater(2000)
    state$name       <- state$name %||% engine$project$name
    state$data       <- engine$project$data
    state$res        <- engine$project$results
    state$meta       <- engine$project$meta
    state$omics_type <- engine$project$omics_type
  })
}

shinyApp(ui, server)