source("global.R")

ui <- page_navbar(
  id = "main_nav",
  title = "OmicsVis",
  nav_panel("项目管理", project_ui("proj")),
  nav_panel("数据预处理", data_ui("data")),
  nav_panel("分析", analysis_ui("anal")),
  nav_panel("设置", settings_ui("set")),
  nav_spacer(),
  nav_item(input_dark_mode())
)

server <- function(input, output, session) {
  state <- create_state()
  
  project_server("proj", state, nav_session = session)
  data_server("data", state, nav_session = session)
  analysis_server("anal", state)
  settings_server("set", state)
}

shinyApp(ui, server)