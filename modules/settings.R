# 设置模块

settings_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      card_header("分析参数"),
      selectInput(ns("set_norm"), "归一化", choices = c("log2", "log10", "None")),
      numericInput(ns("set_pval"), "p值阈值", value = 0.05, min = 0),
      numericInput(ns("set_logfc"), "logFC 阈值", value = 1, min = 0),
      selectInput(ns("dup"), "去重策略", choices = c("max", "mean", "kmax"))
    ),
    card(
      card_header("系统设置"),
      textInput(ns("work_dir"), "工作目录", value = getwd()),
      numericInput(ns("set_seed"), "随机种子", value = 42)
    ),
    col_widths = c(6, 6)
  )
}

settings_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    observe({
      state$settings$norm_method <- input$set_norm
      state$settings$pval_cut <- input$set_pval
      state$settings$logfc_cut <- input$set_logfc
      state$settings$seed <- input$set_seed
      state$settings$work_dir <- input$work_dir
      state$settings$dup <- input$dup
    })
  })
}