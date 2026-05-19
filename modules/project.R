# 项目管理模块：新建 + 加载

project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      card(
        card_header("新建项目"),
        textInput(ns("new_name"), "项目名称"),
        actionButton(ns("btn_new"), "创建", class = "btn-success")
      ),
      card(
        card_header("加载项目"),
        selectInput(ns("rds_select"), "项目文件", choices = NULL),
        actionButton(ns("btn_load"), "加载", class = "btn-primary")
      ),
      col_widths = c(6, 6)
    ),
    card(
      card_header("状态"),
      verbatimTextOutput(ns("status"))
    )
  )
}

project_server <- function(id, state, nav_session) {
  moduleServer(id, function(input, output, session) {
    
    # 刷新文件列表
    observe({
      req(state$settings$work_dir)
      wd <- state$settings$work_dir
      if (!dir.exists(wd)) return()
      files <- list.files(wd, pattern = "\\.rds$", full.names = FALSE)
      updateSelectInput(session, "rds_select", choices = files)
    })
    
    # 创建
    observeEvent(input$btn_new, {
      req(input$new_name)
      state$name <- input$new_name
      state$raw_data <- NULL
      state$norm_data <- NULL
      state$pca <- NULL
      
      saveRDS(reactiveValuesToList(state),
              file.path(state$settings$work_dir, paste0(input$new_name, ".rds")))
      showNotification(paste("创建:", state$name), type = "message")
      nav_select("main_nav", selected = "数据预处理", session = nav_session)
    })
    
    # 加载
    observeEvent(input$btn_load, {
      req(input$rds_select)
      path <- file.path(state$settings$work_dir, input$rds_select)
      loaded <- readRDS(path)
      
      state$name <- loaded$name
      state$raw_data <- loaded$raw_data
      state$norm_data <- loaded$norm_data
      state$pca <- loaded$pca
      if (!is.null(loaded$settings)) state$settings <- loaded$settings
      
      showNotification(paste("加载:", state$name), type = "message")
    })
    
    output$status <- renderPrint({
      if (is.null(state$name)) cat("无项目\n")
      else {
        cat("项目:", state$name, "\n")
        cat("数据:", ifelse(is.null(state$raw_data), "未导入", paste(dim(state$raw_data), collapse = " x ")), "\n")
      }
    })
  })
}