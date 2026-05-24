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

project_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    # 刷新文件列表：关键修复。observeEvent + ignoreInit = TRUE，只在 state$settings 真正变化时执行
    observeEvent(state$settings, {
      wd <- state$settings$system$work_dir
      if (is.null(wd) || wd == "" || !dir.exists(wd)) {
        return()
      }
      
      files <- list.files(wd, pattern = "\\.rds$", full.names = FALSE)
      updateSelectInput(session, "rds_select", choices = files)
    }, ignoreInit = TRUE)
    
    # 创建项目
    observeEvent(input$btn_new, {
      req(input$new_name)
      
      wd <- state$settings$system$work_dir
      
      state$name <- input$new_name
      state$raw_data <- NULL
      state$count <- NULL
      state$norm_data <- NULL
      state$pca <- NULL
      
      saveRDS(reactiveValuesToList(state), file.path(wd, paste0(input$new_name, ".rds")))
      showNotification(paste("创建:", state$name), type = "message")
      
      files <- list.files(wd, pattern = "\\.rds$", full.names = FALSE)
      updateSelectInput(session, "rds_select", choices = files, selected = paste0(input$new_name, ".rds"))
    })
    
    # 加载项目
    observeEvent(input$btn_load, {
      req(input$rds_select)
      
      wd <- state$settings$system$work_dir
      path <- file.path(wd, input$rds_select)
      
      if (!file.exists(path)) {
        showNotification("文件不存在", type = "error")
        return()
      }
      
      loaded <- readRDS(path)
      state$name <- loaded$name
      state$raw_data <- loaded$raw_data
      state$count <- loaded$count
      state$norm_data <- loaded$norm_data
      state$pca <- loaded$pca
      if (!is.null(loaded$meta)) state$meta <- loaded$meta
      
      showNotification(paste("加载:", state$name), type = "message")
    })
    
    output$status <- renderPrint({
      if (is.null(state$name)) cat("无项目\n")
      else {
        cat("项目:", state$name, "\n")
        cat("数据:", ifelse(is.null(state$count), "未导入", paste(dim(state$count), collapse = " x ")), "\n")
      }
    })
  })
}