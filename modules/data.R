# modules/data.R

data_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "读取数据",
      width = 320,
      radioGroupButtons(
        inputId = ns("import_method"), label = "选择组学类型",
        choices = c("加载中..." = ""), selected = "", status = "primary"
      ),
      hr(),
      uiOutput(ns("method_ui")),
      hr(),
      sliderInput(ns("page_size"), "每页行数", min = 5, max = 100, value = 10),
      actionButton(ns("confirm_input"), "确认导入", class = "btn-info")
    ),
    card(
      full_screen = TRUE,
      card_header("数据预览"),
      verbatimTextOutput(ns("debug_info")),
      uiOutput(ns("main_view"))
    )
  )
}

data_server <- function(id, state, nav_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$sc_browse_dir, {
      ps1 <- tempfile(fileext = ".ps1")
      writeLines(c(
        'Add-Type -AssemblyName System.Windows.Forms',
        '$d = New-Object System.Windows.Forms.FolderBrowserDialog',
        '$d.Description = "Select Data Directory"',
        paste0('$d.SelectedPath = "', normalizePath(".", mustWork = FALSE), '"'),
        '$d.ShowNewFolderButton = $true',
        'if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {',
        '    Write-Output $d.SelectedPath',
        '}'
      ), ps1)
      result <- tryCatch(
        system2("powershell",
                args = c("-NoProfile", "-STA", "-ExecutionPolicy", "Bypass", "-File", ps1),
                stdout = TRUE, stderr = FALSE, wait = TRUE),
        error = function(e) character(0)
      )
      if (length(result) > 0 && nzchar(trimws(result[1]))) {
        updateTextInput(session, "root_dir",
                       value = normalizePath(trimws(result[1]), winslash = "/", mustWork = FALSE))
      }
    }, ignoreInit = TRUE)

    # 临时存文件上传后读取的原始 df（用于列选择 modal）
    temp_df   <- reactiveVal(NULL)
    temp_cols <- reactiveVal(list())

    # 单细胞导入：扫描结果 + 过滤状态（按 batch 管理）
    sc_scan          <- reactiveVal(NULL)
    sc_root          <- reactiveVal(NULL)
    sc_filter_status <- reactiveVal(list())   # batch_key -> "pending" | "done"
    sc_filter_params <- reactiveVal(list())   # batch_key -> list(mode, thresholds)

    # 获取所有 SC batch 及其样本
    get_sc_batches <- function() {
      batches <- list()
      for (key in sort(names(engine$project$data))) {
        if (grepl("^dataList", key)) {
          dl <- engine$project$data[[key]]
          if (is.character(dl)) dl <- readRDS(dl)
          if (is.list(dl)) {
            batch_samples <- list()
            for (nm in names(dl)) {
              if (inherits(dl[[nm]], "Seurat")) batch_samples[[nm]] <- dl[[nm]]
            }
            if (length(batch_samples) > 0) {
              batches[[key]] <- list(
                batch_name = engine$project$meta$sc_batch_info[[key]]$batch_name %||% key,
                samples = batch_samples
              )
            }
          }
        }
      }
      batches
    }

    # 获取所有 SC 样本（扁平）
    get_all_sc_samples <- function() {
      samples <- list()
      for (bk in names(get_sc_batches())) {
        for (nm in names(get_sc_batches()[[bk]]$samples)) {
          samples[[nm]] <- get_sc_batches()[[bk]]$samples[[nm]]
        }
      }
      samples
    }

    # 计算 batch 共享 filter（同 seqTools get_filter_params 逻辑）
    get_batch_filter <- function(samples, species) {
      fv_list <- lapply(samples, function(s) seqTools::sc_filter(s, species))
      fv_mat <- do.call(rbind, fv_list)
      lower_cols <- c("nCount_lower", "nFeature_lower")
      upper_cols <- c("nCount_upper", "nFeature_upper", "mt_upper")
      filter <- numeric(5)
      names(filter) <- names(fv_list[[1]])
      filter[lower_cols] <- apply(fv_mat[, lower_cols, drop = FALSE], 2, max)
      filter[upper_cols] <- apply(fv_mat[, upper_cols, drop = FALSE], 2, min)
      filter
    }

    # 加载项目时清空过滤状态
    observeEvent(state$name, {
      sc_filter_status(list())
      sc_filter_params(list())
    }, ignoreInit = TRUE)

    # 同步 SC 过滤状态（按 batch，也处理 pre-analyzed clustered_seurat）
    observe({
      force(state$data)
      omics <- state$omics_type
      batches <- get_sc_batches()
      status  <- sc_filter_status()

      if (is.null(omics) || omics != "single_cell") return()

      # pre-analyzed 直接导入 → 不需要 batch QC
      if (length(batches) == 0 && !is.null(engine$project$data[["clustered_seurat"]])) {
        if (length(sc_filter_status()) == 0)
          sc_filter_status(setNames(list("done"), "clustered_seurat"))
        return()
      }
      if (length(batches) == 0) return()

      need_init <- length(status) == 0

      if (need_init) {
        already_filtered <- isTRUE(engine$project$meta$sc_filtered)
        new_status <- setNames(
          rep(if (already_filtered) "done" else "pending", length(batches)),
          names(batches)
        )
        sc_filter_status(new_status)
        if (length(sc_filter_params()) == 0) sc_filter_params(list())
      }
    })

    # 统计过滤状态
    sc_all_filtered <- reactive({
      status <- sc_filter_status()
      samples <- get_all_sc_samples()
      if (length(samples) == 0) return(FALSE)
      all(names(samples) %in% names(status)) &&
        all(unlist(status) == "done")
    })

    # ---------- 注册表同步（只在初始化时设置一次，不抢用户选择） ----------
    observe({
      methods <- ImportRegistry$list_methods()
      if (length(methods) > 0) {
        choices <- setNames(
          sapply(methods, `[[`, "id"),
          sapply(methods, `[[`, "name")
        )
        # 只在 choices 变化时更新，且保留用户当前选择
        cur <- isolate(input$import_method)
        if (!identical(cur, "") && cur %in% choices) {
          updateRadioGroupButtons(session, "import_method",
                                  choices = choices, selected = cur)
        } else {
          updateRadioGroupButtons(session, "import_method",
                                  choices = choices, selected = choices[1])
        }
      }
    })

    current_method <- reactive({
      req(input$import_method, input$import_method != "")
      ImportRegistry$get(input$import_method)
    })

    # ---------- 动态 UI（schema → 控件），bulk_rna 特殊处理 ----------
    output$method_ui <- renderUI({
      req(current_method())
      mid <- input$import_method

      if (mid == "bulk_rna") {
        # bulk_rna 的列选择在 modal 里做，这里只显示元数据控件
        tagList(
          helpText("请上传基因表达矩阵，行为基因，列为样本。"),
          fileInput(ns("infile"), "选择 CSV/TXT", accept = c(".csv", ".txt")),
          hr(),
          selectInput(ns("data_type"), "数据类型",
                      choices = c("auto" = "auto", "count","tpm","fpkm","cpm"), selected = "auto"),
          selectInput(ns("log_state"), "是否log化",
                      choices = c("自动检测" = "auto", "是" = "TRUE", "否" = "FALSE"), selected = "auto"),
          selectInput(ns("species"), "物种",
                      choices = c("自动检测" = "auto", "Human" = "Hs", "Mouse" = "Mm"), selected = "auto"),
          selectInput(ns("id_type"), "基因ID类型",
                      choices = c("自动检测" = "auto", "SYMBOL","ENSEMBL","ENTREZID"), selected = "auto"),
          textAreaInput(ns("group_info"), "分组信息",
                        placeholder = "组名,样本数;...  例: Control,3;Treat,3", rows = 3),
          hr(),
          actionButton(ns("id_convert"), "手动ID转换为SYMBOL", class = "btn-outline-info btn-sm")
        )
      } else if (mid == 'single_cell') {
        scan <- sc_scan()
        wd <- state$settings$system$work_dir %||%
              (if (exists("APP_WORK_DIR", inherits = TRUE)) APP_WORK_DIR else getwd())
        scan_block <- if (!is.null(scan)) tagList(
          h6(paste0('发现 ', scan$n_total, ' 个样本 (', scan$method, ')，',
                    length(scan$batches), ' 个批次')),
          DT::dataTableOutput(ns('sc_sample_table')),
          selectInput(ns('sc_species'), '物种',
                      choices = c('Human' = 'Hs', 'Mouse' = 'Mm'),
                      selected = 'Hs')
        )
        tagList(
          fileInput(ns('sc_files'), '选择单细胞文件 (.rds/.h5/.txt/.csv)',
                    accept = c('.rds','.h5','.txt','.csv','.tsv'),
                    multiple = TRUE),
          helpText('或输入 10X 数据目录路径扫描：'),
          div(style = "display:flex; gap:6px;",
            textInput(ns('root_dir'), '10X 数据根目录', value = wd,
                      placeholder = 'D:/data/scRNA_seq/GSE123456') |> tagAppendAttributes(style = "flex:1; margin-bottom:0;"),
            actionButton(ns('sc_browse_dir'), '浏览...', class = 'btn-outline-secondary btn-sm')
          ),
          actionButton(ns('scan_dir'), '🔍 扫描目录', class = 'btn-outline-primary'),
          scan_block
        )
      } else {
        # 其他导入方法走通用 schema_to_ui
        schema <- current_method()$schema
        if (length(schema) == 0) return(NULL)
        schema_to_ui(schema, ns, state)
      }
    })

    # ================================================================
    #  bulk_rna：上传文件后弹 modal 选列
    # ================================================================
    observeEvent(input$infile, {
      req(input$import_method == "bulk_rna", input$infile)
      if (is.null(state$name)) {
        showNotification("请先创建项目", type = "error", duration = NULL); return()
      }
      showNotification("读取中...", type = "message", duration = 2)
      tryCatch({
        df <- data.table::fread(input$infile$datapath, header = TRUE, data.table = FALSE)
        df <- na.omit(df)
        temp_df(df)

        showModal(modalDialog(
          title = "确认数据结构", size = "l",
          p("请指定基因ID列和样本列。"),
          selectInput(ns("gene_col"), "基因ID列",
                      choices = names(df), selected = names(df)[1]),
          selectInput(ns("gene_length_col"), "基因长度列（可选，用于 RPKM/FPKM）",
                      choices = c("无" = "", names(df)), selected = ""),
          checkboxGroupInput(ns("sample_col"), "样本列（可多选）",
                             choices = names(df), selected = names(df)[-1]),
          footer = tagList(
            actionButton(ns("cancel_cols"), "取消"),
            actionButton(ns("confirm_cols"), "确认", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error", duration = NULL)
      })
    })

    # 用户取消列选择
    observeEvent(input$cancel_cols, {
      temp_df(NULL); temp_cols(list()); removeModal()
    })

    # 用户确认列选择 → 自动检测数据属性，更新 UI
    observeEvent(input$confirm_cols, {
      req(input$import_method == "bulk_rna")
      df <- temp_df()
      req(df)

      gene_col    <- input$gene_col
      sample_cols <- input$sample_col
      gene_length <- input$gene_length_col

      # 只保留选中列
      df_sel <- df[, c(gene_col, sample_cols), drop = FALSE]
      temp_df(df_sel)
      temp_cols(list(gene_col = gene_col, sample_cols = sample_cols,
                     gene_length = gene_length))

      # 自动检测
      meta <- detect_data(df_sel, list(filename = input$infile$name))
      for (field in c("data_type", "log_state", "species", "id_type")) {
        val <- meta[[field]]
        if (!is.null(val)) {
          updateSelectInput(session, field, selected = as.character(val))
        }
      }
      removeModal()
      showNotification(
        paste0("已选 ", nrow(df_sel), " 行 × ", length(sample_cols), " 个样本"),
        type = "message", duration = 4
      )

      # 若检测到非 SYMBOL，提示转换
      if (!is.null(meta$id_type) && meta$id_type != "SYMBOL") {
        shinyalert(
          title = "ID 转换提示",
          text  = paste0("检测到基因ID为 ", meta$id_type, "，是否在导入时转换为 SYMBOL？"),
          type  = "info",
          showCancelButton  = TRUE,
          confirmButtonText = "是，导入时转换",
          cancelButtonText  = "否，保持原样",
          callbackR = function(value) {
            if (value) updateSelectInput(session, "id_type", selected = meta$id_type)
            # 实际转换在 confirm_input 里根据 id_type != SYMBOL 决定
          }
        )
      }
    })

    # 手动 ID 转换按钮（立即对 temp_df 执行）
    observeEvent(input$id_convert, {
      req(input$import_method == "bulk_rna")
      df <- temp_df(); req(df)
      tryCatch({
        colnames(df)[1] <- "Geneid"
        df <- seqTools::Quick_ID_conversion(
          df, species = input$species, from = input$id_type, to = "SYMBOL"
        )
        temp_df(df)
        updateSelectInput(session, "id_type", selected = "SYMBOL")
        showNotification("ID 转换完成", type = "message")
      }, error = function(e) {
        showNotification(paste("转换失败:", e$message), type = "error", duration = NULL)
      })
    })

    # ================================================================
    #  single_cell：直接导入选中文件 (rds/h5/txt/csv)
    # ================================================================
    observeEvent(input$sc_files, {
      req(state$name)
      files <- input$sc_files
      if (is.null(files) || nrow(files) == 0) return()

      species <- input$sc_species %||% "Hs"
      mt_pattern <- if (toupper(species) == "MM") "^mt-" else "^MT-"

      dataList <- list()
      for (i in seq_len(nrow(files))) {
        fname  <- files$name[i]
        fpath  <- files$datapath[i]
        ext    <- tolower(tools::file_ext(fname))
        sname  <- tools::file_path_sans_ext(fname)

        sobj <- tryCatch({
          if (ext == "rds") {
            obj <- readRDS(fpath)
            obj <- sc_update_seurat(obj)
            if (!inherits(obj, "Seurat")) stop("不是 Seurat 对象")
            if (!"percent.mt" %in% colnames(obj@meta.data)) {
              obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = mt_pattern)
            }
            obj
          } else if (ext == "h5") {
            counts <- Seurat::Read10X_h5(filename = fpath)
            obj <- CreateSeuratObject(counts, project = sname, min.cells = 3, min.features = 200)
            obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = mt_pattern)
            obj
          } else {
            # txt/csv/tsv count matrix → 保持 sparse，避免 dense 矩阵耗尽内存
            if (ext == "csv") {
              raw <- data.table::fread(fpath, header = TRUE, data.table = FALSE)
            } else {
              raw <- data.table::fread(fpath, header = TRUE, data.table = FALSE,
                                       sep = "\t")
            }
            genes <- raw[[1]]
            mat <- as(as.matrix(raw[, -1, drop = FALSE]), "dgCMatrix")
            rownames(mat) <- make.unique(as.character(genes))
            rm(raw); gc()
            obj <- CreateSeuratObject(mat, project = sname, min.cells = 3, min.features = 200)
            obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = mt_pattern)
            obj
          }
        }, error = function(e) {
          showNotification(paste("读取失败:", fname, "-", e$message), type = "error", duration = NULL)
          NULL
        })

        if (!is.null(sobj)) {
          sobj@misc$species <- species
          dataList[[sname]] <- sobj
        }
      }

      if (length(dataList) == 0) return()

      # 自动命名：如果项目还是默认名，用第一个文件/样本名
      if (is.null(state$name) || state$name == "未命名" || !nzchar(state$name)) {
        first_name <- names(dataList)[1] %||% files$name[1] %||% "sc_project"
        state$name <- gsub("\\.[^.]+$", "", first_name)
        engine$project$name <- state$name
      }

      # 检测是否已分析过的 Seurat 对象（有关键 reductions 或 cell_type）
      pre_analyzed <- any(sapply(dataList, function(s) {
        length(intersect(c("umap","tsne","pca"), names(s@reductions))) >= 1 &&
          ("seurat_clusters" %in% colnames(s@meta.data) ||
           "cell_type" %in% colnames(s@meta.data))
      }))

      if (pre_analyzed) {
        # 直接解包存入已知 key，不经过 QC
        obj <- dataList[[1]]
        Idents(obj) <- if ("cell_type" %in% colnames(obj@meta.data)) "cell_type"
                       else "seurat_clusters"
        if (!"seurat_clusters" %in% colnames(obj@meta.data))
          obj$seurat_clusters <- as.character(Idents(obj))
        engine$project$data[["clustered_seurat"]] <- obj
        engine$project$meta$species <- species
        engine$project$meta$sc_filtered <- TRUE
        engine$project$omics_type <- "single_cell"

        state$data       <- engine$project$data
        state$data_keys  <- paste(names(engine$project$data), collapse = ",")
        state$meta       <- engine$project$meta
        state$omics_type <- engine$project$omics_type

        # 标记为已过滤
        sc_filter_status(setNames(list("done"), "clustered_seurat"))

        showNotification(
          paste0("导入已分析数据: ", ncol(obj), " 个细胞, ",
                 length(unique(Idents(obj))), " 个 cluster，QC 已跳过"),
          type = "message", duration = 8)

        later::later(~ push_state_to_python(), 10)
      } else {
        # 存入 project$data
        slot_name <- paste0("dataList", length(grep("^dataList", names(engine$project$data))) + 1)
        engine$project$data[[slot_name]] <- dataList

        batch_info <- list(list(
          batch_name = "Batch1",
          samples    = names(dataList),
          n_cells    = sapply(dataList, ncol)
        ))
        names(batch_info) <- slot_name
        engine$project$meta$sc_batch_info <- c(
          engine$project$meta$sc_batch_info %||% list(), batch_info)
        engine$project$meta$species   <- species
        engine$project$meta$sc_filtered <- FALSE
        engine$project$omics_type <- "single_cell"

        state$data       <- engine$project$data
        state$data_keys  <- paste(names(engine$project$data), collapse = ",")
        state$meta       <- engine$project$meta
        state$omics_type <- engine$project$omics_type

        # 初始化过滤状态（按 batch key）
        old_status <- sc_filter_status()
        new_status <- setNames(list(pending = "pending"), slot_name)
        for (nm in names(old_status)) {
          if (nm %in% names(new_status)) new_status[[nm]] <- old_status[[nm]]
        }
        sc_filter_status(new_status)

        n_cells <- sum(sapply(dataList, ncol))
        showNotification(
          paste0("导入 ", length(dataList), " 个样本, ", n_cells, " 个细胞"),
          type = "message", duration = 5)

        later::later(~ push_state_to_python(), 10)
      }
    })

    # ================================================================
    #  single_cell：扫描目录
    # ================================================================
    observeEvent(input$scan_dir, {
      root <- input$root_dir
      if (is.null(root) || !nzchar(root)) {
        showNotification("请先输入或浏览选择数据根目录", type = "warning"); return()
      }
      if (!dir.exists(root)) {
        showNotification(paste("目录不存在:", root), type = "error"); return()
      }
      showNotification("扫描中...", type = "message", duration = 2)
      tryCatch({
        result <- sc_scan_directory(root)
        sc_scan(result)
        sc_root(root)
        showNotification(
          paste0("发现 ", result$n_total, " 个样本 (", result$method, "), ",
                 length(result$batches), " 个批次"),
          type = "message", duration = 8
        )
      }, error = function(e) {
        showNotification(paste("扫描失败:", e$message), type = "error", duration = NULL)
      })
    })

    output$sc_sample_table <- DT::renderDataTable({
      scan <- sc_scan(); req(scan)
      df <- data.frame(
        Sample = sapply(scan$samples, `[[`, "name"),
        Type   = sapply(scan$samples, `[[`, "type"),
        Batch  = sapply(scan$samples, `[[`, "batch"),
        stringsAsFactors = FALSE
      )
      DT::datatable(df,
        options = list(pageLength = 20, dom = 'tip', scrollY = "300px"),
        rownames = FALSE)
    })

    # ================================================================
    #  通用确认导入
    # ================================================================
    observeEvent(input$confirm_input, {
      req(state$name, input$import_method != "")
      method <- current_method(); req(method)
      mid <- input$import_method

      if (mid == "bulk_rna") {
        # bulk_rna：必须已选列
        df <- temp_df()
        if (is.null(df)) {
          showNotification("请先上传文件并确认数据结构", type = "error"); return()
        }
        cols <- temp_cols()
        group_vec <- parse_group_info(input$group_info %||% "")
        n_samples  <- length(cols$sample_cols %||% (ncol(df) - 1))
        if (!is.null(group_vec) && length(group_vec) != n_samples) {
          showNotification(paste0("分组数(", length(group_vec),
                                  ") 与样本数(", n_samples, ") 不匹配"),
                           type = "error", duration = NULL)
          return()
        }

        # 去重策略：优先用 settings
        dup_from_settings <- tryCatch(
          state$settings$analysis$dup %||% "kmax",
          error = function(e) "kmax"
        )

        params <- list(
          gene_col          = cols$gene_col    %||% "",
          sample_cols       = paste(cols$sample_cols %||% "", collapse = ","),
          gene_length       = cols$gene_length %||% "",
          data_type         = input$data_type  %||% "auto",
          log_state         = input$log_state  %||% "auto",
          species           = input$species    %||% "auto",
          id_type           = input$id_type    %||% "auto",
          id_convert        = if (!is.null(input$id_type) && input$id_type != "SYMBOL" &&
                                  input$id_type != "auto") "to_SYMBOL" else "no",
          dup_method        = "settings",
          dup_from_settings = dup_from_settings,
          group_info        = input$group_info %||% ""
        )

        # 把已预处理的 temp_df 写成临时文件，让 IMPORT_RUN 读取
        # （这样 AI 和 UI 都走同一个 IMPORT_RUN 入口，架构一致）
        tmp <- tempfile(fileext = ".csv")
        data.table::fwrite(cbind(rowname__ = rownames(df), df)
                           |> (\(x) { names(x)[1] <- cols$gene_col %||% names(df)[1]; x })(),
                           tmp)
        # 如果 temp_df 已经是纯数值（列选择后），直接 fwrite
        data.table::fwrite(data.frame(Gene = rownames(df), df, check.names = FALSE), tmp)

      } else if (mid == "single_cell") {
        scan <- sc_scan()
        if (is.null(scan)) {
          showNotification("请先扫描目录", type = "error"); return()
        }
        root <- sc_root()
        if (is.null(root) || !dir.exists(root)) {
          showNotification("目录不存在，请重新扫描", type = "error"); return()
        }

        batch_info <- lapply(scan$samples, function(s) s$batch)
        names(batch_info) <- sapply(scan$samples, `[[`, "name")

        params <- list(
          action    = "import",
          species   = input$sc_species %||% "Hs",
          batch_assign_json = jsonlite::toJSON(batch_info, auto_unbox = TRUE)
        )
        tmp <- root

      } else {
        # 其他方法：从 schema 收集 input
        inputs <- list(); file_path <- NULL
        for (pid in names(method$schema)) {
          pdef <- method$schema[[pid]]
          val  <- input[[pid]]
          if (pdef$type == "file") {
            if (is.list(val) && !is.null(val$datapath)) file_path <- val$datapath
          } else {
            inputs[[pid]] <- val
          }
        }
        if (is.null(file_path)) {
          showNotification("请先上传文件", type = "error"); return()
        }
        tmp    <- file_path
        params <- inputs
      }

      local_session <- session
      nid <- showNotification("导入中...", type = "message", duration = NULL,
                              session = local_session)
      later::later(function() {
        tryCatch({
          result <- engine$project$import_data(mid, tmp, params)
          if (!isTRUE(result$success) && !is.null(result$error)) stop(result$error)

          if (is.null(state$name) || state$name == "未命名" || !nzchar(state$name)) {
            nm <- if (mid == "single_cell") basename(tmp) else input$new_name
            if (!is.null(nm) && nzchar(nm)) { state$name <- nm; engine$project$name <- nm }
          }

          state$data       <- engine$project$data
          state$data_keys  <- paste(names(engine$project$data), collapse = ",")
          state$meta       <- engine$project$meta
          state$omics_type <- engine$project$omics_type
          temp_df(NULL); temp_cols(list())

          removeNotification(nid, session = local_session)
          showNotification("导入完成", type = "message", session = local_session)

          later::later(~ push_state_to_python(), 10)

          if (mid == "single_cell") {
            batches <- get_sc_batches()
            old_status <- sc_filter_status()
            new_status <- setNames(rep("pending", length(batches)), names(batches))
            for (nm in names(old_status)) {
              if (old_status[[nm]] == "done" && nm %in% names(new_status))
                new_status[[nm]] <- "done"
            }
            sc_filter_status(new_status)
            sc_scan(NULL); sc_root(NULL)
          } else if (mid == "custom_table") {
            sc_scan(NULL); sc_root(NULL)
          } else {
            sc_scan(NULL); sc_root(NULL)
            nav_select("main_nav", selected = "分析", session = nav_session)
          }
        }, error = function(e) {
          removeNotification(nid, session = local_session)
          showNotification(paste("导入失败:", e$message), type = "error",
                           duration = NULL, session = local_session)
        })
      }, 0.2)
    })

    # ================================================================
    #  数据预览（非 SC） / SC QC 过滤视图
    # ================================================================
    output$preview <- DT::renderDataTable({
      df <- temp_df()
      if (!is.null(df)) {
        return(DT::datatable(head(df, 200),
          options = list(pageLength = input$page_size, scrollX = TRUE, dom = 'tip'),
          rownames = FALSE))
      }
      if (!is.null(state$name) && length(state$data) > 0) {
        info <- state$meta$sc_batch_info
        if (!is.null(info)) {
          rows <- do.call(rbind, lapply(names(info), function(k) {
            bi <- info[[k]]
            data.frame(Batch = bi$batch_name, Slot = k,
                       Samples = paste(bi$samples, collapse = ", "),
                       Cells = sum(bi$n_cells),
                       stringsAsFactors = FALSE)
          }))
          return(DT::datatable(rows,
            options = list(pageLength = input$page_size, scrollX = TRUE, dom = 'tip'),
            rownames = FALSE))
        }
        mat <- state$data[[1]]
        if (!is.null(mat) && !inherits(mat, "Seurat") && (is.matrix(mat) || is.data.frame(mat))) {
          r <- min(nrow(mat), 50); c <- min(ncol(mat), 20)
          df <- as.data.frame(mat[seq_len(r), seq_len(c), drop = FALSE])
          return(DT::datatable(df,
            caption = paste0("展示前 ", r, " 行 × ", c, " 列（共 ",
                           nrow(mat), " × ", ncol(mat), "）"),
            options = list(pageLength = input$page_size, scrollX = TRUE, dom = 'tip')))
        }
      }
      validate(need(FALSE, "请上传文件并确认数据结构"))
    })

    # ================================================================
    # ================================================================
    #  SC QC + 过滤视图（按 batch 分组，每 batch 共享一套 filter）
    # ================================================================
    output$sc_qc_view <- renderUI({
      batches <- get_sc_batches()
      status  <- sc_filter_status()
      if (length(batches) == 0) return(NULL)

      n_total  <- length(batches)
      n_done   <- sum(unlist(status) == "done")
      all_done <- n_done >= n_total

      # 每个 batch 一张 card
      cards <- lapply(names(batches), function(batch_key) {
        bk   <- batches[[batch_key]]
        st   <- status[[batch_key]] %||% "pending"
        badge <- if (st == "done")
          tags$span("✓ 已过滤", class = "badge bg-success")
        else
          tags$span("○ 待过滤", class = "badge bg-warning")

        samples <- bk$samples
        n_total_cells <- sum(sapply(samples, ncol))
        n_samples <- length(samples)

        # 每个样本的小提琴图
        plot_list <- lapply(names(samples), function(sname) {
          sobj <- samples[[sname]]
          tagList(
            tags$h6(sname, style = "margin:4px 0;"),
            plotOutput(ns(paste0("sc_qc_plot_", sname)), height = "180px")
          )
        })

        fp <- sc_filter_params()[[batch_key]] %||% list(mode = "auto")

        card(
          card_header(tags$div(
            tags$strong(bk$batch_name),
            tags$small(sprintf(" | %d 个样本, %d 个细胞", n_samples, n_total_cells)),
            tags$span(style = "float:right;", badge)
          )),
          layout_sidebar(
            sidebar = sidebar(
              width = 280,
              radioButtons(ns(paste0("sc_filter_mode_", batch_key)), "过滤方式",
                choices = c("自动 (sc_filter)" = "auto",
                             "手动" = "manual"),
                selected = fp$mode %||% "auto",
                inline = TRUE),
              conditionalPanel(
                paste0("input['", ns(paste0("sc_filter_mode_", batch_key)), "'] == 'manual'"),
                numericInput(ns(paste0("sc_nCount_lower_", batch_key)), "nCount下限",
                  value = fp$nCount_lower %||% 200, min = 0),
                numericInput(ns(paste0("sc_nCount_upper_", batch_key)), "nCount上限",
                  value = fp$nCount_upper %||% 50000, min = 0),
                numericInput(ns(paste0("sc_nFeature_lower_", batch_key)), "nFeature下限",
                  value = fp$nFeature_lower %||% 200, min = 0),
                numericInput(ns(paste0("sc_nFeature_upper_", batch_key)), "nFeature上限",
                  value = fp$nFeature_upper %||% 10000, min = 0),
                numericInput(ns(paste0("sc_mt_upper_", batch_key)), "MT%上限",
                  value = fp$mt_upper %||% 20, min = 0, max = 100)
              ),
              textInput(ns(paste0("sc_group_", batch_key)), "分组 (group)",
                value = fp$group %||% "",
                placeholder = "如: Control, Treat"),
              actionButton(ns(paste0("sc_filter_btn_", batch_key)),
                paste("过滤", bk$batch_name), class = "btn-primary btn-sm", width = "100%")
            ),
            do.call(tagList, plot_list)
          )
        )
      })

      tagList(
        tags$div(style = "margin-bottom:15px;",
          tags$div(style = "display:flex; align-items:center; gap:10px;",
            tags$h5(sprintf("QC 过滤: %d / %d 批次已过滤", n_done, n_total),
                    style = "margin:0;"),
            if (all_done) actionButton(ns("sc_go_analysis"),
              "进入分析 ▶", class = "btn-success")
          ),
          tags$div(style = "margin-top:5px;",
            tags$progress(id = ns("sc_filter_progress"),
              value = as.character(n_done), max = as.character(n_total),
              style = "width:100%; height:10px;"))
        ),
        do.call(tagList, cards)
      )
    })

    # 响应式读取 batch + 样本
    sc_batches_reactive <- reactive({
      sc_filter_status()
      get_sc_batches()
    })

    # 动态渲染 QC violin plots（每个样本）
    observe({
      batches <- sc_batches_reactive()
      for (batch_key in names(batches)) {
        for (sname in names(batches[[batch_key]]$samples)) {
          local({
            local_name <- sname
            out_id <- paste0("sc_qc_plot_", local_name)
            output[[out_id]] <- renderPlot({
              sobj <- batches[[batch_key]]$samples[[local_name]]
              req(sobj)
              VlnPlot(sobj, features = c("nCount_RNA", "nFeature_RNA", "percent.mt"),
                      ncol = 3, pt.size = 0.1) &
                theme(axis.title.x = element_blank(),
                      axis.text.x  = element_text(size = 8))
            })
          })
        }
      }
    })

    # per-batch 过滤按钮
    observe({
      batches <- sc_batches_reactive()
      lapply(names(batches), function(batch_key) {
        btn_id <- paste0("sc_filter_btn_", batch_key)
        observeEvent(input[[btn_id]], {
          batch  <- sc_batches_reactive()[[batch_key]]
          samples <- batch$samples
          mode   <- input[[paste0("sc_filter_mode_", batch_key)]] %||% "auto"
          species <- engine$project$meta$species %||% "Hs"

          # 计算该 batch 共享的 filter
          if (mode == "auto") {
            fv <- get_batch_filter(samples, species)
          } else {
            fv <- c(
              nCount_lower  = input[[paste0("sc_nCount_lower_",  batch_key)]] %||% 200,
              nCount_upper  = input[[paste0("sc_nCount_upper_",  batch_key)]] %||% 50000,
              nFeature_lower = input[[paste0("sc_nFeature_lower_", batch_key)]] %||% 200,
              nFeature_upper = input[[paste0("sc_nFeature_upper_", batch_key)]] %||% 10000,
              mt_upper      = input[[paste0("sc_mt_upper_",       batch_key)]] %||% 20
            )
          }

          # 同一 filter 应用到该 batch 所有样本
          group_val <- input[[paste0("sc_group_", batch_key)]] %||% ""

          for (sname in names(samples)) {
            sobj <- samples[[sname]]
            sobj <- subset(sobj,
              subset = nCount_RNA > fv[["nCount_lower"]] & nCount_RNA < fv[["nCount_upper"]] &
                       nFeature_RNA > fv[["nFeature_lower"]] & nFeature_RNA < fv[["nFeature_upper"]] &
                       percent.mt < fv[["mt_upper"]])
            if (nzchar(group_val)) sobj$group <- group_val

            # 更新 project$data
            if (is.character(engine$project$data[[batch_key]]))
              engine$project$data[[batch_key]] <- readRDS(engine$project$data[[batch_key]])
            engine$project$data[[batch_key]][[sname]] <- sobj
          }

          state$data <- engine$project$data
          state$data_keys <- paste(names(engine$project$data), collapse = ",")

          # 更新 batch 过滤状态
          cur_status <- sc_filter_status()
          cur_status[[batch_key]] <- "done"
          sc_filter_status(cur_status)

          cur_params <- sc_filter_params()
          cur_params[[batch_key]] <- list(
            mode        = mode,
            group       = group_val,
            nCount_lower  = as.numeric(fv[["nCount_lower"]]),
            nCount_upper  = as.numeric(fv[["nCount_upper"]]),
            nFeature_lower = as.numeric(fv[["nFeature_lower"]]),
            nFeature_upper = as.numeric(fv[["nFeature_upper"]]),
            mt_upper      = as.numeric(fv[["mt_upper"]])
          )
          sc_filter_params(cur_params)

          showNotification(paste(batch$batch_name, "过滤完成:", ncol(sobj), "个细胞"),
                           type = "message")

          # 异步同步到 Python 后端，不阻塞 UI
          later::later(~ push_state_to_python(), 10)
        }, ignoreInit = TRUE)
      })
    })

    # 进入分析按钮
    observeEvent(input$sc_go_analysis, {
      # 记录每个 batch 的过滤方法
      if (is.null(engine$project$results$`_methods`))
        engine$project$results$`_methods` <- list()
      fps <- sc_filter_params()
      batches <- get_sc_batches()
      for (batch_key in names(batches)) {
        fp <- fps[[batch_key]]
        if (is.null(fp)) next
        batch_name <- batches[[batch_key]]$batch_name
        engine$project$results$`_methods`[[paste0("sc_filter_", batch_key)]] <- list(
          tool       = "sc_filter",
          tool_name  = paste0("QC过滤-", batch_name),
          method     = if (fp$mode == "auto") "seqTools::sc_filter + get_filter_params (auto)" else "subset (manual)",
          package    = "seqTools",
          version    = tryCatch(as.character(packageVersion("seqTools")), error = function(e) "?"),
          params     = fp[setdiff(names(fp), c("mode", "group"))],
          group      = fp$group %||% "",
          time       = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      }
      # pre-analyzed 导入无 batch，记录导入方法
      if (length(batches) == 0 && !is.null(engine$project$data[["clustered_seurat"]])) {
        engine$project$results$`_methods`[["sc_import_preanalyzed"]] <- list(
          tool       = "sc_import",
          tool_name  = "导入已分析数据",
          method     = "readRDS (pre-analyzed Seurat object)",
          package    = "Seurat",
          version    = tryCatch(as.character(packageVersion("Seurat")), error = function(e) "?"),
          params     = list(note = "QC已跳过，直接使用已有聚类和注释"),
          time       = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      }
      engine$project$meta$sc_filtered <- TRUE
      state$meta <- engine$project$meta
      state$data <- engine$project$data
      state$data_keys <- paste(names(engine$project$data), collapse = ",")

      # 预热缓存：提前加载 Seurat 对象，避免首次工具点击卡顿
      tryCatch({
        sc_key <- sc_find_seurat_key(engine$project, NULL)
        sc_load_from_disk(engine$project, sc_key)  # 内部自动缓存 meta_cols
      }, error = function(e) {})
      nav_select("main_nav", selected = "分析", session = nav_session)
    })

    # ================================================================
    #  主视图：SC 模式下显示 QC，否则显示 DT preview
    # ================================================================
    output$main_view <- renderUI({
      force(state$data); force(state$omics_type)
      # pre-analyzed 直接导入 → 显示汇总，可直接进入分析
      if (identical(state$omics_type, "single_cell") &&
          !is.null(engine$project$data[["clustered_seurat"]]) &&
          length(get_sc_batches()) == 0) {
        sobj <- engine$project$data[["clustered_seurat"]]
        if (is.character(sobj)) sobj <- readRDS(sobj)
        tagList(
          h5(paste0("已导入分析好的数据: ", ncol(sobj), " 个细胞, ",
                    length(unique(Idents(sobj))), " 个 cluster")),
          actionButton(ns("sc_go_analysis"), "进入分析 ▶", class = "btn-success btn-lg")
        )
      } else if (identical(state$omics_type, "single_cell") &&
                 length(get_all_sc_samples()) > 0) {
        uiOutput(ns("sc_qc_view"))
      } else {
        DT::dataTableOutput(ns("preview"))
      }
    })

    output$debug_info <- renderPrint({
      cat("项目:", state$name %||% "无", "\n")
      cat("导入方法:", input$import_method %||% "无", "\n")
      df <- temp_df()
      if (!is.null(df)) {
        cat("待导入数据:", paste(dim(df), collapse = " x "), "\n")
        cols <- temp_cols()
        cat("基因列:", cols$gene_col %||% "未选", "\n")
        cat("样本列:", length(cols$sample_cols %||% c()), "个\n")
      } else if (length(state$data) > 0) {
        info <- state$meta$sc_batch_info
        if (!is.null(info)) {
          cat("已导入单细胞数据:", length(state$data), "个batch\n")
          for (k in names(info)) {
            bi <- info[[k]]
            cat("  ", k, "(", bi$batch_name, "):",
                length(bi$samples), "个样本,",
                sum(bi$n_cells), "个细胞\n")
          }
          status <- sc_filter_status()
          if (length(status) > 0) {
            cat("\n过滤状态:\n")
            for (nm in names(status))
              cat("  ", nm, ":", status[[nm]], "\n")
          }
        } else {
          cat("已导入数据:", paste(dim(state$data[[1]]), collapse = " x "), "\n")
        }
      }
    })
  })
}
