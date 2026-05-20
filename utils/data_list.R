import_methods <- list(
  bulk_rna = list(
    name = "RNA-seq",
    ui = function(ns) {
      tagList(
        helpText("请上传基因表达矩阵，行为基因，列为样本。"),
        fileInput(ns("infile"), "选择 CSV/TXT", accept = c(".csv", ".txt")),
        hr(),
        selectInput(ns("data_type"), "数据类型",
                    choices = c("", "count", "tpm", "fpkm", "cpm"),
                    selected = ""),
        selectInput(ns("log_state"), "是否log化",
                    choices = c("", TRUE, FALSE), selected = ""),
        selectInput(ns("species"), "物种",
                    choices = c("", "Human", "Mouse"), selected = ""),
        selectInput(ns("id_type"), "基因 ID 类型",
                    choices = c("", "SYMBOL", "ENSEMBL", "ENTREZID"), selected = ""),
        textAreaInput(ns("group_info"), "分组信息",
                      placeholder = "组名,样本数;...", rows = 3),
        hr(),
        actionButton(ns("id_convert"), "基因ID转换", class = "btn-info")
      )
    },
    validate = function(input, state) {
      if (is.null(state$temp_df)) return("请先导入并确认数据结构")
      if (length(state$meta[["group_info"]]) != ncol(state$temp_df) - 1)
        return("分组信息与样本数不匹配")
      TRUE
    },
    run = function(input, state, session, ns) {
      temp <- state$temp_df
      showNotification("去除重复基因中...", type = "message", duration = 5)
      temp <- seqTools::remove_dup(temp, 1, method = state$settings$dup)
      showNotification(paste("保留", nrow(temp), "行"), type = "message", duration = 5)
      row.names(temp) <- temp[, 1]
      temp <- temp[, -1]
      if (state$meta[["data_type"]] == "count") temp <- round(temp)
      state$data[[state$meta[["data_type"]]]] <- temp
      state$omics_type <- "bulk_rna"
      state$temp_df <- NULL
      showNotification("RNA-seq 数据导入完成", type = "message")
      print(state$meta)
    },
    preview_data = function(input, state) {
      state$temp_df
    }
  ),

  single_cell = list(
    name = "Single Cell",
    ui = function(ns) {
      tagList(
        helpText("上传 10X Genomics 三个文件："),
        fileInput(ns("sc_matrix"), "矩阵文件 (matrix.mtx.gz)", accept = ".gz"),
        fileInput(ns("sc_features"), "基因信息 (features.tsv.gz)", accept = ".gz"),
        fileInput(ns("sc_barcodes"), "细胞条码 (barcodes.tsv.gz)", accept = ".gz"),
        hr(),
        selectInput(ns("sc_species"), "物种", choices = c("", "Human", "Mouse"), selected = ""),
        textAreaInput(ns("sc_group_info"), "分组信息（可选）", rows = 2),
        hr()
      )
    },
    validate = function(input, state) {
      if (is.null(input$sc_matrix) || is.null(input$sc_features) || is.null(input$sc_barcodes))
        return("请上传所有三个文件")
      TRUE
    },
    run = function(input, state, session, ns) {
      showNotification("创建 Seurat 对象...", type = "message", duration = NULL)
      # 假设三个文件在同一临时目录（Shiny 上传后通常在同一目录）
      data_dir <- dirname(input$sc_matrix$datapath)
      counts <- Seurat::Read10X(data.dir = data_dir, gene.column = 1)
      seurat_obj <- Seurat::CreateSeuratObject(counts = counts)
      if (input$sc_species != "") seurat_obj@misc$species <- input$sc_species
      if (nzchar(input$sc_group_info)) {
        state$meta[["group_info"]] <- parse_group_info(input$sc_group_info)
      }
      state$seurat_obj <- seurat_obj
      state$omics_type <- "single_cell"
      showNotification("单细胞数据导入成功", type = "message")
    },
    preview_data = function(input, state) {
      if (is.null(state$seurat_obj)) return(NULL)
      mat <- GetAssayData(state$seurat_obj, slot = "counts")
      if (ncol(mat) > 10) mat <- mat[, 1:10]
      as.data.frame(as.matrix(mat))
    }
  )
)