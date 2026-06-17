# R/imports/single_cell_import.R
# 单细胞智能导入：自动扫描目录，识别10X/h5/flat格式，支持批次分组和过滤

IMPORT_ID   <- "single_cell"
IMPORT_NAME <- "Single Cell"

IMPORT_SCHEMA <- list(
  file = list(type = "file", required = TRUE,
              description = "包含单细胞数据的根目录"),
  action = list(type = "select", choices = c("import", "scan"),
                default = "import", description = "import=确认导入, scan=扫描目录"),
  species = list(type = "select", choices = c("Hs", "Mm"), required = TRUE,
    description = "物种（必选）。Hs=人类，Mm=小鼠。影响MT基因过滤和下游分析"),
  # 导入时不做过滤，过滤放到 data 模块的 QC 步骤
  batch_assign_json = list(type = "text", required = FALSE,
                           description = "JSON格式批次分配")
)

IMPORT_RUN <- function(file_path, params) {
  require(Seurat)
  require(seqTools)

  action <- params$action %||% "scan"

  # ========== Stage 1: 扫描目录 ==========
  if (action == "scan") {
    scan_result <- sc_scan_directory(file_path)

    # 构建样本摘要
    sample_info <- lapply(scan_result$samples, function(s) {
      list(name = s$name, type = s$type, batch = s$batch)
    })

    return(list(
      data = list(),
      meta = list(
        sc_scan_result = list(
          samples     = sample_info,
          batches     = scan_result$batches,
          method      = scan_result$method,
          n_total     = scan_result$n_total,
          root_dir    = file_path
        )
      ),
      omics_type = NULL
    ))
  }

  # ========== Stage 2: 确认导入（只读取，不过滤） ==========
  scan_result <- sc_scan_directory(file_path)

  species <- params$species %||% "Hs"

  # 解析批次分配
  batch_assign <- list()
  if (!is.null(params$batch_assign_json) && nzchar(params$batch_assign_json)) {
    tryCatch({
      batch_assign <- jsonlite::fromJSON(params$batch_assign_json)
    }, error = function(e) {
      warning("批次分配JSON解析失败: ", e$message)
    })
  }

  if (length(batch_assign) == 0) {
    for (s in scan_result$samples) {
      batch_assign[[s$name]] <- s$batch
    }
  }

  # 按批次分组
  batch_groups <- split(names(batch_assign),
                        unlist(batch_assign, use.names = FALSE))
  batch_groups <- batch_groups[order(names(batch_groups))]

  # 读取每个样本，不进行任何过滤
  data_out <- list()
  batch_info <- list()
  batch_idx <- 1

  for (batch_name in names(batch_groups)) {
    sample_names <- batch_groups[[batch_name]]
    dataList <- list()
    batch_n_cells <- integer(0)

    for (sname in sample_names) {
      sinfo <- scan_result$samples[[sname]]
      if (is.null(sinfo)) stop("样本不存在: ", sname)

      message("[SC import] reading ", sname, " (", sinfo$type, ")")

      if (sinfo$type == "rds") {
        # rds：直接读取 Seurat 对象，兼容 v3→v5
        seurat_obj <- readRDS(sinfo$path)
        seurat_obj <- sc_update_seurat(seurat_obj)
        if (!inherits(seurat_obj, "Seurat"))
          stop(sname, ": rds 文件不是 Seurat 对象，class = ", class(seurat_obj)[1])
        # 确保有 percent.mt
        if (!"percent.mt" %in% colnames(seurat_obj@meta.data)) {
          mt_pattern <- if (toupper(species) == "MM") "^mt-" else "^MT-"
          seurat_obj[["percent.mt"]] <- PercentageFeatureSet(seurat_obj, pattern = mt_pattern)
        }
        seurat_obj@misc$species <- species
        dataList[[sname]] <- seurat_obj
        batch_n_cells <- c(batch_n_cells, setNames(ncol(seurat_obj), sname))

      } else {
        # 读取 counts 矩阵
        counts <- if (sinfo$type == "10x_triple") {
          fl <- sinfo$files
          if (!is.null(fl) && !anyNA(fl)) {
            tmp_dir <- file.path(tempdir(), sname)
            dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
            file.copy(fl["barcodes"], file.path(tmp_dir, "barcodes.tsv.gz"), overwrite = TRUE)
            file.copy(fl["features"], file.path(tmp_dir, "features.tsv.gz"), overwrite = TRUE)
            mtx_dst <- if (grepl("\\.gz$", fl["matrix"], ignore.case = TRUE)) "matrix.mtx.gz" else "matrix.mtx"
            file.copy(fl["matrix"], file.path(tmp_dir, mtx_dst), overwrite = TRUE)
            Seurat::Read10X(data.dir = tmp_dir, gene.column = 1)
          } else {
            Seurat::Read10X(data.dir = sinfo$path, gene.column = 1)
          }
        } else if (sinfo$type == "h5") {
          Seurat::Read10X_h5(filename = sinfo$path)
        } else if (sinfo$type == "flat") {
          fl <- sinfo$files
          tmp_dir <- file.path(tempdir(), sname)
          dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
          file.copy(fl["barcodes"], file.path(tmp_dir, "barcodes.tsv.gz"), overwrite = TRUE)
          file.copy(fl["features"], file.path(tmp_dir, "features.tsv.gz"), overwrite = TRUE)
          # 保留原始压缩状态：matrix.mtx → matrix.mtx.gz, matrix.mtx.gz → matrix.mtx.gz
          mtx_dst <- if (grepl("\\.gz$", fl["matrix"], ignore.case = TRUE)) "matrix.mtx.gz" else "matrix.mtx"
          file.copy(fl["matrix"], file.path(tmp_dir, mtx_dst), overwrite = TRUE)
          Seurat::Read10X(data.dir = tmp_dir, gene.column = 1)
        } else if (sinfo$type == "matrix_txt") {
          # txt/csv/tsv count 矩阵：行=基因, 列=细胞
          ext <- tolower(tools::file_ext(sinfo$path))
          if (ext == "csv") {
            mat <- as.matrix(read.csv(sinfo$path, row.names = 1,
                                      check.names = FALSE))
          } else {
            mat <- as.matrix(read.table(sinfo$path, header = TRUE, row.names = 1,
                                        sep = "\t", check.names = FALSE))
          }
          if (!is.numeric(mat)) stop(sname, ": count 矩阵包含非数值列")
          mat
        } else {
          stop("未知样本类型: ", sinfo$type)
        }

        seurat_obj <- Seurat::CreateSeuratObject(
          counts = counts,
          project = sname,
          min.cells = 3,
          min.features = 200
        )

        mt_pattern <- if (toupper(species) == "MM") "^mt-" else "^MT-"
        seurat_obj[["percent.mt"]] <- PercentageFeatureSet(seurat_obj, pattern = mt_pattern)

        seurat_obj@misc$species <- species
        dataList[[sname]] <- seurat_obj
        batch_n_cells <- c(batch_n_cells, setNames(ncol(seurat_obj), sname))
      }
    }

    if (length(dataList) > 0) {
      slot_name <- paste0("dataList", batch_idx)
      data_out[[slot_name]] <- dataList
      batch_info[[slot_name]] <- list(
        batch_name = batch_name,
        samples = names(dataList),
        n_cells = batch_n_cells
      )
      batch_idx <- batch_idx + 1
    }
  }

  if (length(data_out) == 0) stop("导入失败：无有效数据")

  list(
    data = data_out,
    meta = list(
      species       = species,
      sc_filtered   = FALSE,
      sc_batch_info = batch_info,
      sc_method     = scan_result$method,
      filename      = basename(file_path)
    ),
    omics_type = "single_cell"
  )
}
