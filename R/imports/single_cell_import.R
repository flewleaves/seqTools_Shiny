# R/imports/single_cell_import.R

IMPORT_ID <- "single_cell"
IMPORT_NAME <- "Single Cell"

IMPORT_SCHEMA <- list(
  file = list(type = "file", required = TRUE, description = "矩阵文件 (matrix.mtx.gz) 或包含三个文件的目录"),
  features_file = list(type = "file", required = FALSE, description = "基因信息 (features.tsv.gz)"),
  barcodes_file = list(type = "file", required = FALSE, description = "细胞条码 (barcodes.tsv.gz)"),
  species = list(type = "select", choices = c("Hs","Mm"), default = "Hs")
)

IMPORT_RUN <- function(file_path, params) {
  require(Seurat)

  # 判断输入是目录还是单个文件
  if (dir.exists(file_path)) {
    data_dir <- file_path
  } else {
    # 如果是单个文件，取所在目录
    data_dir <- dirname(file_path)
  }

  # 检查三个文件是否存在
  required_files <- c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz")
  found <- file.exists(file.path(data_dir, required_files))

  if (!all(found)) {
    # 尝试 features/genes 变体
    alt_files <- c("matrix.mtx.gz", "genes.tsv.gz", "barcodes.tsv.gz")
    found_alt <- file.exists(file.path(data_dir, alt_files))
    if (all(found_alt)) {
      required_files <- alt_files
    } else {
      stop("10X 数据不完整，需要: ", paste(required_files, collapse = ", "),
           " 在目录: ", data_dir)
    }
  }

  counts <- Seurat::Read10X(data.dir = data_dir, gene.column = 1)
  seurat_obj <- Seurat::CreateSeuratObject(counts = counts)

  species <- params$species %||% "Hs"
  seurat_obj@misc$species <- species

  list(
    data = list(seurat = seurat_obj),
    meta = list(species = species, filename = basename(data_dir)),
    omics_type = "single_cell"
  )
}
