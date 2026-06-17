# R/utils/sc_helpers.R
# 单细胞工具函数：目录扫描、RDS暂存、数据访问

# ---------- 目录扫描 ----------

#' 自动扫描单细胞数据目录，识别样本和批次
sc_scan_directory <- function(root_dir) {
  if (!dir.exists(root_dir)) stop("目录不存在: ", root_dir)

  samples <- list()  # list of list(name, path, type)
  method <- NULL

  # ---- 策略1: 检查子目录中的10X三件套 ----
  subdirs <- list.dirs(root_dir, full.names = TRUE, recursive = FALSE)
  if (length(subdirs) > 0) {
    for (sd in subdirs) {
      files_sd <- list.files(sd, full.names = FALSE)
      find_file <- function(pat) {
        hits <- files_sd[grepl(pat, files_sd, ignore.case = TRUE)]
        if (length(hits) > 0) file.path(sd, hits[1]) else NA_character_
      }
      mtx_path <- find_file("\\.mtx")
      feat_path <- find_file("features|genes")
      bc_path   <- find_file("barcodes")
      if (!is.na(mtx_path) && !is.na(feat_path) && !is.na(bc_path)) {
        samples[[length(samples) + 1]] <- list(
          name = basename(sd), path = sd, type = "10x_triple",
          batch = basename(dirname(sd)),
          files = c(barcodes = bc_path, features = feat_path, matrix = mtx_path)
        )
        method <- "10x_triple"
      }
    }
  }

  # ---- 策略2: 检查 .h5 文件 ----
  h5_files <- list.files(root_dir, pattern = "\\.h5$", full.names = TRUE,
                         ignore.case = TRUE, recursive = FALSE)
  if (length(h5_files) > 0) {
    for (hf in h5_files) {
      samples[[length(samples) + 1]] <- list(
        name = gsub("\\.h5$", "", basename(hf), ignore.case = TRUE),
        path = hf, type = "h5",
        batch = basename(root_dir)
      )
    }
    method <- if (is.null(method)) "h5" else paste(method, "h5", sep = "+")
  }

  # ---- 策略3: flat-folder 自动分组 ----
  if (length(samples) == 0) {
    all_files <- list.files(root_dir, full.names = FALSE, recursive = FALSE)
    if (length(all_files) == 0) stop("目录为空: ", root_dir)

    # 按文件类型分类
    classify_file <- function(fname) {
      fn <- tolower(fname)
      if (grepl("barcode", fn)) return("barcodes")
      if (grepl("feature|gene", fn)) return("features")
      if (grepl("matrix|\\.mtx", fn)) return("matrix")
      return(NA)
    }

    types <- sapply(all_files, classify_file)
    typed_files <- all_files[!is.na(types)]
    types <- types[!is.na(types)]

    if (length(typed_files) == 0) {
      # 回退：不是 10X flat 文件，跳到策略 4/5；不执行后面分组逻辑
      samples <- list()
    } else {

      # 提取样本前缀：去除已知后缀，找共同分隔符
    extract_prefix <- function(fname, ftype) {
      fn <- tolower(fname)
      # 尝试常见模式
      patterns <- c(
        paste0("_", ftype, ".*$"),
        paste0("\\.", ftype, ".*$"),
        paste0("-", ftype, ".*$"),
        paste0("_", ftype, "$"),
        paste0("\\.", ftype, "$")
      )
      for (pat in patterns) {
        if (grepl(pat, fn)) return(sub(pat, "", fname))
      }
      fname
    }

    prefixes <- mapply(extract_prefix, typed_files, types, USE.NAMES = FALSE)

    # 按前缀分组
    groups <- split(data.frame(file = typed_files, type = types,
                               full_path = file.path(root_dir, typed_files),
                               stringsAsFactors = FALSE),
                    prefixes)

    for (pfx in names(groups)) {
      grp <- groups[[pfx]]
      grp_types <- grp$type
      if (length(unique(grp_types)) >= 2 || nrow(grp) == 3) {
        # 达到最少文件类型，作为一个样本
        samples[[length(samples) + 1]] <- list(
          name = pfx,
          path = root_dir,
          type = "flat",
          batch = basename(root_dir),
          files = setNames(grp$full_path, grp_types)
        )
        method <- if (is.null(method)) "flat" else paste(method, "flat", sep = "+")
      }
    }
    }  # end of else (strategy 3)

    if (length(samples) == 0) {
      # ---- 策略4: txt/csv/tsv count 矩阵（每个文件 = 一个样本） ----
      matrix_files <- list.files(root_dir,
        pattern = "\\.(txt|csv|tsv|gz)$", full.names = TRUE,
        ignore.case = TRUE, recursive = FALSE)
      # 排除明显不是 count 矩阵的文件
      matrix_files <- matrix_files[!grepl("barcode|feature|gene|readme|requirement",
        basename(matrix_files), ignore.case = TRUE)]

      if (length(matrix_files) > 0) {
        for (mf in matrix_files) {
          samples[[length(samples) + 1]] <- list(
            name  = tools::file_path_sans_ext(basename(mf)),
            path  = mf,
            type  = "matrix_txt",
            batch = basename(root_dir)
          )
        }
        method <- if (is.null(method)) "matrix_txt" else paste(method, "matrix_txt", sep = "+")
      }

      # ---- 策略5: .rds 文件（每个文件 = 一个 Seurat 对象） ----
      rds_files <- list.files(root_dir,
        pattern = "\\.rds$", full.names = TRUE,
        ignore.case = TRUE, recursive = FALSE)

      if (length(rds_files) > 0) {
        for (rf in rds_files) {
          samples[[length(samples) + 1]] <- list(
            name  = tools::file_path_sans_ext(basename(rf)),
            path  = rf,
            type  = "rds",
            batch = basename(root_dir)
          )
        }
        method <- if (is.null(method)) "rds" else paste(method, "rds", sep = "+")
      }
    }

    if (length(samples) == 0) stop("无法自动分组文件。请将每个样本放入独立子目录（含 matrix/features/barcodes 三文件），或放入 count 矩阵文件(.txt/.csv/.tsv) 或 Seurat 对象(.rds)")
  }

  # ---- 自动批次分配：同父目录→同批次 ----
  batch_names <- unique(sapply(samples, `[[`, "batch"))
  batch_map <- setNames(paste0("Batch", seq_along(batch_names)), batch_names)
  for (i in seq_along(samples)) {
    samples[[i]]$batch <- batch_map[[samples[[i]]$batch]]
  }

  batches <- unique(sapply(samples, `[[`, "batch"))

  names(samples) <- sapply(samples, `[[`, "name")

  list(
    samples = samples,
    batches = batches,
    method  = method %||% "unknown",
    n_total = length(samples)
  )
}


# ---------- RDS 暂存 ----------

sc_init_temp_dir <- function(project) {
  if (is.null(project$meta$sc_temp_dir) || !dir.exists(project$meta$sc_temp_dir)) {
    wd <- tryCatch(get("APP_WORK_DIR", envir = .GlobalEnv), error = function(e) tempdir())
    dir_path <- file.path(wd, "cache", "sc", gsub("[^a-zA-Z0-9]", "_", project$name %||% "project"))
    dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
    project$meta$sc_temp_dir <- dir_path
  }
  invisible(project$meta$sc_temp_dir)
}

sc_save_to_disk <- function(project, key, release = FALSE) {
  obj <- project$data[[key]]
  if (is.null(obj)) return(invisible(NULL))
  if (is.character(obj)) return(invisible(obj))

  sc_init_temp_dir(project)
  rds_path <- file.path(project$meta$sc_temp_dir, paste0(key, ".rds"))
  saveRDS(obj, rds_path, compress = FALSE)
  if (release) {
    project$data[[key]] <- rds_path
    rm(obj)
  }
  gc()  # saveRDS 会产生临时序列化内存，立即回收
  message("[sc] ", if (release) "saved+released " else "cached ", key, " -> ", rds_path)
  invisible(rds_path)
}

sc_load_from_disk <- function(project, key) {
  val <- project$data[[key]]
  if (is.null(val)) stop("数据不存在: ", key)
  if (!is.character(val)) {
    # 已在内存，预热缓存
    if (inherits(val, "Seurat")) {
      if (is.null(project$meta$sc_meta_cols)) {
        project$meta$sc_meta_cols <- as.character(colnames(val@meta.data))
        cl <- sort(unique(as.character(Idents(val))))
        project$meta$sc_cluster_ids <- cl[!is.na(cl) & nzchar(cl)]
      }
      if (!"All_Cells" %in% colnames(val@meta.data))
        val$All_Cells <- "All"
    }
    return(val)
  }

  if (!file.exists(val)) stop("RDS文件不存在: ", val)
  message("[sc] loading ", key, " from ", val)
  obj <- readRDS(val)
  obj <- sc_update_seurat(obj)
  if (inherits(obj, "Seurat") && !"All_Cells" %in% colnames(obj@meta.data))
    obj$All_Cells <- "All"
  project$data[[key]] <- obj
  # 预热缓存，避免后续 schema_to_ui 重复 readRDS
  if (inherits(obj, "Seurat")) {
    project$meta$sc_meta_cols <- as.character(colnames(obj@meta.data))
    cl <- sort(unique(as.character(Idents(obj))))
    project$meta$sc_cluster_ids <- cl[!is.na(cl) & nzchar(cl)]
  }
  obj
}

# Seurat v3→v5 兼容升级（跳过已是 v5 的对象）
sc_update_seurat <- function(obj) {
  if (!inherits(obj, "Seurat")) return(obj)
  # Assay5 说明已是 v5，跳过
  if (inherits(obj@assays$RNA, "Assay5")) return(obj)
  sv <- as.character(tryCatch(packageVersion("SeuratObject"), error = function(e) "0"))
  if (utils::compareVersion(sv, "5.0.0") >= 0) {
    obj <- tryCatch(SeuratObject::UpdateSeuratObject(obj), error = function(e) obj)
  }
  obj
}


# ---------- 数据访问 ----------

sc_list_dataList_keys <- function(project) {
  keys <- names(project$data)
  keys[grepl("^dataList\\d+$", keys)]
}

sc_get_dataLists <- function(project) {
  keys <- sc_list_dataList_keys(project)
  if (length(keys) == 0) stop("无单细胞数据 (dataList1, dataList2, ...)，请先导入")
  dl <- lapply(keys, function(k) sc_load_from_disk(project, k))
  names(dl) <- keys
  dl
}

sc_has_dataLists <- function(project) {
  length(sc_list_dataList_keys(project)) > 0
}

# 在 project$data 中按优先级查找 Seurat 对象的 key
sc_find_seurat_key <- function(project, preferred = NULL) {
  if (!is.null(preferred) && nzchar(preferred) && !is.null(project$data[[preferred]]))
    return(preferred)
  for (k in c("clustered_seurat", "integrated_seurat", "merged_seurat", "sct_seurat")) {
    if (!is.null(project$data[[k]])) return(k)
  }
  stop("未找到可用的 Seurat 对象，请先运行标准化或聚类")
}

# 获取分类 meta.data 列（只读缓存，不加载 Seurat 对象，避免阻塞 UI）
sc_get_categorical_cols <- function(project) {
  cols <- project$meta$sc_meta_cols
  if (is.null(cols) || length(cols) == 0) return(character(0))
  cols <- setdiff(cols, "All_Cells")
  # 过滤连续型变量（唯一值 > 50），只从内存读
  sc_key <- sc_find_seurat_key(project, NULL)
  val <- project$data[[sc_key]]
  if (!is.character(val) && inherits(val, "Seurat")) {
    cols <- Filter(function(cn) {
      uv <- length(unique(val@meta.data[[cn]]))
      uv > 0 && uv <= 50
    }, cols)
  }
  as.character(cols)
}

# 解析 "percent.mt,S.Score" 为字符向量
sc_parse_vars <- function(vars_str, default = "percent.mt") {
  v <- trimws(strsplit(vars_str %||% default, ",")[[1]])
  v[nzchar(v)]
}

# 基因名模糊匹配：先精确匹配，缺失的尝试大小写不敏感查找，给出纠错提示
sc_match_genes <- function(gene_vec, seurat_obj) {
  all_genes <- rownames(seurat_obj)
  missing <- setdiff(gene_vec, all_genes)
  if (length(missing) == 0) return(list(genes = gene_vec, msg = NULL))

  suggestions <- character(0)
  still_missing <- character(0)
  matched <- gene_vec[gene_vec %in% all_genes]

  for (g in missing) {
    idx <- which(tolower(all_genes) == tolower(g))
    if (length(idx) > 0) {
      suggestions <- c(suggestions,
        paste0(g, " → ", paste(all_genes[idx][1:min(3, length(idx))], collapse = " 或 ")))
      matched <- c(matched, all_genes[idx[1]])  # 自动修正为第一个匹配
    } else {
      still_missing <- c(still_missing, g)
    }
  }

  msg <- NULL
  if (length(suggestions) > 0)
    msg <- paste0("基因名大小写已自动修正: ", paste(suggestions, collapse = "; "))
  if (length(still_missing) > 0)
    stop(paste0(msg %||% "",
      if (!is.null(msg)) "\n",
      "以下基因不存在: ", paste(still_missing, collapse = ", "),
      "\n可用的基因示例: ", paste(head(all_genes, 5), collapse = ", ")))

  list(genes = matched, msg = msg)
}

# 解析 "0.2,0.4,0.6" 为数值向量
sc_parse_resolution <- function(res_str, default = c(0.2, 0.4, 0.6, 0.8, 1.0)) {
  tryCatch({
    r <- as.numeric(trimws(strsplit(res_str %||% "", ",")[[1]]))
    if (length(r) > 0 && !anyNA(r)) r else default
  }, error = function(e) default)
}

# 解析 "1:30" 为整数向量，留空返回 NULL
sc_parse_PCs <- function(pcs_str) {
  if (is.null(pcs_str) || !nzchar(pcs_str)) return(NULL)
  tryCatch(eval(parse(text = pcs_str)), error = function(e) NULL)
}
