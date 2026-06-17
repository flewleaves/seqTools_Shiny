# R/tools/sc_cor_tool.R
# 单细胞基因表达相关性 — seqTools::sc_cor

TOOL_NAME <- "sc_cor"
TOOL_CATEGORY <- "数据绘图"
TOOL_DISPLAY_NAME <- "基因相关性"
TOOL_OMICS <- "single_cell"
TOOL_ORDER <- 38

TOOL_SCHEMA <- list(
  genes = list(type = "text", required = TRUE,
    description = "基因名，英文逗号分隔。2个=散点图，>2个=相关性热图。例: Slc3a2,Slc7a5（大小写自动纠正）"),
  group_by = list(type = "select", choices = NULL, multiple = TRUE,
    default = "seurat_clusters",
    description = "分组列（可多选）。例: 选 cell_type 或 seurat_clusters。通常选一个就够了"),
  ident_group = list(type = "select", choices = NULL, required = TRUE,
    description = "身份列（单选，必须是 group_by 中选中的某一列）。例: group_by 选了 cell_type，这里就填 cell_type。target_group 的选项取决于这一列"),
  target_group = list(type = "select", choices = NULL, multiple = TRUE,
    cascade_from = "ident_group",
    description = "目标组（多选）。选项来自 ident_group 列的具体值。例: ident_group=cell_type 时可勾选 Acinar,MPC,Tip；ident_group=seurat_clusters 时可勾选 0,1,2"),
  reduction = list(type = "select",
    choices = c("harmony","pca","umap"), default = "harmony",
    description = "降维方法 (创建 metacell 时使用)"),
  method = list(type = "select",
    choices = c("pearson","spearman"), default = "pearson",
    description = "相关性计算方法"),
  k = list(type = "integer", default = 25, min = 5, max = 100,
    description = "metacell K 近邻数"),
  min_cells = list(type = "integer", default = 100, min = 10, max = 1000,
    description = "最少细胞数"),
  max_shared = list(type = "integer", default = 10, min = 1, max = 50,
    description = "最大共享细胞数"),
  cache_name = list(type = "text", default = "sc_cor_wgcna",
    description = "WGCNA 缓存文件名（不含 .rds 后缀）。相同名称复用缓存加速重复分析")
)

TOOL_RUN <- function(inputs, project) {
  require(seqTools)

  key <- sc_find_seurat_key(project, NULL)
  seurat_obj <- sc_load_from_disk(project, key)

  gene_vec <- trimws(strsplit(inputs$genes, ",")[[1]])
  gene_vec <- gene_vec[nzchar(gene_vec)]
  if (length(gene_vec) < 2) stop("至少需要 2 个基因")
  res <- sc_match_genes(gene_vec, seurat_obj)
  if (!is.null(res$msg)) message(res$msg)
  gene_vec <- res$genes

  target_group <- if (is.list(inputs$target_group))
    unlist(inputs$target_group) else inputs$target_group
  if (is.null(target_group) || length(target_group) == 0)
    stop("请选择至少一个目标组")

  group_by <- inputs$group_by
  if (is.null(group_by) || length(group_by) == 0)
    stop("请选择至少一个分组列")
  ident_group <- inputs$ident_group
  if (is.null(ident_group) || !nzchar(ident_group))
    stop("请选择身份列")
  if (!ident_group %in% group_by)
    stop("身份列 ", ident_group, " 不在分组列 ", paste(group_by, collapse=", "), " 中，请重新选择")

  wd <- tryCatch(get("APP_WORK_DIR", envir = .GlobalEnv), error = function(e) ".")
  # 缓存名包含 group_by 和 ident_group，避免不同分组参数复用同一 metacell
  cache_base <- inputs$cache_name %||% "sc_cor_wgcna"
  cache_base <- gsub("\\.rds$", "", cache_base)
  cache_name <- paste0(cache_base, "_", paste(group_by, collapse = "-"), "_", ident_group)

  result <- seqTools::sc_cor(
    scRNA        = seurat_obj,
    genes.check  = gene_vec,
    group.by     = group_by,
    ident.group  = ident_group,
    reduction    = inputs$reduction %||% "harmony",
    target.group = target_group,
    k            = inputs$k %||% 25,
    max_shared   = inputs$max_shared %||% 10,
    min.cells    = inputs$min_cells %||% 100,
    name         = cache_name,
    method       = inputs$method %||% "pearson"
  )

  if (length(gene_vec) == 2 && is.matrix(result)) {
    # 2 基因散点图
    df <- as.data.frame(result)
    colnames(df) <- gene_vec
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[gene_vec[1]]],
                                           y = .data[[gene_vec[2]]])) +
      ggplot2::geom_smooth(method = "lm", color = "#4D79A6",
                           formula = y ~ x, fill = "#A1CAE6") +
      ggplot2::theme_bw() +
      ggplot2::geom_point(colour = "#669BD2", size = 2) +
      ggpubr::stat_cor(method = inputs$method %||% "pearson")
    list(data = list(plot = p),
         messages = paste("相关性:", paste(gene_vec, collapse=" vs "),
                          "| 目标组:", paste(target_group, collapse=",")),
         method_info = record_method("sc_cor", "基因相关性",
           "Hmisc::rcorr + ggpubr::stat_cor", "Hmisc", inputs))
  } else {
    # >2 基因热图：转 ggplot
    r_mat <- result$r
    r_mat[is.na(r_mat)] <- 0
    # 去掉下三角避免重复
    r_mat[lower.tri(r_mat)] <- NA
    melt_df <- reshape2::melt(r_mat, varnames = c("Gene1", "Gene2"), value.name = "Corr")
    melt_df <- melt_df[!is.na(melt_df$Corr), ]
    p <- ggplot2::ggplot(melt_df, ggplot2::aes(Gene1, Gene2, fill = Corr)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                    midpoint = 0, limit = c(-1, 1)) +
      ggplot2::geom_text(ggplot2::aes(label = round(Corr, 2)), size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    list(data = list(plot = p),
         messages = paste("相关性热图完成:", length(gene_vec), "个基因",
                          "| 目标组:", paste(target_group, collapse=",")),
         method_info = record_method("sc_cor", "基因相关性",
           "Hmisc::rcorr + ggpubr::stat_cor", "Hmisc", inputs))
  }
}

TOOL_OUTPUTS <- list(
  plot = function(result) { if (inherits(result, "ggplot")) result else result$data$plot },
  table = NULL
)
