# R/tools/gsea_tool.R
TOOL_NAME         <- "gsea"
TOOL_CATEGORY     <- "差异/富集分析"
TOOL_OMICS        <- "bulk_rna"
TOOL_ORDER        <- 25
TOOL_DISPLAY_NAME <- "GSEA分析"

TOOL_SCHEMA <- list(
  ranking_mat   = list(type = "select",  choices = c("logFC","Signal-to-noise","Signed-p"), default = "logFC", description = "基因排序方法"),
  geneset_gsea  = list(type = "select",  choices = c("GO","KEGG","Reactome"),               default = "GO",    description = "参考基因集"),
  custom_gsea   = list(type = "file",    description = "自定义 .gmt 文件路径（可选）"),
  padj_gsea     = list(type = "number",  default = 1,    min = 0, max = 1, description = "Adjusted p 阈值"),
  ppadj_gsea    = list(type = "number",  default = 0.05, min = 0, max = 1, description = "绘图 Adjusted p 阈值"),
  minGS_gsea    = list(type = "integer", default = 10,   min = 0,           description = "最小基因集大小"),
  maxGS_gsea    = list(type = "integer", default = 500,  min = 0,           description = "最大基因集大小"),
  top_n_gsea    = list(type = "integer", default = 10,   min = 1,           description = "结果图展示前 N 个")
)

TOOL_RUN <- function(inputs, project) {
  deg_res <- project$results[["deg_result"]]   # ← 与 deg_tool 对齐
  if (is.null(deg_res)) stop("请先进行差异分析（运行 deg 工具）")

  tpm_mat <- NULL
  if (inputs$ranking_mat == "Signal-to-noise") {
    tpm_mat <- project$data[["tpm"]] %||% project$data[["count"]]
    if (is.null(tpm_mat)) stop("Signal-to-noise 需要表达矩阵")
    if (is.null(project$meta$deg_contrast))
      stop("缺少 deg_contrast，请先运行差异分析")
  }

  rank_vec <- engine_build_rank(
    deg_res  = deg_res,
    method   = inputs$ranking_mat,
    tpm_mat  = tpm_mat,
    group    = project$meta$group_info,
    contrast = project$meta$deg_contrast
  )

  genesets    <- inputs$geneset_gsea
  custom_path <- inputs$custom_gsea %||% ""
  if (nzchar(custom_path) && file.exists(custom_path)) genesets <- custom_path

  gsea_res <- engine_gsea(
    rank_vec  = rank_vec,
    genesets  = genesets,
    species   = project$meta$species  %||% "Hs",
    from      = project$meta$id_type  %||% "SYMBOL",
    p.value   = inputs$padj_gsea,
    minGSSize = inputs$minGS_gsea,
    maxGSSize = inputs$maxGS_gsea
  )

  dotplot <- engine_gsea_dotplot(
    gsea_res = gsea_res$result,
    topn     = inputs$top_n_gsea,
    pajust   = inputs$ppadj_gsea
  )

  # 存为两个键：gsea_obj（gseaResult）和 gsea_dotplot（ggplot）
  list(
    data     = list(obj = gsea_res$result, dotplot = dotplot$plot),
    messages = c(gsea_res$note, dotplot$note),
    method_info = record_method("gsea", "GSEA分析", "clusterProfiler::GSEA", "clusterProfiler", inputs)
  )
}

# engine$run 存为 gsea_obj 和 gsea_dotplot
# 多图输出示例：两个 tab，各自从 objs 列表中取所需对象
TOOL_OUTPUTS <- list(
  plot = list(
    dotplot = function(objs) {
      for (o in objs) if (inherits(o, "ggplot")) return(o)
      NULL
    },
    enrich = function(objs) {
      for (o in objs) if (inherits(o, "gseaResult") && nrow(o@result) > 0) {
        return(tryCatch(
          enrichplot::gseaplot2(o, geneSetID = 1, title = o@result$Description[1]),
          error = function(e) NULL
        ))
      }
      NULL
    }
  ),
  table = function(result) if (inherits(result, "gseaResult")) result@result else NULL
)
