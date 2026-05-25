check <- function(state, check_list){
  
  for(i in check_list){
     if(!i %in% names(state$data) && !i %in% names(state$res) && !i %in% names(state$meta)){
       showNotification(paste0("失败:未读入", i), type = "error")
       return(FALSE)
     }
  }
  TRUE
}

normalize <- function(state, inputs, session, ns) {
  # 1. 基础检查
  if (!check(state, "count")) {
    return()
  }

  # 2. 如果需要 TPM 但没有基因长度 → 弹窗交互，中断本次运行
   if (inputs$method == "tpm" && is.null(state$meta[["gene.length"]])) {
    shinyalert(
      title = "缺少基因长度",
      text = "是否使用标准基因长度进行 TPM 标准化？（可能引入误差）",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "是",
      cancelButtonText = "否",
      callbackR = function(value) {
        if (value) {
          state$meta$gene.length <- seqTools::get_standard_gene_length(
            state$data[["count"]], species = state$meta[["species"]], from = state$meta[["id_type"]]
          )
          tryCatch({
            state$data[[inputs$method]] <- seqTools::normalization(
              state$data[["count"]],
              method = inputs$method,
              group = state$meta[["group_info"]],
              gene.length = state$meta$gene.length
            )
            showNotification("标准化完成", type = "message")
          }, error = function(e) {
            showNotification(e$message, type = "error")
          })
        }
        # value = FALSE（用户点"否"）→ 什么都不做
      }
    )
    return()
  }

  # 3. 正常执行标准化
  tryCatch({
    state$data[[toupper(inputs$method)]] <- seqTools::normalization(
      state$data[["count"]],
      method = inputs$method,
      group = state$meta[["group_info"]],
      gene.length = state$meta$gene.length
    )
    showNotification("标准化完成", type = "message")
  }, error = function(e) {
    showNotification(e$message, type = "error")
  })
}

expr_filter <- function(state, inputs, session, ns){
  n <- inputs$excluding_noncoding
  u <- inputs$excluding_unknown
  m <- inputs$min_count
  if(u && state$meta[["species"]] == "Mm"){
    kg <- org.Mm.eg.db::keys(org.Mm.eg.db::org.Mm.eg.db, keytype = state$meta[["id_type"]])
  }else{
    kg <- org.Hs.eg.db::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = state$meta[["id_type"]])
  }
  mats <- state$data
  for (idx in seq_along(mats)) {
    tp <- mats[[idx]]
    if(!is.null(tp)){
      tp <- tp[rowSums(tp) > m, ]
      if(n){
          pa <- paste0(
            "(?i)^(LINC|MIR|SNORD|SNORA|RNU|Gm\\d+|",     # 开头：长链非编码、miRNA、snoRNA、snRNA、小鼠预测基因
            "MALAT1|NEAT1|XIST|HOTAIR|TUG1|GAS5|H19|KCNQ1OT1|UCA1|PVT1|",  # 完整匹配的知名 lncRNA
            ")|",
            "(-AS\\d*$|-IT\\d*$|-Rik$|-Ps\\d*$)"       # 结尾：反义/内含子/RIKEN/假基因后缀
          )
        tp <- tp[!stringr::str_detect(row.names(tp), pa), ]
      }else if(u){
        tp <- tp[row.names(tp) %in% kg,]
      }
      mats[[idx]] <- tp
    }
  }
  state$data <- mats
}

read_vector <- function(text, state, optional = FALSE) {
  # 允许为空，且输入为空 → 返回 NULL
  if (optional && (is.null(text) || trimws(text) == "")) {
    return(NULL)
  }

  # 不允许为空 → 即使输入为空，也继续向下传递，让下游报错
  words <- strsplit(gsub(" ", "", text %||% ""), ";")[[1]]

  # 如果是基因名（非 HEX 颜色），进行大小写转换
  if (length(words) > 0 && !stringr::str_detect(words[1], "^#")) {
    words <- if (state$meta$species == "Mm") {
      stringr::str_to_title(words)
    } else {
      toupper(words)
    }
  }
  words
}

run_gsea <- function(state, inputs, session, ns){
  check(state, list("deg_res"))
  req(state$res$deg_res)
  geneList <- row.names(state$res$deg_res)
          rg <- NULL
          if(inputs$ranking_mat == "logFC"){
            rg <- state$res$deg_res$log2FoldChange
            names(rg) <- geneList
          }else if(inputs$ranking_mat == "Signed-p"){
            rg <- -log(state$res$deg_res$p.value)*sign(state$res$deg_res$log2FoldChange)
            names(rg) <- geneList
          }else{
            if(!check(state, list("tpm"))){return()}
            tryCatch({rg <- seqTools::s2nCalulator(state$data$tpm, group = state$meta$group_info, contrast = state$meta$deg_contrast)
              rg <- na.omit(rg)
              rg <- rg[names(rg) %in% geneList]
            }, error = function(e){showNotification(e$message, type = "error")})
          }
          if(!is.null(rg)){
             rg <- rg[order(rg, decreasing = TRUE)]
             genesets <- if(!is.null(inputs$custom_gsea)){
                clusterProfiler::read.gmt(inputs$custom_gsea)
             }else{
               inputs$geneset_gsea
             }
             state$res$gsea_res <- seqTools::quick_GSEA(rg, genesets = genesets, species = state$meta$species, from = state$meta$id_type,
              p.value = inputs$padj_gsea, minGSSize = inputs$minGS_gsea, maxGSSize = inputs$maxGS_gsea)
          }
          
}

get_result <- function(state, name) {
  if (name %in% names(state$res)) return(state$res[[name]])
  if (name %in% names(state$data)) return(state$data[[name]])
  NULL
}