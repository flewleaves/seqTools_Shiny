check <- function(state, check_list){
  
  for(i in check_list){
     if(!i %in% names(data)){
       showNotification(paste0("失败:未读入", i), type = "error")
       return(FALSE)
     }
  }
  return(TRUE)
    
}


normalize <- function(state, inputs, session, ns) {
  # 1. 基础检查
  if (!check(state, "count")) {
    return()
  }

  # 2. 如果需要 TPM 但没有基因长度 → 弹窗交互，中断本次运行
  if (inputs$method == "TPM" && is.null(state$gene.length)) {
    showModal(modalDialog(
      title = "缺少基因长度",
      "是否使用标准基因长度进行 TPM 标准化？（可能引入误差）",
      footer = tagList(
        actionButton(ns("skip_length"), "否"),
        actionButton(ns("get_length"), "是", class = "btn-primary")
      )
    ))

    observeEvent(input$get_length, {
      state$gene.length <- seqTools::get_standard_gene_length(
        state$data[["count"]], species = state$meta[["species"]], from = state$meta[["id_type"]]
      )
      removeModal()
      tryCatch({
        state$data[[toupper(inputs$method)]] <- seqTools::normalization(
          state$data[["count"]],
          method = inputs$method,
          group = state$meta[["group_info"]],
          gene.length = state$gene.length
        )
        showNotification("标准化完成", type = "message")
      }, error = function(e) {
        showNotification(e$message, type = "error")
      })
    }, once = TRUE)

    observeEvent(input$skip_length, {
      removeModal()
      # 用户取消，什么都不做
    }, once = TRUE)

    return()  # 直接返回，不执行下面的标准化
  }

  # 3. 正常执行标准化
  tryCatch({
    state$data[[toupper(inputs$method)]] <- seqTools::normalization(
      state$data[["count"]],
      method = inputs$method,
      group = state$meta[["group_info"]],
      gene.length = state$gene.length
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
  if(u && state$meta[["species"]] == "Mouse"){
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