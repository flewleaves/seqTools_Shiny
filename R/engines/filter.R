# R/engines/filter.R
# 纯计算层：数据过滤引擎

#' 表达矩阵过滤引擎
#' @param mats       矩阵列表（list of matrix/data.frame）
#' @param min_count  最小表达量阈值
#' @param exclude_noncoding 是否排除非编码基因
#' @param exclude_unknown   是否排除未注释基因
#' @param species    物种
#' @param id_type    基因ID类型
#' @return list(mats=过滤后列表, note=日志, stats=过滤统计)
engine_filter <- function(mats, min_count = 10,
                          exclude_noncoding = FALSE, exclude_unknown = FALSE,
                          species = "Hs", id_type = "SYMBOL") {
  require(stringr)

  stats <- list()
  original_rows <- sapply(mats, nrow)

  # 获取已知基因列表
  kg <- NULL
  if (exclude_unknown) {
    if (species == "Mm") {
      require(org.Mm.eg.db)
      kg <- keys(org.Mm.eg.db, keytype = id_type)
    } else {
      require(org.Hs.eg.db)
      kg <- keys(org.Hs.eg.db, keytype = id_type)
    }
  }

  # 非编码基因正则
  nc_pattern <- NULL
  if (exclude_noncoding) {
    nc_pattern <- paste0(
      "(?i)^(LINC|MIR|SNORD|SNORA|RNU|Gm\\d+|",
      "MALAT1|NEAT1|XIST|HOTAIR|TUG1|GAS5|H19|KCNQ1OT1|UCA1|PVT1|",
      ")|",
      "(-AS\\d*$|-IT\\d*$|-Rik$|-Ps\\d*$)"
    )
  }

  for (idx in seq_along(mats)) {
    tp <- mats[[idx]]
    if (is.null(tp) || (!is.matrix(tp) && !is.data.frame(tp))) next

    # 1. 低表达过滤
    tp <- tp[rowSums(tp) > min_count, , drop = FALSE]

    # 2. 非编码/未知过滤
    if (exclude_noncoding && !is.null(nc_pattern)) {
      tp <- tp[!stringr::str_detect(row.names(tp), nc_pattern), , drop = FALSE]
    } else if (exclude_unknown && !is.null(kg)) {
      tp <- tp[row.names(tp) %in% kg, , drop = FALSE]
    }

    mats[[idx]] <- tp
  }

  filtered_rows <- sapply(mats, nrow)
  stats <- mapply(function(orig, filt, name) {
    paste(name, ":", orig, "->", filt, "(保留", round(filt/orig*100, 1), "%)")
  }, original_rows, filtered_rows, names(mats), SIMPLIFY = FALSE)

  list(
    mats = mats,
    note = "数据过滤完成",
    stats = unlist(stats)
  )
}
