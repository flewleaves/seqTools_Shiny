# R/utils/ui_helpers.R

#' 将工具/导入方法的 schema 转换为 Shiny UI 控件
schema_to_ui <- function(schema, ns, state) {
  if (length(schema) == 0) return(NULL)

  # isolate 切断响应式依赖——modal 弹出时读一次快照，不随 state$data 轮询重执行
  data_keys   <- isolate(strsplit(state$data_keys %||% "", ",")[[1]])
  data_keys   <- data_keys[nzchar(data_keys)]
  result_keys <- isolate(strsplit(state$res_keys %||% "", ",")[[1]])
  result_keys <- result_keys[nzchar(result_keys)]
  vis_result_keys <- if (length(result_keys) > 0)
    result_keys[!startsWith(result_keys, "vis_") & !startsWith(result_keys, "_")]
  else character(0)
  group_info  <- isolate(state$meta$group_info)

  controls <- lapply(names(schema), function(id) {
    def     <- schema[[id]]
    type    <- def$type %||% "string"
    label   <- def$description %||% id
    choices <- def$choices

    # ---------- 动态填充 choices ----------
    if (type == "select" && (is.null(choices) || length(choices) == 0 ||
                             identical(choices, c("")) || identical(choices, ""))) {
      choices <- switch(id,
        # 数据矩阵选择
        deg_input = ,
        pca_input = ,
        normalize_input = {
          if (length(data_keys) > 0) data_keys else c("暂无数据" = "")
        },

        # 结果查看：只显示可可视化的对象
        vis = {
          can_show <- function(key) {
            obj <- engine$project$results[[key]] %||% engine$project$data[[key]]
            if (is.null(obj) || is.environment(obj)) return(FALSE)
            if (inherits(obj, c("gg","ggplot","gseaResult","Seurat","data.frame","matrix","dgCMatrix"))) return(TRUE)
            if (is.list(obj)) return(FALSE)
            FALSE
          }
          vis_data <- data_keys[!startsWith(data_keys, "_")]
          vis_res <- vis_result_keys[!startsWith(vis_result_keys, "_")]
          vis_res <- vis_res[sapply(vis_res, can_show)]
          all_keys <- c(
            if (length(vis_data) > 0) setNames(vis_data, paste0("[数据] ", vis_data)),
            if (length(vis_res) > 0)  setNames(vis_res,  paste0("[结果] ", vis_res))
          )
          if (length(all_keys) > 0) all_keys else c("暂无数据" = "")
        },

        # meta.data 列名（动态读取 Seurat 对象）
        target_group = {
          cl <- engine$project$meta$sc_cluster_ids
          if (is.null(cl)) {
            cl <- tryCatch({
              sc_key <- sc_find_seurat_key(engine$project, NULL)
              sobj <- sc_load_from_disk(engine$project, sc_key)
              if (inherits(sobj, "Seurat")) {
                cl <- sort(unique(as.character(Idents(sobj))))
                cl <- cl[!is.na(cl) & nzchar(cl)]
                engine$project$meta$sc_cluster_ids <- cl
              } else NULL
            }, error = function(e) NULL)
          }
          if (length(cl) > 0) setNames(cl, cl) else c("暂无数据" = "")
        },
        group_by = ,
        split_by = ,
        anno_by = ,
        ident_group = ,
        cluster_col = {
          # 只显示分类列（唯一值 ≤ 50），避免 nCount_RNA 等连续变量
          cols <- sc_get_categorical_cols(engine$project)
          if (length(cols) > 0) cols else c("暂无数据" = "")
        },

        # 对比组
        contrast = {
          if (!is.null(group_info) && length(unique(group_info)) >= 2) {
            groups <- unique(as.character(group_info))
            combos <- combn(groups, 2)
            ch <- c()
            for (k in seq_len(ncol(combos))) {
              a <- combos[1, k]; b <- combos[2, k]
              ch[paste(a, "vs", b)] <- paste(a, b, sep = "-")
              ch[paste(b, "vs", a)] <- paste(b, a, sep = "-")
            }
            ch
          } else {
            c("请先导入含分组信息的数据" = "")
          }
        },

        # 默认：保留原 choices 或空
        if (length(choices) > 0 && !identical(choices, c(""))) choices
        else c("暂无数据" = "")
      )
    }

    # ---------- 生成控件 ----------
    if (type == "select") {
      if (is.null(choices) || length(choices) == 0) choices <- c("暂无数据" = "")
      # 非必填 select 首项加入「全部」，传入 "All_Cells"，tool 层临时添加 meta.data 列
      # 使用实际列名而非空字符串，兼容 Shiny selectInput 且无需特殊过滤
      if (!isTRUE(def$required) && !isTRUE(def$multiple))
        choices <- c("全部 (不分组)" = "All_Cells", choices)
      # cascade_from：先用占位控件，由 analysis.R 的 observer 动态填充
      if (!is.null(def$cascade_from)) {
        placeholder <- c("请先选择分组列" = "")
        if (isTRUE(def$multiple)) {
          return(checkboxGroupInput(ns(id), label,
            choiceNames = names(placeholder), choiceValues = unname(placeholder)))
        } else {
          return(selectInput(ns(id), label, choices = placeholder))
        }
      }
      if (isTRUE(def$multiple)) {
        if (is.null(names(choices))) choices <- setNames(choices, choices)
        checkboxGroupInput(ns(id), label,
          choiceNames  = names(choices),
          choiceValues = unname(choices),
          selected = def$default)
      } else {
        selectInput(ns(id), label,
                    choices  = choices,
                    selected = def$default %||% choices[[1]])
      }

    } else if (type == "boolean") {
      checkboxInput(ns(id), label,
                    value = as.logical(def$default %||% FALSE))

    } else if (type %in% c("number", "integer")) {
      step <- if (type == "integer") 1 else 0.1
      args <- list(inputId = ns(id), label = label,
                   value = if (type == "integer") as.integer(def$default %||% 0)
                           else as.numeric(def$default %||% 0),
                   step = step)
      if (!is.null(def$min)) args$min <- def$min
      if (!is.null(def$max)) args$max <- def$max
      do.call(numericInput, args)

    } else if (type == "text") {
      textAreaInput(ns(id), label, value = def$default %||% "", rows = 2)

    } else if (type == "file") {
      fileInput(ns(id), label,
                accept = c(".gmt", ".csv", ".txt", ".tsv", ".gz", ".mtx"))

    } else {
      textInput(ns(id), label, value = def$default %||% "")
    }
  })

  do.call(tagList, controls)
}

#' 创建 mock session（用于 AI 工具调用）
create_mock_session <- function() {
  list(
    sendCustomMessage  = function(type, message) invisible(NULL),
    sendInputMessage   = function(inputId, message) invisible(NULL),
    sendNotification   = function(type, message) invisible(NULL),
    onSessionEnded     = function(callback) invisible(NULL),
    ns                 = function(id) id
  )
}