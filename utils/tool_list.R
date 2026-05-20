# 工具注册表 —— 所有分析工具的唯一注册处
tools <- list(
  # ========== 数据处理 ==========
  normalize = list(
    name = "数据标准化",
    category = "数据处理", 
    omics = "bulk_rna",      
    params = function(ns,state) {
      list(
        ui = tagList(
          selectInput(ns("method"), "标准化方法",
                      choices = c("", "cpm", "rpkm", "tpm", "tmm", "vst/rlog"),
                      selected = "")
        ),
        ids = c("method")
      )
    },
    output_type = "none",         # 标准化一般只修改数据，不输出图表
    run = function(state, inputs, session, ns) {
      normalize(state, inputs, session, ns)
    },
    plot = function(state) NULL,
    table = function(state) NULL
  ),

  filter = list(
    name = "数据过滤",
    category = "数据处理",
    omics = "bulk_rna", 
    params = function(ns,state) {
      list(
        ui = tagList(
          p("数据过滤可以加强结果的显著性。注意，差异分析时也会默认进行数据过滤，无需手动过滤。"),
          p("但是，手动过滤的数据可能更符合分析要求。"),
          p("过滤对所有表达矩阵（包括count和标准化矩阵）生效"),
          numericInput(ns("min_count"), "每个基因在不同样本中表达量之和至少大于", value = 10, min = 0),
          checkboxInput(ns("exclude_noncoding"), "排除常见非编码基因", value = FALSE),
          checkboxInput(ns("exclude_unknown"), "排除未知基因（未被注释的基因）", value = FALSE)
        ),
        ids = c("min_count", "exclude_noncoding", "exclude_unknown")
      )
    },
    output_type = "none",
    run = function(state, inputs, session, ns) {
      expr_filter(state, inputs, session, ns)
    },
    plot = function(state) NULL,
    table = function(state) NULL
  ),

  # ========== 差异/富集分析 ==========
  deg = list(
    name = "差异分析",
    category = "差异/富集分析",
    omics = "bulk_rna", 
    params = function(ns,state) {
      groups <- unique(as.character(state$meta[["group_info"]]))
      if (length(groups) < 2) {
        values <- c("分组不足" = "")
      } else {
        combos <- combn(groups, 2)
        values <- c()
        for (k in 1:ncol(combos)) {
          a <- combos[1, k]; b <- combos[2, k]
          values[paste(a, "vs", b)] <- paste(a, b, sep = "-")
          values[paste(b, "vs", a)] <- paste(b, a, sep = "-")
        }
      }
      list(
        ui  =  tagList(
          selectInput(ns("deg_input"), "选择输入矩阵（推荐count）", choices = names(state$data)),
          selectInput(ns("contrast"), "选择比较组", choices = values),  
          selectInput(ns("method"), "选择分析方法", choices = c("DESeq2", "limma", "edgeR", "wilcox", "t"), selected = "DESeq2"), 
          numericInput(ns("logFC_threshold"), "logFC阈值(0为保留全部结果)", value = 0, min = 0),
          numericInput(ns("padj_threshold"), "Adjusted p阈值(1为保留全部结果)", value = 1, min = 0, max = 1),
          checkboxInput(ns("filter"), "自动过滤低表达基因", value = TRUE)
        ),
        ids = c("deg_input","contrast", "method" ,"logFC_threshold", "padj_threshold", "filter")
      )
    },
    output_type = "table",        # 结果通常是表格
    run = function(state, inputs, session, ns) {
      state$res$deg_res <- DEG_analysis_v2(state$data[[inputs$deg_input]], group = state$meta[["group_info"]], contrast = inputs$contrast,
      p.value = inputs$padj_threshold, logFC = inputs$logFC_threshold, FilterGene = inputs$filter, log_transformed = state$meta[["log_state"]])
      state$res$deg_res <- na.omit(state$res$deg_res)
      state$meta$deg_contrast <- inputs$contrast
      showNotification("差异分析完成", type = "message")
    },
    plot = function(state) NULL,
    table = function(state) {
      req(state$res$deg_res)
      DT::datatable(state$res$deg_res)
    }
  ),
  gsea = list(
      name = "GSEA分析",
      category = "差异/富集分析",
      params = function(ns, state){
        list(
          ui = tagList(
            selectInput(ns("ranking_mat"), "基因排序方法",
                      choices = c("logFC", "Signal-to-noise", "Signed-p"),
                      selected = "logFC"),
            selectInput(ns("geneset_gsea"), "参考基因集",
                      choices = c("GO", "KEGG", "Reactome"),
                      selected = "GO"),
            fileInput(ns("custom_gsea"), "GSEA数据库下载的参考基因集(.gmt)", 
                      accept = c(".gmt")),
            numericInput(ns("padj_gsea"), "Adjusted p阈值(1为保留全部结果)", value = 1, min = 0, max = 1),
            numericInput(ns("ppadj_gsea"), "绘图时Adjusted p阈值", value = 0.05, min = 0, max = 1),
            numericInput(ns("minGS_gsea"), "参与分析的最小基因集大小", value = 10, min = 0),
            numericInput(ns("maxGS_gsea"), "参与分析的最大基因集大小", value = 500, min = 0),
            numericInput(ns("top_n_gsea"), "结果图展示前", value = 10, min = 1),
          ), 
          ids = c("ranking_mat","geneset_gsea", "custom_gsea", "padj_gsea","ppadj_gsea", "minGS_gsea", "maxGS_gsea","top_n_gsea")
        )
      },
      output_type = "both",
      run = function(state, inputs, session, ns){
          run_gsea(state,inputs, session, ns)
          attr(state$res$gsea_res, "plot_params") <- list(
            topn   = inputs$top_n_gsea,
            pajust = inputs$ppadj_gsea
          )
      },
      plot = function(state) {
        req(state$res$gsea_res)
        params <- attr(state$res$gsea_res, "plot_params")
        GseaVis::dotplotGsea(
          data   = state$res$gsea_res,
          topn   = params$topn,
          pajust = params$pajust
        )$plot
      },
      table = function(state){
        req(state$res$gsea_res)
        DT::datatable(state$res$gsea_res@result)
      }
    ),

  # ========== 数据绘图 ==========
  pca = list(
    name = "PCA图",
    category = "数据绘图",
    params = function(ns, state){
      list(
        ui = tagList(
          selectInput(ns("pca_input"), "输入数据", choices = names(state$data)),
          numericInput(ns("pca_dims"), "保留维度", value = 2, min = 2),
          selectInput(ns("pca_label"), "是否添加标签", choices = c("是" = "ind", "否" = "none"), selected = "否"),
          selectInput(ns("pca_eclip"), "是否使用椭圆框选数据点", choices = c(TRUE, FALSE), selected = TRUE),
          textAreaInput(ns("pca_color"), "分组颜色(HEX码),半角分号隔开", value = "#00AFBB;#E7B800;#FC4E07", rows = 2),
          
        ),
        ids = c("pca_input", "pca_dims", "pca_label", "pca_eclip", "pca_color")
      )
    },               
    output_type = "plot",
    run = function(state, inputs, session, ns) {
      tryCatch({state$res$pca_res <- seqTools::draw_pca(state$data[[inputs$pca_input]], group = state$meta$group_info, 
      ncp = inputs$pca_dims, label = inputs$pca_label, addEllipses = as.logical(inputs$pca_eclip), palette = read_vector(inputs$pca_color,state))},
      error = function(e){showNotification(paste("失败:", e$message), type = "error", duration = NULL)})
    },
    plot = function(state) {
      req(state$res$pca_res)
      state$res$pca_res
    },
     table = function(state) NULL
  ),

  volcano = list(
    name = "火山图",
    category = "数据绘图",
    params = function(ns, state){
      list(
        ui = tagList(
          numericInput(ns("vol_p"), "Adjust p值阈值", value = 0.05, min = 0, max = 1),
          numericInput(ns("vol_lfc"), "logFC阈值", value = 1, min = 0),
          textAreaInput(ns("vol_color"), "下调/ns/上调颜色(HEX码),半角分号隔开", value = "#6a82ed;grey;#ed6f6f", rows = 2),
          numericInput(ns("vol_al"), "透明度，范围从 0（完全透明）到 1（完全不透明）", value = 0.3, min = 0, max = 1),
          numericInput(ns("vol_size"), "点的大小", value = 2, min = 0),
          textAreaInput(ns("vol_labels"), "标记的基因名，半角分号隔开（可选）", rows = 3),
          textAreaInput(ns("vol_highlight"), "高亮的基因名，半角分号隔开（可选）", rows = 3),
          textAreaInput(ns("vol_hcolor"), "高亮的基因颜色", rows = 1, value = "#07a818"),
          numericInput(ns("vol_maxo"), "最大标记重叠数", value = 10, min = 10),
        ),
        ids = c("vol_p", "vol_lfc", "vol_color", "vol_al", "vol_size", "vol_labels", "vol_highlight", "vol_hcolor", "vol_maxo")
      )
    },  
    output_type = "plot",
    run = function(state, inputs, session, ns){
        if (!check(state, "deg_res")) {
          showNotification("请先进行差异分析", type = "error")
          return()
        }
        tryCatch({state$res$vol_res <- seqTools::draw_Volcano(state$res$deg_res, p.value = inputs$vol_p, logFC = inputs$vol_lfc,
          color = read_vector(inputs$vol_color,state), alpha = inputs$vol_al, size = inputs$vol_size, label = read_vector(inputs$vol_labels,state, TRUE), highlight = read_vector(inputs$vol_highlight,state, TRUE),
          highlight_color = inputs$vol_hcolor, max.overlaps = inputs$vol_maxo)}, error = function(e){showNotification(paste("失败:", e$message), type = "error", duration = NULL)}) 
    },  
    plot = function(state) {
        req(state$res$vol_res)
        state$res$vol_res
    },
     table = function(state) NULL
  ),
  gseaPlot = list(
    name = "GSEA通路图",
    category = "数据绘图",
    params = function(ns, state){
      list(
        ui = tagList(
          textAreaInput(ns("id_gsea"), "目标通路名称", rows = 2),
          numericInput(ns("np_gsea"), "显示几张分图", value = 3, min = 0, max = 3),
          selectInput(ns("addp_gsea"), "是否在结果图中添加p值和富集分数", choices = c(TRUE, FALSE), selected = TRUE),
          numericInput(ns("x_gsea"), "p值标签X坐标", value = 0.95),
          numericInput(ns("y_gsea"), "p值标签Y坐标", value = 0.8),
        ),
        ids = c("id_gsea","np_gsea","addp_gsea","x_gsea","y_gsea")
      )
    },  
    output_type = "plot",
    run = function(state, inputs, session, ns){
        if (!check(state, "gsea_res")) {
          showNotification("请先进行GSEA分析", type = "error")
          return()
        }
        state$res$gsea_plot <- GseaVis::gseaNb(
          object= state$res$gsea_res,
          geneSetID= inputs$id_gsea,
          subPlot= inputs$np_gsea, 
          addPval= as.logical(inputs$addp_gsea), 
          pvalX= inputs$x_gsea,
          pvalY= inputs$y_gsea 
        )
    },  
    plot = function(state) {
        req(state$res$gsea_res)
        state$res$gsea_plot
    },
     table = function(state) NULL
  ),

  # ========== 表格查看 ==========
  count_table = list(
    name = "Count矩阵",
    category = "表格查看",
    params = NULL,
    output_type = "table",
    run = function(state, inputs, session, ns) NULL,
    plot = function(state) {
      req(state$data[["count"]])
      DT::datatable(state$data[["count"]])
    }
  ),

  norm_table = list(
    name = "标准化矩阵",
    category = "表格查看",
    params = NULL,
    output_type = "table",
    run = function(state, inputs, session, ns) NULL,
    plot = function(state) {
      req(state$data)
      DT::datatable(state$data)
    }
  )
)

# 分类配置 —— 控制 accordion 面板显示哪些工具
tool_categories <- split(
  names(tools),                    # ← 工具 ID：normalize, filter, deg...
  sapply(tools, `[[`, "category")  # ← 按 category 分组
)


