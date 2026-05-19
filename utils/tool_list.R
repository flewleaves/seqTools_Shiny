# 工具注册表 —— 所有分析工具的唯一注册处
tools <- list(
  # ========== 数据处理 ==========
  normalize = list(
    name = "数据标准化",
    category = "数据处理",       
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
    plot = function(state) NULL
  ),

  filter = list(
    name = "数据过滤",
    category = "数据处理",
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
    plot = function(state) NULL
  ),

  # ========== 差异/富集分析 ==========
  deg = list(
    name = "差异分析",
    category = "差异/富集分析",
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
          selectInput(ns("input_data"), "选择输入矩阵（推荐count）", choices = names(state$data)),
          selectInput(ns("contrast"), "选择比较组", choices = values),  
          selectInput(ns("method"), "选择分析方法", choices = c("DESeq2", "limma", "edgeR", "wilcox", "t"), selected = "DESeq2"), 
          numericInput(ns("logFC_threshold"), "logFC阈值", value = 1, min = 0),
          numericInput(ns("padj_threshold"), "Adjusted p阈值", value = 0.05, min = 0, max = 1),
          checkboxInput(ns("filter"), "自动过滤低表达基因", value = TRUE)
        ),
        ids = c("input_data","contrast", "method" ,"logFC_threshold", "padj_threshold", "filter")
      )
    },
    output_type = "table",        # 结果通常是表格
    run = function(state, inputs, session, ns) {
      print(colnames(state$data[[inputs$input_data]]))
      print(state$meta[["group"]])
      state$deg_res <- DEG_analysis_v2(state$data[[inputs$input_data]], group = state$meta[["group_info"]], contrast = inputs$contrast,
      p.value = inputs$padj_threshold, logFC = inputs$logFC_threshold, FilterGene = inputs$filter, log_transformed = state$meta[["log_state"]])
      showNotification("差异分析完成", type = "message")
    },
    plot = function(state) {
      req(state$deg_res)
      DT::datatable(state$deg_res)
    }
  ),

  # ========== 数据绘图 ==========
  pca = list(
    name = "PCA图",
    category = "数据绘图",
    params = NULL,                # 无参数
    output_type = "plot",
    run = function(state, inputs) {
      req(state$data)
      state$pca_res <- prcomp(t(state$data))
    },
    plot = function(state) {
      req(state$pca_res)
      ggplot(data.frame(PC1 = state$pca_res$x[,1],
                       PC2 = state$pca_res$x[,2]),
             aes(x = PC1, y = PC2)) + geom_point() + theme_minimal()
    }
  ),

  volcano = list(
    name = "火山图",
    category = "数据绘图",
    params = NULL,
    output_type = "plot",
    run = function(state, inputs) NULL,  # 直接利用 deg_res 画图
    plot = function(state) {
      req(state$deg_res)
      ggplot(state$deg_res, aes(x = log2FoldChange, y = -log10(pvalue))) +
        geom_point() + theme_minimal()
    }
  ),

  # ========== 表格查看 ==========
  count_table = list(
    name = "Count矩阵",
    category = "表格查看",
    params = NULL,
    output_type = "table",
    run = function(state, inputs) NULL,
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
    run = function(state, inputs) NULL,
    plot = function(state) {
      req(state$data)
      DT::datatable(state$data)
    }
  )
)

# 分类配置 —— 控制 accordion 面板显示哪些工具
tool_categories <- list(
  "数据处理"      = c("normalize", "filter"),
  "差异/富集分析" = c("deg"),
  "数据绘图"      = c("pca", "volcano"),
  "表格查看"      = c("count_table", "norm_table")
)



