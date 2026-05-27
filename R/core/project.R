# R/core/project.R
library(R6)

`%||%` <- function(x, y) if (is.null(x)) y else x

Project <- R6Class("Project",
  public = list(
    name = NULL,
    omics_type = NULL,
    data = list(),
    meta = list(),
    results = list(),
    history = list(),
    settings = list(),
    import_registry = list(),

    initialize = function(name = "未命名", settings = NULL) {
      self$name <- name
      self$settings <- settings %||% list(
        norm_method = "log2", pval_cut = 0.05, logfc_cut = 1,
        seed = 42, dup = "mean"
      )
    },

    to_list = function() {
      list(
        name = self$name,
        omics_type = self$omics_type %||% "未设置",
        data = lapply(self$data, function(x) {
          if (is.matrix(x) || is.data.frame(x)) {
            list(type = class(x)[1], dims = dim(x),
                 colnames = head(colnames(x), 10), rownames = head(rownames(x), 10))
          } else if (inherits(x, "Seurat")) {
            list(type = "Seurat", dims = c(
              x@assays$RNA@counts %>% nrow(),
              x@assays$RNA@counts %>% ncol()
            ))
          } else {
            list(type = class(x)[1])
          }
        }),
        meta = self$meta,
        results = names(self$results),
        history = tail(self$history, 10)
      )
    },

    # 完整序列化（用于 saveRDS）
    serialize = function() {
      list(
        name = self$name,
        omics_type = self$omics_type,
        data = self$data,
        meta = self$meta,
        results = self$results,
        history = self$history,
        settings = self$settings
        # import_registry 不序列化（运行时重新加载）
      )
    },

    # 从序列化数据恢复
    deserialize = function(obj) {
      self$name <- obj$name %||% "未命名"
      self$omics_type <- obj$omics_type
      self$data <- obj$data %||% list()
      self$meta <- obj$meta %||% list()
      self$results <- obj$results %||% list()
      self$history <- obj$history %||% list()
      self$settings <- obj$settings %||% list()
      invisible(self)
    },

    register_import = function(id, name, schema, run_fn) {
      self$import_registry[[id]] <- list(id = id, name = name, schema = schema, run = run_fn)
    },

    import_data = function(method_id, file_path, params = list()) {
      method <- self$import_registry[[method_id]]
      if (is.null(method)) stop("导入方法不存在: ", method_id)

      result <- method$run(file_path, params)

      if (!is.null(result$data)) {
        for (nm in names(result$data)) self$data[[nm]] <- result$data[[nm]]
      }
      if (!is.null(result$meta)) {
        self$meta <- modifyList(self$meta, result$meta)
      }
      if (!is.null(result$omics_type)) {
        self$omics_type <- result$omics_type
      }

      private$log("import", list(
        method = method_id,
        file = basename(file_path),
        dims = lapply(result$data, dim)
      ))
      invisible(self)
    },

    add_result = function(name, data) {
      self$results[[name]] <- data
      private$log("result", list(name = name))
    },

    get_result = function(name) {
      self$results[[name]]
    },

    # 批量设置数据（用于 filter 等修改多个矩阵的工具）
    set_data = function(data_list) {
      for (nm in names(data_list)) {
        self$data[[nm]] <- data_list[[nm]]
      }
      private$log("set_data", list(names = names(data_list)))
      invisible(self)
    }
  ),

  private = list(
    log = function(action, details) {
      self$history <- append(self$history, list(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        action = action,
        details = details
      ))
    }
  )
)
