# R/core/import_registry.R
library(R6)

ImportRegistry <- R6Class("ImportRegistry",
  public = list(
    methods = list(),

    register = function(method_def) {
      self$methods[[method_def$id]] <- method_def
    },

    get = function(id) {
      self$methods[[id]]
    },

    load_directory = function(path) {
      files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
      loaded <- character(0)
      for (f in files) {
        env <- new.env()
        tryCatch({
          source(f, local = env)
          if (all(c("IMPORT_ID", "IMPORT_NAME", "IMPORT_SCHEMA", "IMPORT_RUN") %in% ls(env))) {
            self$register(list(
              id = env$IMPORT_ID, name = env$IMPORT_NAME,
              schema = env$IMPORT_SCHEMA, run = env$IMPORT_RUN
            ))
            loaded <- c(loaded, env$IMPORT_ID)
          }
        }, error = function(e) {
          message("[ImportRegistry] 加载失败: ", basename(f), " - ", conditionMessage(e))
        })
      }
      message("[ImportRegistry] 已加载 ", length(loaded), " 个导入方法: ", paste(loaded, collapse = ", "))
      invisible(loaded)
    },

    list_methods = function() {
      lapply(names(self$methods), function(n) {
        m <- self$methods[[n]]
        list(id = m$id, name = m$name, schema = m$schema)
      })
    }
  )
)

ImportRegistry <- ImportRegistry$new()
