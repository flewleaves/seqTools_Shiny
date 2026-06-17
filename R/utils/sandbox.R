# R/utils/sandbox.R
# 文件 I/O 沙箱：限制 run_r_code / execute_r 只能读写指定目录
# 原理：R 的绝大多数文件操作最终都通过 file/gzfile/bzfile/xzfile 打开连接，
# 因此只需要覆写这几个函数即可拦截几乎所有的读写。

sandbox_init <- function(allowed_read, allowed_write) {
  # 保存原始函数引用
  .orig <- list(
    file   = base::file,
    gzfile = base::gzfile,
    bzfile = base::bzfile,
    xzfile = base::xzfile,
    unlink = base::unlink
  )

  # 规范化路径列表，确保结尾有分隔符以便 startsWith 精确匹配
  norm_dirs <- function(dirs) {
    vapply(dirs, function(d) {
      p <- normalizePath(d, mustWork = FALSE, winslash = "/")
      if (!grepl("/$", p)) paste0(p, "/") else p
    }, character(1), USE.NAMES = FALSE)
  }
  read_dirs  <- norm_dirs(allowed_read)
  write_dirs <- norm_dirs(allowed_write)

  # 判断 open 模式是否涉及写入
  is_write <- function(open) {
    if (is.null(open) || open == "") return(FALSE)
    grepl("w|a", open, ignore.case = TRUE)
  }

  # 核心校验
  check_path <- function(path, mode) {
    if (!is.character(path) || !nzchar(path)) return(TRUE)

    # 跳过临时目录（R 内部大量使用 tempfile）
    tmp <- normalizePath(tempdir(), mustWork = FALSE, winslash = "/")
    if (!grepl("/$", tmp)) tmp <- paste0(tmp, "/")

    abs_path <- tryCatch(
      normalizePath(path, mustWork = FALSE, winslash = "/"),
      error = function(e) path
    )

    # tempdir 放行
    if (startsWith(abs_path, tmp)) return(TRUE)

    dirs <- if (mode == "write") write_dirs else read_dirs
    ok <- any(vapply(dirs, function(d) startsWith(abs_path, d), logical(1)))
    if (!ok) {
      stop(sprintf(
        "[沙箱] %s 被拒绝: '%s' 不在允许的目录内。\n允许%s的目录: %s",
        if (mode == "write") "写入" else "读取",
        path,
        if (mode == "write") "写入" else "读取",
        paste(dirs, collapse = ", ")
      ))
    }
    TRUE
  }

  # 覆写 file()
  assign("file", function(description = "", open = "", ...) {
    if (is.character(description) && nzchar(description)) {
      check_path(description, if (is_write(open)) "write" else "read")
    }
    .orig$file(description, open, ...)
  }, envir = parent.frame())

  # 覆写 gzfile / bzfile / xzfile
  for (fn in c("gzfile", "bzfile", "xzfile")) {
    orig_fn <- .orig[[fn]]
    body <- substitute({
      if (is.character(description) && nzchar(description)) {
        check_path(description, if (is_write(open)) "write" else "read")
      }
      ORIG(description, open, ...)
    }, list(ORIG = as.name(paste0(".orig$", fn))))
    f <- function(description = "", open = "", ...) {}
    body(f) <- body
    environment(f) <- environment()
    assign(fn, f, envir = parent.frame())
  }

  # 覆写 unlink() — 禁止删除沙箱外的文件
  assign("unlink", function(x, recursive = FALSE, force = FALSE) {
    if (is.character(x)) {
      for (p in x) {
        if (nzchar(p)) check_path(p, "write")
      }
    }
    .orig$unlink(x, recursive, force)
  }, envir = parent.frame())

  # 覆写 dir.create() — 禁止在沙箱外创建目录
  .orig_dir_create <- base::dir.create
  assign("dir.create", function(path, showWarnings = TRUE, recursive = FALSE, ...) {
    if (is.character(path) && nzchar(path)) check_path(path, "write")
    .orig_dir_create(path, showWarnings, recursive, ...)
  }, envir = parent.frame())

  invisible(NULL)
}
