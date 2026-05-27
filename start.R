# start.R —— seqTools 统一启动器
# 用法: Rscript start.R    或    在 RStudio 里 source("start.R")

# ---------- 获取脚本所在目录 ----------
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    dirname(normalizePath(sys.frames()[[1]]$ofile))
  } else {
    getwd()
  }
}

APP_ROOT <- script_dir()

# ---------- 检查 R 包 ----------
required_pkgs <- c("shiny", "shinyjs", "bslib", "configr", "DT", "seqTools",
                   "shinyWidgets", "shinyalert", "geneSync", "ggraph", "httr", "stringr", "ggplot2", "ggrepel",
                   "processx")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  message("正在安装缺失 R 包: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org/")
}
library(processx)

# ---------- 查找 Python ----------
find_python <- function() {
  conda_py <- Sys.getenv("CONDA_PYTHON_EXE", "")
  if (conda_py != "" && file.exists(conda_py)) return(conda_py)
  for (cmd in c("python", "python3")) {
    path <- Sys.which(cmd)
    if (path != "") {
      ver <- tryCatch(system2(path, "--version", stdout = TRUE, stderr = TRUE), error = function(e) "")
      if (grepl("Python 3", paste(ver, collapse = ""))) return(path)
    }
  }
  stop("未找到 Python 3，请检查环境变量或安装 Python")
}

py_cmd <- find_python()
message("使用 Python: ", py_cmd)

# ---------- 安装 Python 依赖 ----------
# 找 pip：优先用同目录下的 pip，venv 可能没把 pip 加到 PATH
find_pip <- function(py) {
  # 同目录下的 pip / pip3
  py_dir <- dirname(py)
  for (pip_name in c("pip", "pip3")) {
    pip_path <- file.path(py_dir, pip_name)
    if (file.exists(pip_path)) return(pip_path)
    pip_path_exe <- file.path(py_dir, paste0(pip_name, ".exe"))
    if (file.exists(pip_path_exe)) return(pip_path_exe)
  }
  # 回退：python -m pip
  return(NULL)
}

run_pip <- function(py, args) {
  pip <- find_pip(py)
  if (!is.null(pip)) {
    system2(pip, args, stdout = FALSE, stderr = FALSE)
  } else {
    system2(py, c("-m", "pip", args), stdout = FALSE, stderr = FALSE)
  }
}

check_and_install_py_deps <- function(py) {
  check_code <- "import fastapi, uvicorn, httpx, websockets; print('ok')"
  result <- tryCatch(
    system2(py, c("-c", check_code), stdout = TRUE, stderr = FALSE),
    error = function(e) ""
  )
  if (!identical(trimws(paste(result, collapse = "")), "ok")) {
    message("正在安装 Python 依赖...")
    req_file <- file.path(APP_ROOT, "python", "requirements.txt")
    if (file.exists(req_file)) {
      run_pip(py, c("install", "-r", req_file))
    }
    # 确保 uvicorn[standard] 和 websockets 存在（WebSocket 支持必需）
    run_pip(py, c("install", "--upgrade", "uvicorn[standard]", "websockets"))
    message("Python 依赖安装完成")
  } else {
    message("Python 依赖已就绪")
  }
}

check_and_install_py_deps(py_cmd)

# ---------- 启动 Python 后端 ----------
message("正在启动 Python AI 后端...")
py_proc <- process$new(
  command = py_cmd,
  args = c("-m", "uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8765"),
  wd = file.path(APP_ROOT, "python"),
  stdout = "|", stderr = "|",
  env = c(Sys.getenv(), PYTHONUNBUFFERED = "1"),
  cleanup = TRUE
)

# 后台读取输出并实时 cat 到 R 控制台
cat_python_output <- function(proc) {
  later::later(function() {
    if (proc$is_alive()) {
      out <- proc$read_output()
      err <- proc$read_error()
      if (nzchar(out)) cat("PY:", out, "\n")
      if (nzchar(err)) cat("PY ERR:", err, "\n")
      cat_python_output(proc)  # 递归调度
    }
  }, 0.1)
}
cat_python_output(py_proc)
# 等待端口就绪（最多 10 秒）
started <- FALSE
for (i in 1:20) {
  Sys.sleep(0.5)
  if (!py_proc$is_alive()) break
  ready <- tryCatch({
    con <- socketConnection("127.0.0.1", port = 8765, timeout = 1, open = "r")
    close(con); TRUE
  }, error = function(e) FALSE)
  if (ready) { started <- TRUE; break }
}

if (!py_proc$is_alive()) {
  stop("Python 后端启动失败:\n", py_proc$read_all_error())
}
if (started) {
  message("✅ Python AI 后端已启动 (PID: ", py_proc$get_pid(), ")")
} else {
  message("警告: 端口 8765 未在 10 秒内就绪，仍继续启动 Shiny...")
}

# ---------- 启动 Shiny，退出时关闭 Python ----------
# 用 tryCatch 而不是 on.exit，避免 source() 模式下提前触发
message("🚀 启动 Shiny 应用...")
tryCatch(
  shiny::runApp(appDir = APP_ROOT),
  finally = {
    message("正在关闭 Python 后端...")
    try(py_proc$kill(), silent = TRUE)
  }
)

