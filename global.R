# global.R
library(shiny)
library(shinyjs)
library(bslib)
library(configr)
library(DT)
library(seqTools)

# 上传文件大小限制（单细胞数据文件较大，设为 50GB）
options(shiny.maxRequestSize = 50000 * 1024^2)
library(shinyWidgets)
library(shinyalert)
library(geneSync)
library(ggraph)
library(httr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(R6)

# ---------- 辅助函数 ----------
`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path(APP_ROOT, "R", "utils", "tool_helpers.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "utils", "ui_helpers.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "utils", "sc_helpers.R"), local = TRUE)

# ---------- 核心层（R6 状态 + 注册表 + 引擎） ----------
source(file.path(APP_ROOT, "R", "core", "project.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "core", "import_registry.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "core", "registry.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "core", "engine.R"), local = TRUE)

# ---------- 纯计算引擎层（engines） ----------
source(file.path(APP_ROOT, "R", "engines", "normalize.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "deg.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "pca.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "volcano.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "gsea.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "filter.R"), local = TRUE)
source(file.path(APP_ROOT, "R", "engines", "sc_engine.R"), local = TRUE)

# ---------- 动态加载导入方法与工具 ----------
ImportRegistry$load_directory(file.path(APP_ROOT, "R", "imports"))
ToolRegistry$load_directory(file.path(APP_ROOT, "R", "tools"))

# ---------- 技能（Skill）系统 ----------
source(file.path(APP_ROOT, "R", "skills", "registry.R"), local = TRUE)
SkillRegistry$load_directory(file.path(APP_ROOT, "R", "skills"))

# ---------- 创建全局 AnalysisEngine（Shiny + Python + AI 共用唯一状态源） ----------
engine <- AnalysisEngine$new()
for (m in ImportRegistry$methods) {
  engine$project$register_import(m$id, m$name, m$schema, m$run)
}

# ---------- 工具热加载函数 ----------
reload_tools <- function(path = file.path(APP_ROOT, "R", "tools")) {
  ToolRegistry$reload(path)
  SkillRegistry$reload(file.path(APP_ROOT, "R", "skills"))
  message("[reload_tools] 已重新加载，当前工具: ", paste(names(ToolRegistry$tools), collapse = ", "),
          " | 技能: ", paste(names(SkillRegistry$skills), collapse = ", "))
}

# ---------- 配置与状态 ----------
source(file.path(APP_ROOT, "utils", "config.R"), local = TRUE)
source(file.path(APP_ROOT, "utils", "state.R"), local = TRUE)

# ---------- AI 工具（依赖 engine，必须放在 engine 创建之后） ----------
source(file.path(APP_ROOT, "utils", "ai.R"), local = TRUE)

# ---------- UI 模块 ----------
source(file.path(APP_ROOT, "modules", "project.R"), local = TRUE)
source(file.path(APP_ROOT, "modules", "data.R"), local = TRUE)
source(file.path(APP_ROOT, "modules", "analysis.R"), local = TRUE)
source(file.path(APP_ROOT, "modules", "settings.R"), local = TRUE)
source(file.path(APP_ROOT, "modules", "ai_console.R"), local = TRUE)