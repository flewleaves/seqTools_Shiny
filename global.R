library(shiny)
library(bslib)
library(DT)
library(seqTools)
library(shinyWidgets)

# source 所有文件（按依赖顺序）
source("utils/state.R")
source("utils/data_list.R")
source("utils/data_read.R")
source("utils/tool_list.R")
source("utils/processing_tool.R")
source("utils/plot.R")
source("modules/project.R")
source("modules/data.R")
source("modules/analysis.R")
source("modules/settings.R")

