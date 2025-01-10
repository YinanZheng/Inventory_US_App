# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shiny.fluent)

library(dplyr)
library(DT)
library(stringi)
library(baRcodeR)
library(DBI)
library(RMariaDB)
library(uuid)
library(plotly)
library(openxlsx)
library(lubridate)
library(tidyr)
library(tesseract)
library(pdftools)
library(later)

# Source all modular functions
source("./modules/typeModuleUI.R", local = TRUE)
source("./modules/uniqueItemsTableUI.R", local = TRUE)
source("./modules/imageModuleUI.R", local = TRUE)
source("./modules/orderTableUI.R", local = TRUE)
source("./modules/itemFilterUI.R", local = TRUE)
source("./modules/autocompleteInputUI.R", local = TRUE)

source("./modules/supplierModuleServer.R", local = TRUE)
source("./modules/typeModuleServer.R", local = TRUE)
source("./modules/uniqueItemsTableServer.R", local = TRUE)
source("./modules/imageModuleServer.R", local = TRUE)
source("./modules/orderTableServer.R", local = TRUE)
source("./modules/itemFilterServer.R", local = TRUE)
source("./modules/autocompleteInputServer.R", local = TRUE)

source("utils.R", local = TRUE)

# 定义轮询间隔（以毫秒为单位）
poll_interval <<- 10000  # 每 10 秒检查一次

# 主机URL
host_url <<- "http://54.254.120.88/"

# 通用物品表的列名
placeholder_300px_path <<- "https://dummyimage.com/300x300/cccccc/000000.png&text=No+Image"
placeholder_150px_path <<- "https://dummyimage.com/150x150/cccccc/000000.png&text=No+Image"
placeholder_50px_path <<- "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image"

# Size of barcode paper (in cm)
page_width <<- 4
page_height <<- 2
size_unit <<- "cm"

# 通用物品表显示列
common_columns <<- list(
  SKU = "条形码",
  ItemName = "商品名",
  ItemImagePath = "商品图",
  Maker = "供应商",
  # MajorType = "大类",
  # MinorType = "小类",
  ProductCost = "单价",
  # DomesticShippingCost = "平摊运费",
  Status = "库存态"
  # Defect = "瑕疵态"
)

# 定义需要记录时间的状态
status_columns <<- list(
  "采购" = "PurchaseTime",
  "国内入库" = "DomesticEntryTime",
  "国内出库" = "DomesticExitTime",
  "国内售出" = "DomesticSoldTime",
  "美国入库" = "UsEntryTime",
  "美国售出" = "UsSoldTime",
  "美国调货" = "UsRelocationTime",
  "退货" = "ReturnTime"
)

table_default_options <<- list(
  scrollY = "730px",
  scrollX = TRUE,
  fixedHeader = TRUE,
  paging = TRUE,
  pageLength = 30,
  dom = 'frtip',
  searching = FALSE
)

# 定义瑕疵和修复的状态
defect_statuses <<- c("瑕疵", "修复", "无瑕")

# 自定义函数
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# 定义管理员密码
admin_password <<- "1029"
