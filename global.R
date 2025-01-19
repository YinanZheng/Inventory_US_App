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
library(plotly)
library(networkD3)
library(openxlsx)
library(tidyr)
library(lubridate)

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

# 通用账务表列名映射
transaction_common_columns <- list(
  TransactionTime = "转账时间",
  AmountIn = "转入金额",
  AmountOut = "转出金额",
  Balance = "当前余额",
  TransactionImagePath = "转账截图",
  Remarks = "备注"
)

# 通用订单表列名映射
orders_table_columns <<- list(
  OrderID = "订单号",
  OrderImagePath = "订单图",
  CustomerName = "姓名",
  CustomerNetName = "网名",
  Platform = "平台",
  UsTrackingNumber = "运单号",
  LabelStatus = "运单PDF",
  OrderStatus = "状态",
  OrderNotes = "备注",
  created_at = "创建时间"
)

# 定义需要记录时间的状态
status_columns <<- list(
  "采购" = "PurchaseTime",
  "国内入库" = "DomesticEntryTime",
  "国内出库" = "DomesticExitTime",
  "国内售出" = "DomesticSoldTime",
  "美国入库" = "UsEntryTime",
  "美国发货" = "UsShippingTime",
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
  searching = FALSE,
  language = list(search = "搜索：")
)

# 定义瑕疵和修复的状态
defect_statuses <<- c("瑕疵", "修复", "无瑕")

# 定义管理员密码
admin_password <<- "1029"

client_id <<- "NzfGxVtsAiw6Y0IIYpg8ivmjHiTWuZJqH3ZOdTaLSPJpSm5H"
client_secret <<- "h1LCAMKRiwa0hqaBpRTRZZeFdLXJGvibmGhM3W7YVaO7vBfWoHV7MRGA4nED0GKD"