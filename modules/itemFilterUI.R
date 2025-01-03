itemFilterUI <- function(id, border_color = "#007BFF", text_color = "#007BFF", use_purchase_date = TRUE) {
  ns <- NS(id)
  
  div(
    class = "card",
    style = sprintf("padding: 10px; border: 1px solid %s; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);", border_color),
    
    # 标题和清空按钮在同一行，左右对齐
    div(
      style = "margin-bottom: 5px; display: flex; align-items: center; justify-content: space-between; width: 100%;", # Flex 布局
      tags$h4(
        "物品筛选", 
        style = sprintf("color: %s; font-weight: bold; margin: 0;", text_color) # 左对齐标题
      ),
      actionButton(
        ns("reset_btn"), 
        "重置", 
        icon = icon("snowplow"), 
        class = "btn-danger", 
        style = "font-size: 14px; height: 30px; padding: 5px 10px; margin-left: auto;" # 右对齐按钮
      )
    ),
    
    # 供应商和商品名筛选行
    fluidRow(
      column(6, 
             selectizeInput(ns("maker"), "供应商:", choices = NULL, width = "100%",
                            options = list(placeholder = '名称(或拼音)...', 
                                           maxOptions = 500,
                                           create = FALSE)),
             class = "custom-selectize"
      ),
      column(6, 
             selectizeInput(
               ns("name"),                
               label = "商品名:",         
               choices = NULL,            
               options = list(
                 placeholder = "商品名...",
                 create = FALSE
               ),
               width = "100%"
             ),
             class = "custom-selectize"
      )
    ),
    
    # 根据 use_purchase_date 参数动态显示采购日期筛选部分
    if (use_purchase_date) {
      fluidRow(
        column(12, 
               dateRangeInput(ns("purchase_date_range"), "采购日期范围", 
                              start = Sys.Date() - 365, end = Sys.Date(), width = "100%")
        )
      )
    } else {
      NULL  # 如果不使用采购日期筛选，隐藏该部分
    }
  )
}
