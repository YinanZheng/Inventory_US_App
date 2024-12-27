itemFilterUI <- function(id, border_color = "#007BFF", text_color = "#007BFF") {
  ns <- NS(id)
  div(
    class = "card",
    style = sprintf("margin-bottom: 5px; padding: 5px; border: 1px solid %s; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);", border_color),
    
    tags$h4("物品筛选", style = sprintf("color: %s; font-weight: bold; margin-bottom: 15px;", text_color)),
    
    fluidRow(
      column(6, 
             selectizeInput(ns("maker"), "供应商:", choices = NULL, width = "100%",
                            options = list(placeholder = '供应商名称(或拼音)...', maxOptions = 500)),
             class = "custom-selectize"
      ),
      column(6, 
             selectizeInput(
               ns("name"),                
               label = "商品名:",         
               choices = NULL,            
               options = list(
                 placeholder = "商品名...",
                 create = TRUE            # 允许自定义输入值
               ),
               width = "100%"
             ),
             class = "custom-selectize"
      )
    ),
    
    fluidRow(
      column(9, 
             textInput(ns("sku"), "输入或扫描条形码", placeholder = "请输入条形码", width = "100%")),
      column(3, 
             actionButton(ns("reset_btn"), "清空", icon = icon("snowplow"), class = "btn-danger", 
                          style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;")
      )
    )
  )
}
