itemFilterUI <- function(
    id, 
    border_color = "#007BFF", 
    text_color = "#007BFF", 
    use_status = TRUE,
    status_choices = c("所有状态" = "", "采购", "国内入库", "国内出库", "国内售出", "美国入库", "美国发货", "美国调货", "退货"), 
    use_purchase_date = TRUE, 
    use_sold_date = FALSE, 
    use_exit_date = FALSE
) {
  ns <- NS(id)
  
  div(
    class = "card",
    style = sprintf("padding: 10px; border: 1px solid %s; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);", border_color),
    
    # 标题和清空按钮在同一行，左右对齐
    div(
      style = "margin-bottom: 5px; display: flex; align-items: center; justify-content: space-between; width: 100%;", 
      tags$h4(
        "物品筛选", 
        style = sprintf("color: %s; font-weight: bold; margin: 0;", text_color) 
      ),
      actionButton(
        ns("reset_btn"), 
        "重置筛选", 
        icon = icon("rotate-right"), 
        class = "btn-danger", 
        style = "font-size: 14px; height: 30px; padding: 5px 10px; margin-left: auto;" 
      )
    ),
    
    # 根据 use_status 条件渲染供应商和库存状态
    tagList(
      if (use_status) {
        fluidRow(
          column(7, 
                 selectizeInput(ns("maker"), "供应商:", choices = NULL, width = "100%",
                                options = list(placeholder = '名称(或拼音)...', 
                                               maxOptions = 500,
                                               create = FALSE))
          ),
          column(5, 
                 selectInput(
                   inputId = ns("status"),
                   label = "库存状态",
                   choices = status_choices,
                   selected = "",
                   width = "100%"
                 )
          )
        )
      } else {
        fluidRow(
          column(12, 
                 selectizeInput(ns("maker"), "供应商:", choices = NULL, width = "100%",
                                options = list(placeholder = '名称(或拼音)...', 
                                               maxOptions = 500,
                                               create = FALSE))
          )
        )
      }
    ),
    
    # 商品名筛选
    fluidRow(
      column(12, 
             selectizeInput(
               ns("name"),                
               label = "商品名:",         
               choices = NULL,            
               options = list(
                 placeholder = "商品名...",
                 create = FALSE
               ),
               width = "100%"
             )
      )
    ),
    
    # 日期范围筛选部分
    tagList(
      if (use_purchase_date) {
        fluidRow(
          column(12, 
                 dateRangeInput(ns("purchase_date_range"), "采购日期范围", 
                                start = Sys.Date() - 365, end = Sys.Date(), width = "100%")
          )
        )
      },
      
      if (use_exit_date) {
        div(
          dateRangeInput(ns("exit_date_range"), "出库日期范围", 
                         start = Sys.Date() - 365, end = Sys.Date(), width = "100%"),
          checkboxInput(ns("only_show_exit"), "仅显示出库物品", value = FALSE, width = "100%")
        )
      },
      
      if (use_sold_date) {
        div(
          dateRangeInput(ns("sold_date_range"), "售出日期范围", 
                         start = Sys.Date() - 365, end = Sys.Date(), width = "100%"),
          checkboxInput(ns("only_show_sold"), "仅显示售出物品", value = FALSE, width = "100%")
        )
      }
    )
  )
}
