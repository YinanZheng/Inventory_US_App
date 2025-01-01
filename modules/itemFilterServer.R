itemFilterServer <- function(id, makers_df, unique_items_data, filtered_unique_items_data, unique_items_table_selected_row) {
  moduleServer(id, function(input, output, session) {
    # 更新 makers 控件
    observeEvent(makers_df(), {
      req(makers_df())  # 确保 makers_df 已加载
      updateSelectizeInput(session, "maker", 
                           choices = c("", setNames(makers_df()$Maker, paste0(makers_df()$Maker, "(", makers_df()$Pinyin, ")"))), 
                           selected = "", server = TRUE)
    })
    
    # 动态更新商品名称
    observe({
      req(unique_items_data())  # 确保数据已加载
      selected_makers <- input$maker
      filtered_data <- if (!is.null(selected_makers) && selected_makers != "") {
        unique_items_data() %>% filter(Maker %in% as.character(selected_makers))
      } else {
        unique_items_data()
      }
      item_names <- c("", unique(filtered_data$ItemName))
      updateSelectizeInput(session, "name", choices = item_names, selected = "")
    })
    
    # 清空输入
    observeEvent(input$reset_btn, {
      tryCatch({
        updateSelectizeInput(session, "maker", 
                             choices = c("", setNames(makers_df()$Maker, paste0(makers_df()$Maker, "(", makers_df()$Pinyin, ")"))), 
                             selected = "", server = TRUE)        
        updateSelectizeInput(session, "name", choices = c(""), selected = "")
        updateDateRangeInput(session, "purchase_date_range", start = Sys.Date() - 365, end = Sys.Date())
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}