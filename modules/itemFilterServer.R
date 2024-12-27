itemFilterServer <- function(id, makers_df, unique_items_data, filtered_unique_items_data, unique_items_table_selected_row) {
  moduleServer(id, function(input, output, session) {
    # 更新 makers 控件
    observeEvent(makers_df(), {
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
    
    # 监听选中行并更新控件
    observeEvent(unique_items_table_selected_row(), {
      if (!is.null(unique_items_table_selected_row()) && length(unique_items_table_selected_row()) > 0) {
        selected_data <- filtered_unique_items_data()[unique_items_table_selected_row(), ]
        updateSelectizeInput(session, "maker", selected = selected_data$Maker)
        shinyjs::delay(100, {
          updateTextInput(session, "name", value = selected_data$ItemName)
        })
        updateTextInput(session, "sku", value = selected_data$SKU)
      }
    })
    
    # 清空输入
    observeEvent(input$reset_btn, {
      tryCatch({
        updateSelectizeInput(session, "maker", 
                             choices = c("", setNames(makers_df()$Maker, paste0(makers_df()$Maker, "(", makers_df()$Pinyin, ")"))), 
                             selected = "", server = TRUE)        
        updateSelectizeInput(session, "name", choices = c(""), selected = "")
        updateTextInput(session, "sku", value = "")
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}