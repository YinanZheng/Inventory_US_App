uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data, 
                                   options = table_default_options) {
  output$unique_items_table <- renderDT({
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath",
      options = options
    )
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  }, server = TRUE)
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}
