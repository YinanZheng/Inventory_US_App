uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data, options = list(
  scrollY = "730px",  # 根据内容动态调整滚动高度
  scrollX = TRUE,  # 支持水平滚动
  fixedHeader = TRUE,  # 启用表头固定
  paging = TRUE,  # 启用分页
  pageLength = 30,      # 每页显示30条
  dom = 'frtip',         # 控制表格显示控件，去掉多余的功能
  searching = FALSE  # 支持搜索
)) {
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
