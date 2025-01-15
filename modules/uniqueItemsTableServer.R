uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data, 
                                   options = table_default_options) {
  output$unique_items_table <- renderDT({
    
    # 获取并格式化数据
    formatted_data <- data() %>% 
      mutate(
        IntlShippingCost = sprintf("%.2f", IntlShippingCost), # 格式化为两位小数
        DomesticShippingCost = sprintf("%.2f", DomesticShippingCost) # 格式化为两位小数
      )
    
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = formatted_data,                
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath",
      options = options
    )
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  }, server = TRUE)
  
  # 监听用户点击图片列
  observeEvent(input$unique_items_table_cell_clicked, {
    info <- input$unique_items_table_cell_clicked
    
    # 检查是否点击了图片列（第三列）
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == 2) {  # 第三列在 R 中的索引是 2
        
        img_path <- as.character(data()[info$row, "ItemImagePath"])
        
        img_host_path <- paste0(host_url, "/images/", basename(img_path))
        
        # 弹出窗口显示大图
        showModal(modalDialog(
          title = "物品图片预览",
          tags$div(
            style = "overflow: auto; max-height: 700px; text-align: center;",
            tags$img(
              src = img_host_path,
              style = "max-width: 100%; height: auto; display: inline-block;"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}
