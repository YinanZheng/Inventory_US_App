uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data) {
  output$unique_items_table <- renderDT({
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath",
      options = list(
        scrollY = "700px",  # 根据内容动态调整滚动高度
        scrollX = TRUE,  # 支持水平滚动
        fixedHeader = TRUE,  # 启用表头固定
        paging = TRUE,  # 支持分页
        searching = TRUE  # 支持搜索
      )
    )
    
    # 获取数据列名
    column_names <- datatable_and_names$column_names
    
    table <- datatable_and_names$datatable
    
    # 动态应用样式
    if ("库存状态" %in% column_names) {
      table <- table %>%
        formatStyle(
          "库存状态",
          backgroundColor = styleEqual(
            c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),
            c("lightgray", "#c7e89b", "darkgray", "#46a80d", "#173b02", "darkgray", "red")
          ),
          color = styleEqual(
            c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),
            c("black", "black", "black", "white", "white", "black", "white")
          )
        )
    }
    
    if ("物品状态" %in% column_names) {
      table <- table %>%
        formatStyle(
          "物品状态",
          backgroundColor = styleEqual(
            c("未知", "无瑕", "瑕疵", "修复"),
            c("darkgray", "green", "red", "orange")
          ),
          color = styleEqual(
            c("未知", "无瑕", "瑕疵", "修复"),
            c("black", "white", "white", "white")
          )
        )
    }
    
    if ("国际运输" %in% column_names) {
      table <- table %>%
        formatStyle(
          "国际运输",
          backgroundColor = styleEqual(
            c("空运", "海运"),
            c("lightblue", "darkblue")
          ),
          color = styleEqual(
            c("空运", "海运"),
            c("black", "white")
          )
        )
    }
    
    table
  })
  
  # # 监听用户点击图片列
  # observeEvent(input$unique_items_table_cell_clicked, {
  #   info <- input$unique_items_table_cell_clicked
  #   
  #   # 检查是否点击了图片列
  #   if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
  #     column_index <- which(names(data()) == "ItemImagePath")  # 图片列的索引
  #     if (info$col == column_index) {
  #       # 获取点击的图片路径
  #       img_path <- data()[info$row, "ItemImagePath"]
  #       req(img_path)  # 确保图片路径存在
  #       
  #       img_host_path <- paste0(host_url, "/images/", basename(img_path))
  #       
  #       # 弹出窗口显示大图
  #       showModal(modalDialog(
  #         title = "物品图片预览",
  #         img(src = img_host_path, height = "400px", style = "display: block; margin: 0 auto;"),
  #         size = "l",
  #         easyClose = TRUE,
  #         footer = modalButton("关闭")
  #       ))
  #     }
  #   }
  # })
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}
