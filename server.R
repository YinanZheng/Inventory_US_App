# Define server logic
server <- function(input, output, session) {
  
  source("utils.R", local = TRUE)
  
  # Database
  con <- db_connection()
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 存储 完整 maker_list 数据
  maker_list <- reactiveVal()
  
  # 存储目前数据库中存在的makers与item_names
  makers_items_map <- reactiveVal(NULL)
  
  # 触发unique_items_data刷新
  unique_items_data_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发inventory刷新
  inventory_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发order刷新
  orders_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储条形码 PDF 文件路径
  barcode_pdf_file_path <- reactiveVal(NULL)
  
  # 用于存储运单 PDF 文件路径
  label_pdf_file_path <- reactiveVal(NULL)
  
  # 初始化货架和箱子内物品（售出分页）
  shelf_items <- reactiveVal(create_empty_shelf_box())
  box_items <- reactiveVal(create_empty_shelf_box())
  
  ####################################################################################################################################
  
  # 应用启动时加载数据: item_type_data
  observe({
    tryCatch({
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
    }, error = function(e) {
      item_type_data(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load item type data.", type = "error")
    })
  })
  
  # 应用启动时加载数据: maker list
  observe({
    tryCatch({
      maker_list(dbGetQuery(con, "SELECT Name AS Maker, Pinyin FROM maker_list ORDER BY Pinyin ASC"))
    }, error = function(e) {
      maker_list(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load maker list data.", type = "error")
    })
  })
  
  # 更新orders表中已有运单pdf的情况
  update_label_status_column(con)
  
  ####################################################################################################################################
  
  # 库存表
  inventory <- reactive({
    # 当 refresh_trigger 改变时触发更新
    inventory_refresh_trigger()
    
    tryCatch({
      # 从 unique_items 表中计算聚合数据并一次性更新
      dbExecute(
        con,
        "
        UPDATE inventory i
        JOIN (
          SELECT 
            SKU,
            AVG(ProductCost) AS AvgProductCost,
            AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost
          FROM unique_items
          GROUP BY SKU
        ) u ON i.SKU = u.SKU
        SET 
          i.ProductCost = ROUND(u.AvgProductCost, 2),
          i.ShippingCost = ROUND(u.AvgShippingCost, 2)
        "
      )
      
      # 从 inventory 表中加载最新数据
      updated_inventory <- dbGetQuery(con, "SELECT * FROM inventory")
      return(updated_inventory)
      
    }, error = function(e) {
      showNotification(paste("更新库存表时发生错误：", e$message), type = "error")
      return(create_empty_inventory())  # 返回空的 inventory 数据表
    })
  })
  
  # 商品名自动联想
  item_names <- reactive({
    req(inventory())
    unique(inventory()$ItemName)  # 提取唯一的商品名
  })
  
  # 物品追踪表
  unique_items_data <- reactive({
    # 当 refresh_trigger 改变时触发更新
    unique_items_data_refresh_trigger()
    
    dbGetQuery(con, "
    SELECT 
      unique_items.UniqueID, 
      unique_items.SKU, 
      unique_items.OrderID,
      unique_items.ProductCost,
      unique_items.DomesticShippingCost,
      unique_items.Status,
      unique_items.Defect,
      unique_items.DefectNotes,
      unique_items.IntlShippingMethod,
      unique_items.IntlTracking,
      unique_items.IntlShippingCost,
      unique_items.PurchaseTime,
      unique_items.DomesticEntryTime,
      unique_items.DomesticExitTime,
      unique_items.DomesticSoldTime,
      unique_items.UsEntryTime,
      unique_items.UsShippingTime,
      unique_items.UsRelocationTime,
      unique_items.UsSoldTime,
      unique_items.ReturnTime,
      unique_items.updated_at,
      inventory.Maker,
      inventory.MajorType,
      inventory.MinorType,
      inventory.ItemName,
      inventory.ItemImagePath
    FROM 
      unique_items
    JOIN 
      inventory 
    ON 
      unique_items.SKU = inventory.SKU
    ORDER BY 
      unique_items.updated_at DESC
  ")
  })
  
  # 加载当前已有的 makers 和 item names 的对应关系
  observe({
    unique_data <- unique_items_data()  # 数据源
    makers_items <- unique_data %>%
      select(Maker, ItemName) %>%  # 选择需要的列
      distinct()                   # 确保唯一性
    
    makers_items_map(makers_items)  # 更新 reactiveVal
  })
  
  # 订单表
  orders <- reactive({
    # 当 refresh_trigger 改变时触发更新
    orders_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM orders")
  })
  
  ####################################################################################################################################
  
  # 入库页过滤
  filtered_unique_items_data_inbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内出库", "美国入库"))
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "inbound_filter-maker",
      item_name_input_id = "inbound_filter-name"    
      )
    
    # 统计 SKU, Status, Defect, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, Defect) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, Defect, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 订单管理页订单过滤
  debounced_item_name <- debounce(
    reactive({ trimws(input[["sold-item_name"]]) }),  # 确保输入值是去除空格的
    millis = 300  # 设置防抖时间为 300 毫秒（可根据需要调整）
  )
  
  filtered_orders <- reactive({
    req(orders())  # 确保订单数据存在
    
    data <- orders()  # 获取所有订单数据
    
    # 根据订单号筛选
    if (!is.null(input$filter_order_id) && input$filter_order_id != "") {
      data <- data %>% filter(grepl(trimws(input$filter_order_id), OrderID, ignore.case = TRUE))
    }
    
    # 根据运单号筛选，处理前缀多余情况
    if (!is.null(input$filter_tracking_id) && input$filter_tracking_id != "") {
      data <- match_tracking_number(data, "UsTrackingNumber", input$filter_tracking_id)
    }
    
    # 根据顾客姓名筛选
    if (!is.null(input$filter_customer_name) && input$filter_customer_name != "") {
      data <- data %>% filter(grepl(input$filter_customer_name, CustomerName, ignore.case = TRUE))
    }
    
    # 根据顾客网名筛选
    if (!is.null(input$filter_customer_netname) && input$filter_customer_netname != "") {
      data <- data %>% filter(grepl(input$filter_customer_netname, CustomerNetName, ignore.case = TRUE))
    }
    
    # 根据电商平台筛选
    if (!is.null(input$filter_platform) && input$filter_platform != "") {
      data <- data %>% filter(Platform == input$filter_platform)
    }
    
    # 根据订单状态筛选
    if (!is.null(input$filter_order_status) && input$filter_order_status != "") {
      data <- data %>% filter(OrderStatus == input$filter_order_status)
    }
    
    # 根据 SKU 或商品名筛选
    req(unique_items_data())  # 确保 unique_items_data 数据存在
    
    # 筛选包含所输入 SKU 或商品名的订单
    if (!is.null(input$filter_sku) && input$filter_sku != "") {
      sku_orders <- unique_items_data() %>%
        filter(SKU == trimws(input$filter_sku)) %>%
        pull(OrderID) %>%  # 提取与 SKU 相关的订单号
        unique()
      
      data <- data %>% filter(OrderID %in% sku_orders)
    }
    
    item_name <- debounced_item_name()
    if (!is.null(item_name) && length(item_name) > 0 && nzchar(item_name)) {
      item_orders <- unique_items_data() %>%
        filter(grepl(debounced_item_name(), ItemName, ignore.case = TRUE)) %>%
        pull(OrderID) %>%  # 提取与商品名相关的订单号
        unique()
      data <- data %>% filter(OrderID %in% item_orders)
    }
    
    # 按更新时间倒序排列
    data <- data %>% arrange(desc(updated_at))
    
    data
  })
  
  # 物品管理页过滤
  filtered_unique_items_data_manage <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "manage_filter-maker",
      item_name_input_id = "manage_filter-name",
      purchase_date_range_id = "manage_filter-purchase_date_range"
    )
    
    data
  })
  
  # 瑕疵品管理页过滤
  filtered_unique_items_data_defect <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 默认过滤条件：状态为“美国入库”且 Defect 不为“未知”
    data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "美国入库", ]
    
    # 处理开关互斥逻辑
    if (isTRUE(input$show_defects_only)) {
      # 如果仅显示瑕疵品
      data <- data[data$Defect == "瑕疵", ]
    } else if (isTRUE(input$show_perfects_only)) {
      # 如果仅显示无瑕品
      data <- data[data$Defect == "无瑕", ]
    }
    
    data
  })
  
  # 国际物流筛选
  filtered_unique_items_data_logistics <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "logistic_filter-maker",
      item_name_input_id = "logistic_filter-name",
      sold_date_range_id = "logistic_filter-sold_date_range",
      exit_date_range_id = "logistic_filter-exit_date_range"
    )
    
    shipping_method <- input$intl_shipping_method
    
    # 判断并根据物流方式筛选
    if (!is.null(shipping_method)) {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
    data
  })
  
  # 查询页过滤-库存表
  filtered_inventory <- reactive({
    req(inventory())
    result <- inventory()
    
    # Return empty inventory if no results
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    # 按供应商筛选
    if (!is.null(input[["query_filter-maker"]]) && length(input[["query_filter-maker"]]) > 0 && any(input[["query_filter-maker"]] != "")) {
      result <- result %>% filter(Maker %in% input[["query_filter-maker"]])
    }
    
    # 按商品名称筛选
    if (!is.null(input[["query_filter-name"]]) && input[["query_filter-name"]] != "") {
      result <- result %>% filter(ItemName == input[["query_filter-name"]])
    }
    
    result <- result[order(result$updated_at, decreasing = TRUE), ]
    
    return(result)
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
      purchase_date_range_id = "download_date_range"
    )
  })
  
  
  
  ####################################################################################################################################
  
  
  
  # 渲染物品追踪数据表
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- c(common_columns, list(
                                                          ItemCount = "数量")
                                                        ), selection = "multiple", data = filtered_unique_items_data_inbound)
  

  # 订单管理分页订单表
  selected_order_row <- callModule(orderTableServer, "orders_table_module",
                                   column_mapping = list(
                                     OrderID = "订单号",
                                     OrderImagePath = "订单图",
                                     CustomerName = "姓名",
                                     CustomerNetName = "网名",
                                     Platform = "平台",
                                     UsTrackingNumber = "运单号",
                                     LabelStatus = "运单PDF",
                                     OrderStatus = "状态",
                                     OrderNotes = "备注"
                                   ),
                                   data = filtered_orders,  # 数据源
                                   selection = "single" # 单选模式
  )
  
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
                                                         UsEntryTime = "入库日",
                                                         UsShippingTime = "发货日",
                                                         OrderID = "订单号")
                                                       ), selection = "multiple", data = unique_items_data,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping <- c(common_columns, list(
                                                         UsEntryTime = "入库日",
                                                         Defect = "瑕疵态",
                                                         DefectNotes = "瑕疵品备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
                                                          column_mapping = c(common_columns, list(
                                                            IntlShippingMethod = "国际运输",
                                                            DomesticExitTime = "出库日",
                                                            IntlShippingCost = "平摊国际运费",
                                                            IntlTracking = "国际物流单号"
                                                          )), selection = "multiple",
                                                          data = filtered_unique_items_data_logistics,
                                                          option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  output$filtered_inventory_table_query <- renderDT({  # input$filtered_inventory_table_query_rows_selected
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      ProductCost = "平均成本",
      ShippingCost = "平均运费"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )$datatable
  })
  
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           UsEntryTime = "入库日",
                                                           UsShippingTime = "发货日")
                                                         ), data = filtered_unique_items_data_download)
  

  
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听标签页切换事件
  observeEvent(input$inventory_us, {
    if (input$inventory_us == "入库") {
      runjs("document.getElementById('inbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "inbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听 SKU 输入
  observeEvent(input$inbound_sku, {
    req(input$inbound_sku)
    
    # 调用 handleSkuInput 并获取待入库数量
    pending_quantity <- handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound) {
      unique_ID <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "国内出库",
        update_status_value = "美国入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL, # 批量处理完了再触发刷新      
        con,                  
        input, output, session
      )
      
      # 检查是否成功处理
      if (!is.null(unique_ID) && unique_ID != "") {
        showNotification(paste0("SKU ", input$inbound_sku, " 的一个物品已自动入库！"), type = "message")
      } else {
        showNotification("自动入库失败，可能物品已全部入库或存在调货物品！", type = "error")
        return()
      }
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      # 清空 SKU 输入框
      updateTextInput(session, "inbound_sku", value = "")
    } else {
      # 未启用自动入库时更新待入库数量最大值
      if (!is.null(pending_quantity) && pending_quantity > 0) {
        updateNumericInput(session, "inbound_quantity", max = pending_quantity, value = 1)
        showNotification(paste0("已更新待入库数量最大值为 ", pending_quantity, "！"), type = "message")
      } else {
        updateNumericInput(session, "inbound_quantity", max = 0, value = 0)
      }
    }
  })
  
  observe({
    # 动态生成按钮后，通知前端激活 clipboard.js
    session$sendCustomMessage(type = 'activateClipboard', message = NULL)
  })
  
  # 确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    # 从输入中获取入库数量，确保为正整数
    inbound_quantity <- as.integer(input$inbound_quantity)
    if (is.na(inbound_quantity) || inbound_quantity <= 0) {
      showNotification("入库数量必须是一个正整数！", type = "error")
      return()
    }
    
    # 批量处理入库逻辑
    for (i in seq_len(inbound_quantity)) {
      unique_ID <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "国内出库",
        update_status_value = "美国入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL, # 批量处理完了再触发刷新      
        con,                  
        input, output, session
      )
      
      # 如果未找到对应的 UniqueID，停止后续操作
      if (is.null(unique_ID) || unique_ID == "") {
        showNotification(paste0("此SKU第 ", i, " 件物品不存在，已中止入库！"), type = "error")
        break
      }
      
      # 检查是否启用了瑕疵品选项
      defective_item <- input$defective_item
      defect_notes <- trimws(input$defective_notes)
      
      if (defective_item && defect_notes != "") {
        tryCatch({
          add_defective_note(
            con = con,
            unique_id = unique_ID,
            note_content = defect_notes,
            status_label = "瑕疵",
            refresh_trigger = NULL
          )
          showNotification("瑕疵品备注已成功添加！", type = "message")
        }, error = function(e) {
          showNotification(paste("添加备注时发生错误：", e$message), type = "error")
        })
      } else if (defective_item) {
        showNotification("无瑕疵品备注！", type = "warning")
      }
    }
    
    # 更新数据并触发 UI 刷新
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    
    # 重置输入
    updateTextInput(session, "inbound_sku", value = "")
    updateNumericInput(session, "inbound_quantity", value = 1)
    
    runjs("document.getElementById('inbound_sku').focus();")
  })
  
  # 监听选中行并更新 SKU
  observeEvent(unique_items_table_inbound_selected_row(), {
    selected_row <- unique_items_table_inbound_selected_row()
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
      updateTextInput(session, "inbound_sku", value = selected_sku)
    }
  })
  
  # 控制备注输入框显示/隐藏
  observeEvent(input$defective_item, {
    if (input$defective_item) {
      shinyjs::show("defective_notes_container")
    } else {
      shinyjs::hide("defective_notes_container")
      updateTextInput(session, "defective_notes", value = "") # 清空备注
    }
  })
  
  # PDF下载按钮默认禁用
  session$onFlushed(function() {
    shinyjs::disable("download_select_pdf")
  })
  
  # 生成选中商品条形码 PDF
  observeEvent(input$export_select_btn, {
    # 获取选中行
    selected_rows <- unique_items_table_inbound_selected_row()  # 从 DT 表选中行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选中至少一个商品！", type = "error")
      return()
    }
    
    # 获取选中物品的数据
    selected_items <- filtered_unique_items_data_inbound()[selected_rows, ]
    if (nrow(selected_items) == 0) {
      showNotification("选中数据无效，请重新选择！", type = "error")
      return()
    }
    
    skus <- selected_items$SKU
    
    # 调用现有函数生成条形码 PDF
    tryCatch({
      pdf_file <- export_barcode_pdf(
        sku = skus,
        page_width = page_width,  # 全局变量
        page_height = page_height,
        unit = size_unit
      )
      barcode_pdf_file_path(pdf_file)  # 保存生成的 PDF 路径
      
      showNotification("选中商品条形码已生成！", type = "message")
      shinyjs::enable("download_select_pdf")  # 启用下载按钮
    }, error = function(e) {
      showNotification(paste("生成条形码失败：", e$message), type = "error")
      shinyjs::disable("download_select_pdf")  # 禁用下载按钮
    })
  })
  
  # 下载选中商品条形码 PDF
  output$download_select_pdf <- downloadHandler(
    filename = function() {
      basename(barcode_pdf_file_path())  # 生成文件名
    },
    content = function(file) {
      file.copy(barcode_pdf_file_path(), file, overwrite = TRUE)
      shinyjs::disable("download_select_pdf")  # 禁用下载按钮
      barcode_pdf_file_path(NULL)  # 清空路径
    }
  )

  
  
  ################################################################
  ##                                                            ##
  ## 发货分页                                                   ##
  ##                                                            ##
  ################################################################
  
  
  ### 数据准备
  
  # 当前订单ID
  current_order_id <- reactiveVal()
  
  # 运单号输入逻辑
  matching_orders <- reactive({
    # 如果运单号为空，返回空数据框
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      return(data.frame())  # 返回空数据框
    }
    
    req(input$shipping_bill_number)  # 确保运单号不为空
    
    data <- match_tracking_number(orders(), "UsTrackingNumber", input$shipping_bill_number)
    
    data %>% arrange(OrderStatus == "装箱")
  })
  
  # 自动设置 current_order_id
  observe({
    req(matching_orders())  # 确保 matching_orders 存在
    
    if (nrow(matching_orders()) > 0) {
      # 设置第一个订单的 OrderID 为当前订单 ID
      current_order_id(matching_orders()$OrderID[1])
    } else {
      # 如果没有订单，清空 current_order_id
      current_order_id(NULL)
    }
  })
  
  order_items <- reactive({
    # 如果当前订单 ID 为空，返回空数据框
    if (is.null(current_order_id()) || trimws(current_order_id()) == "") {
      return(data.frame())  # 返回空数据框
    }
    
    req(current_order_id())  # 确保当前订单 ID 不为空
    
    # 筛选当前订单的物品
    unique_items_data() %>% filter(OrderID == current_order_id())
  })
  
  ### 渲染
  
  # 渲染订单信息卡片
  observe({
    req(matching_orders())
    
    if (nrow(matching_orders()) == 0) {
      renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      current_order_id(NULL)  # 清空当前订单 ID
      return()
    }
    
    renderOrderInfo(output, "order_info_card", matching_orders())
    
    all_packed <- all(matching_orders()$OrderStatus == "装箱")
    if (all_packed) {
      showModal(modalDialog(
        title = "运单完成提示",
        "当前运单号所对应的所有订单已完成装箱操作！",
        easyClose = TRUE,
        footer = NULL  # 不需要关闭按钮
      ))
      
      # 延迟 2 秒后自动关闭弹窗
      shinyjs::delay(2000, removeModal())
    }
  })
  
  observe({
    # 如果 current_order_id 为空，清空标题
    if (is.null(current_order_id()) || trimws(current_order_id()) == "") {
      output$order_items_title <- renderUI({ NULL })  # 清空标题
      return()  # 停止后续逻辑
    }
    
    # 渲染标题
    output$order_items_title <- renderUI({
      tags$h4(
        HTML(paste0(as.character(icon("box")), " 订单号 ", current_order_id(), " 的物品")),
        style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
      )
    })
  })
  
  # 渲染物品信息卡片  
  observe({
    req(order_items(), matching_orders())
    
    if (nrow(order_items()) == 0) {
      renderOrderItems(output, "shipping_order_items_cards", data.frame())  # 清空物品卡片
      return()
    }
    
    renderOrderItems(output, "shipping_order_items_cards", order_items())
    runjs("document.getElementById('sku_input').focus();")
  })
  
  
  ### 逻辑
  
  # 清空运单号逻辑
  observeEvent(input$shipping_bill_number, {
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      current_order_id(NULL)  # 清空当前订单 ID
      output$order_items_title <- renderUI({ NULL })  # 清空标题
      renderOrderItems(output, "shipping_order_items_cards", data.frame())  # 清空物品卡片
    }
  })
  
  # 点击订单卡片逻辑
  observeEvent(input$selected_order_id, {
    req(input$selected_order_id)  # 确保订单 ID 存在
    
    # 获取选中的订单 ID
    current_order_id(input$selected_order_id)
    
    # 更新高亮样式
    runjs(sprintf("
      $('.order-card').css('border-color', '#ddd');  // 清除其他卡片高亮
      $('.order-card').css('box-shadow', '0px 4px 8px rgba(0, 0, 0, 0.1)');  // 恢复默认阴影
      $('#order_card_%s').css('border-color', '#007BFF');  // 高亮选中卡片
      $('#order_card_%s').css('box-shadow', '0px 4px 8px rgba(0, 123, 255, 0.5)');  // 添加高亮阴影
    ", current_order_id(), current_order_id()))
    
    # 聚焦 SKU 输入框
    runjs("document.getElementById('sku_input').focus();")
  })
  
  # 判断选中订单状态，提示用户操作
  observe({
    req(matching_orders(), current_order_id())
    
    # 获取当前选中订单信息
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    
    # 确保选中订单存在
    req(nrow(current_order) > 0)
  
    # 存储运单文件路径
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(current_order$UsTrackingNumber, ".pdf")))
    
    # 获取当前订单内的物品
    current_items <- order_items()
    
    if (nrow(current_items) == 0) {
      # 如果订单内没有物品
      showModal(modalDialog(
        title = "订单内无物品",
        div(
          "当前订单内未检测到任何物品，请核对订单信息无误后手动发货",
          style = "font-size: 16px; margin-bottom: 10px;"
        ),
        footer = NULL,  # 移除确认按钮，避免多余交互
        easyClose = TRUE  # 允许点击背景关闭，但主要通过自动关闭处理
      ))
      
      # 延迟 2 秒后自动关闭弹窗
      shinyjs::delay(2000, removeModal())
      
      return()  # 停止后续逻辑
    }
    
    # 检查订单状态
    if (current_order$OrderStatus[1] != "装箱") {
      if (current_order$OrderStatus[1] != "备货") {
        showNotification(
          paste0("当前订单状态为 '", current_order$OrderStatus[1], "' ，操作可能受限！请核对后继续。"),
          type = "warning"
        )
      } else {
        # 提示扫描SKU条码
        runjs("document.getElementById('sku_input').focus();")
        showNotification(
          paste0("请为订单 ", current_order_id(), " 扫描或输入SKU条码！"),
          type = "message"
        )
      }
    }
  })
  
  # 动态生成下载运单按钮
  output$dynamic_download_button <- renderUI({
    req(matching_orders(), current_order_id())
    
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    
    if (is.null(current_order)) {
      return(NULL)
    }
    
    # 根据 LabelStatus 动态生成按钮或文本
    label_text <- switch(
      current_order$LabelStatus,
      "无" = "无运单文件",
      "已传" = "下载运单文件",
      "印出" = "运单已打印",
      "无运单文件" # 默认值
    )
    
    if (current_order$LabelStatus == "无") {
      div(
        label_text,
        class = "btn btn-secondary",
        style = "background-color: grey; color: white; cursor: not-allowed; padding: 6px 12px; border-radius: 4px; display: inline-block; text-align: center;"
      )
    } else {
      downloadButton("download_pdf", label = label_text, class = "btn btn-primary")
    }
  })
  
  # 定义下载处理器
  output$download_pdf <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
      tracking_number <- tools::file_path_sans_ext(basename(label_pdf_file_path()))
      # 更新数据库中的 LabelStatus 为 "印出"
      dbExecute(
        con,
        "UPDATE orders SET LabelStatus = '印出' WHERE UsTrackingNumber = ?",
        params = list(tracking_number)
      )
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  )
  
  
  # SKU 输入逻辑
  observeEvent(input$sku_input, {
    req(input$shipping_bill_number, input$sku_input)
    
    sku <- trimws(input$sku_input)
    
    # 获取当前订单
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    
    # 检查订单状态是否为“备货”
    if (nrow(current_order) == 0 || current_order$OrderStatus != "备货") {
      showNotification("当前订单状态不是“备货”，不可发货！", type = "error")
      # 清空输入框
      updateTextInput(session, "sku_input", value = "")
      return()
    }
    
    # 查找SKU对应的物品
    matching_item <- order_items() %>% filter(SKU == sku)
    
    # 如果未找到对应的 SKU
    if (nrow(matching_item) == 0) {
      showNotification("未找到商品，请检查输入的商品是否存在于本订单！", type = "error")
      updateTextInput(session, "sku_input", value = "")
      return()
    }
    
    # 查找第一个状态不为“美国发货”的物品
    next_item <- matching_item %>% filter(Status != "美国发货") %>% slice(1)
    
    # 如果所有物品状态均为“美国发货”
    if (nrow(next_item) == 0) {
      showNotification("该商品已完成操作（状态为 '美国发货'）！", type = "message")
      updateTextInput(session, "sku_input", value = "")
      return()
    }
    
    # 更新状态为“美国发货”
    tryCatch({
      update_status(
        con = con,
        unique_id = next_item$UniqueID,
        new_status = "美国发货",
        refresh_trigger = unique_items_data_refresh_trigger
      )
      
      # 检查是否所有物品状态均为“美国发货”
      if (all(order_items()$Status == "美国发货")) {
        # 获取当前订单备注
        order_notes <- matching_orders() %>% 
          filter(OrderID == current_order_id()) %>% 
          pull(OrderNotes)
        
        # 判断备注中是否包含“调货”字样
        if (grepl("调货", order_notes, fixed = TRUE)) {
          # 弹窗提示用户订单涉及调货
          showModal(modalDialog(
            title = "订单涉及调货",
            easyClose = FALSE,
            div(
              style = "padding: 10px; font-size: 16px; color: #FF0000;",
              paste0("订单 ", current_order_id(), " 涉及调货物品，请核对物品备齐后手动发货。")
            ),
            footer = tagList(
              modalButton("关闭")
            )
          ))
        } else {
          # 弹窗提示订单已完成装箱
          showModal(modalDialog(
            title = "确认装箱",
            easyClose = FALSE,
            div(
              style = "padding: 10px; font-size: 16px;",
              paste0("订单 ", current_order_id(), " 的所有物品已完成入箱扫描")
            ),
            footer = tagList(
              actionButton("confirm_shipping_btn", "确认装箱", icon = icon("check"), class = "btn-primary")
            )
          ))
        }
      }
      
      # 清空输入框
      updateTextInput(session, "sku_input", value = "")
      
    }, error = function(e) {
      showNotification(paste("更新状态时发生错误：", e$message), type = "error")
    })
  })
  
  # 确认装箱逻辑
  observeEvent(input$confirm_shipping_btn, {
    
    if (!all(order_items()$Status == "美国发货")) {
      showNotification("还有未完成操作的物品，请核对！", type = "warning")
      return()
    }
    
    # 更新当前订单状态
    tryCatch({
      dbExecute(con, "UPDATE orders SET OrderStatus = '装箱' WHERE OrderID = ?", params = list(current_order_id()))
      
      orders_refresh_trigger(!orders_refresh_trigger())
      
      showNotification(paste0("订单 ", current_order_id(), " 已成功装箱！"), type = "message")
      
      removeModal()
    }, error = function(e) {
      showNotification(paste("更新订单状态时发生错误：", e$message), type = "error")
    })
    runjs("document.getElementById('shipping_bill_number').focus();")
  })
  
  # 清空运单号和 SKU 输入框
  observeEvent(input$clear_shipping_bill_btn, {
    updateTextInput(session, "shipping_bill_number", value = "")
    showNotification("运单号和 SKU 输入框已清空！", type = "message")
  })
  
  # 动态显示“手动发货”按钮
  output$dynamic_ship_button <- renderUI({
    # 确保匹配订单和当前订单 ID 存在
    req(matching_orders(), current_order_id())
    
    # 从 matching_orders() 获取当前订单信息
    order <- matching_orders() %>% filter(OrderID == current_order_id())
    
    # 确保找到当前订单
    req(nrow(order) > 0)
    
    # 提取备注字段
    order_notes <- order$OrderNotes
    
    # 检查订单备注是否包含“调货”
    has_transfer_note <- grepl("调货", order_notes, fixed = TRUE)
    
    # 获取当前订单内的物品
    items <- order_items()
    
    # 判断按钮显示条件
    if (nrow(items) == 0 || (all(items$Status == "美国发货") && has_transfer_note)) {
      # 如果订单内没有物品，或者订单内物品状态全为“美国发货”且备注包含“调货”
      return(actionButton("ship_order_btn", "手动发货", icon = icon("paper-plane"), class = "btn-success", style = "margin-top: 10px;", width = "100%"))
    }
    
    # 如果条件不满足，隐藏按钮
    return(NULL)
  })
  
  # 手动发货按钮功能
  observeEvent(input$ship_order_btn, {
    req(current_order_id())  # 确保当前订单ID存在
    
    tryCatch({
      # 更新订单状态为“装箱”
      dbExecute(con, "UPDATE orders SET OrderStatus = '装箱' WHERE OrderID = ?", params = list(current_order_id()))
      
      orders_refresh_trigger(!orders_refresh_trigger())
      
      showNotification(paste0("订单 ", current_order_id(), " 已手动登记为装箱状态！"), type = "message")
      
      # 清空订单ID
      current_order_id(NULL)
    }, error = function(e) {
      showNotification(paste("发货操作失败：", e$message), type = "error")
    })
  })
  
  ##############################################################################################
  
  # 创建新加订单物品容器
  new_order_items <- reactiveVal()
  
  # 运单号输入、清空后的反应逻辑
  debounced_us_shipping_bill_number <- debounce(reactive(gsub("[^0-9]", "", trimws(input$us_shipping_bill_number))), 500)
  
  # 计算 SKU 的有效库存数量
  stock_data <- reactive({
    req(unique_items_data())  # 确保数据存在
    unique_items_data() %>%
      filter(Status == "美国入库", is.na(Defect) | Defect != "瑕疵") %>%  # 确保过滤条件有效
      group_by(SKU) %>%
      summarise(StockQuantity = n(), .groups = "drop")
  })
  
  # 动态生成订单
  new_order <- reactive({
    req(input$us_shipping_bill_number, input$us_shipping_platform)
    
    # 如果平台未选择或运单号为空，返回 NULL
    if (input$us_shipping_platform == "" || input$us_shipping_bill_number == "") {
      return(NULL)
    }
    
    # 确保 new_order_items 存在
    req(new_order_items())
    
    # 检查物品列表是否为空
    if (nrow(new_order_items()) == 0) {
      return(NULL)  # 如果没有物品，返回 NULL
    }
    
    # 去除空格并提取数字部分
    cleaned_us_bill_number <- debounced_us_shipping_bill_number()
    
    # 生成订单 ID
    generated_order_id <- generate_order_id(
      cleaned_us_bill_number,
      new_order_items()$UniqueID
    )
    
    # 创建动态订单数据
    data.frame(
      OrderID = generated_order_id,
      UsTrackingNumber = cleaned_us_bill_number,
      CustomerName = "",
      CustomerNickname = "",
      Platform = input$us_shipping_platform,
      OrderImagePath = "",
      OrderNotes = trimws(input$us_shipping_order_notes),
      OrderStatus = "备货",
      stringsAsFactors = FALSE
    )
  })
  
  # 动态渲染订单卡片
  observe({
    req(new_order())
    
    renderOrderInfo(output, "order_info_card", new_order(), clickable = FALSE)
    
    # 更新标题
    output$order_items_title <- renderUI({
      tags$h4(
        HTML(paste0(as.character(icon("box")), " 订单号 ", new_order()$OrderID, " 的物品")),
        style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
      )
    })
  })
  
  # 动态渲染订单物品卡片
  observe({
    req(new_order_items())
    renderOrderItems(output, "shipping_order_items_cards", new_order_items(), deletable = TRUE)
  })
  
  observeEvent(input$us_shipping_sku_input, {
    req(input$us_shipping_sku_input)
    
    # 获取输入 SKU
    new_sku <- trimws(input$us_shipping_sku_input)
    
    # 校验 SKU 是否有效
    valid_sku <- stock_data() %>% filter(SKU == new_sku)
    if (nrow(valid_sku) == 0) {
      showNotification("输入的 SKU 不存在或状态不为 '美国入库'！", type = "error")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 获取当前物品
    current_items <- new_order_items()
    if (!is.null(current_items)) {
      existing_count <- sum(current_items$SKU == new_sku)
      if (existing_count >= valid_sku$StockQuantity[1]) {
        showNotification(paste0("输入的 SKU '", new_sku, "' 已达到库存上限！"), type = "error")
        updateTextInput(session, "us_shipping_sku_input", value = "")
        return()
      }
    }
    
    # 筛选未被选择的物品
    available_items <- unique_items_data() %>%
      filter(SKU == new_sku & Status == "美国入库" & !(UniqueID %in% current_items$UniqueID))
    
    if (nrow(available_items) == 0) {
      showNotification("该 SKU 的库存已用尽！", type = "error")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 添加未被选择的第一件物品
    item_info <- available_items %>% slice(1)
    current_items <- rbind(current_items, item_info)
    new_order_items(current_items)
    
    updateTextInput(session, "us_shipping_sku_input", value = "")
  })
  
  
  observe({
    # 获取延迟后的输入值
    bill_number <- debounced_us_shipping_bill_number()
    
    if (bill_number == "") {
      renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      output$order_items_title <- renderUI({ NULL })  # 清空标题
      renderOrderItems(output, "shipping_order_items_cards", data.frame())  # 清空物品卡片
    } else {
      # 延迟后执行的逻辑
      runjs("document.getElementById('us_shipping_sku_input').focus();")
    }
  })
  
  observeEvent(input$us_ship_order_btn, {
    req(new_order(), new_order_items())
    
    order <- new_order()
    items <- new_order_items()
    
    if (nrow(items) == 0) {
      showNotification("没有物品需要发货！", type = "error")
      return()
    }
    
    tryCatch({
      dbBegin(con)
      
      # 生成拼图路径
      combined_image_paths <- items$ItemImagePath[!is.na(items$ItemImagePath) & items$ItemImagePath != ""]
      if (length(combined_image_paths) == 0) {
        showNotification("无法生成订单图片：没有有效的物品图片路径", type = "warning")
        order_image_path <- ""
      } else {
        montage_path <- paste0("/var/www/images/", order$OrderID, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        order_image_path <- generate_montage(combined_image_paths, montage_path)
      }
      
      # 插入订单到 `orders` 表
      dbExecute(con,
                "INSERT INTO orders (OrderID, UsTrackingNumber, CustomerName, CustomerNetName, Platform, OrderImagePath, OrderNotes, OrderStatus, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                params = list(
                  order$OrderID,
                  order$UsTrackingNumber,
                  order$CustomerName,
                  order$CustomerNickname,
                  order$Platform,
                  order_image_path,
                  order$OrderNotes,
                  "装箱"
                )
      )
      
      # 更新物品状态和订单号
      unique_ids <- paste0("'", paste(items$UniqueID, collapse = "','"), "'")
      dbExecute(con,
                sprintf("UPDATE unique_items
               SET Status = '美国发货', OrderID = '%s'
               WHERE UniqueID IN (%s)",
                        order$OrderID, unique_ids)
      )
      
      # 调整库存
      for (sku in unique(items$SKU)) {
        adjust_inventory_quantity(
          con = con,
          sku = sku,
          adjustment = -nrow(items %>% filter(SKU == sku))
        )
      }
      
      dbCommit(con)
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      # 结果展示
      
      added_order <- orders() %>% filter(OrderID == order$OrderID)
      renderOrderInfo(output, "order_info_card", added_order, clickable = FALSE)
      
      output$order_items_title <- renderUI({
        tags$h4(
          HTML(paste0(as.character(icon("box")), " 订单号 ", order$OrderID, " 的物品")),
          style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
        )
      })
      
      added_order_items <- unique_items_data() %>% filter(OrderID == order$OrderID)
      renderOrderItems(output, "shipping_order_items_cards", added_order_items, deletable = FALSE)
      
      showNotification(
        paste0("订单已成功发货！订单号：", order$OrderID, "，共发货 ", nrow(items), " 件。"),
        type = "message"
      )
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("发货失败：", e$message), type = "error")
    })
  })
  
  
  
  
  # 订单物品删除逻辑 （美国售出only）
  observeEvent(input$delete_card, {
    req(input$delete_card, new_order_items())  # 确保输入和物品列表存在
    
    # 当前物品列表
    current_items <- new_order_items()
    
    # 移除对应的物品
    updated_items <- current_items %>% filter(UniqueID != input$delete_card)
    new_order_items(updated_items)  # 更新物品列表
    
    # 提示删除成功
    showNotification("物品已删除。", type = "message")
  })
  
  # 清空逻辑
  observeEvent(input$clear_us_shipping_bill_btn, {
    updateTextInput(session, "us_shipping_bill_number", value = "")
    updateTextInput(session, "us_shipping_sku_input", value = "")
    updateSelectInput(session, "us_shipping_platform", selected = "TikTok")
  })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  
  
  ################################################################
  ##                                                            ##
  ## 订单管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  ##### 网名自动填写
  
  matching_customer <- reactive({
    req(input$customer_name)  # 确保用户输入了顾客姓名
    tryCatch({
      result <- orders() %>%
        filter(grepl(input$customer_name, CustomerName, ignore.case = TRUE))  # 模糊匹配顾客姓名
      
      valid_result <- result %>%
        filter(!is.na(CustomerNetName) & CustomerNetName != "") %>%  # 过滤有效的网名
        slice_head(n = 1)  # 仅返回第一条有网名的记录
      
      # 返回第一个有效的网名或 NULL
      if (nrow(valid_result) > 0) {
        return(valid_result$CustomerNetName[1])
      } else {
        return(NULL)  # 没有匹配的网名
      }
    }, error = function(e) {
      showNotification("网名查找出错！", type = "error")
      return(NULL)
    })
  })
  
  # 缓存最近查询过的顾客姓名与网名
  cache <- reactiveVal(list())
  
  # 使用 debounce 避免频繁触发查询
  customer_name_delayed <- debounce(reactive(input$customer_name), 300)
  
  # 网名自动填写
  observeEvent(customer_name_delayed(), {
    # 如果用户清空了 customer_name，则清空 customer_netname
    if (customer_name_delayed() == "") {
      updateTextInput(session, "customer_netname", value = "")
      return()
    }
    
    req(customer_name_delayed())  # 确保用户输入不为空
    
    cache_data <- cache()
    
    # 检查缓存是否已有数据
    if (customer_name_delayed() %in% names(cache_data)) {
      netname <- cache_data[[customer_name_delayed()]]
    } else {
      # 查询数据库
      netname <- matching_customer()
      
      # 如果有结果，更新缓存
      if (!is.null(netname)) {
        cache_data[[customer_name_delayed()]] <- netname
        cache(cache_data)  # 更新缓存
      }
    }
    
    # 更新网名输入框
    updateTextInput(session, "customer_netname", value = netname %||% "")
  })
  
  ######
  
  # 出售订单图片处理模块
  image_sold <- imageModuleServer("image_sold")
  
  # 在输入订单号时检查订单信息并填充
  observeEvent(input$order_id, {
    # 检查订单号是否为空
    req(input$order_id)  # 如果订单号为空，停止执行
    
    tryCatch({
      # 去除空格和#号
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # 查询订单信息，包含新增字段
      existing_order <- dbGetQuery(con, "
      SELECT CustomerName, CustomerNetName, Platform, UsTrackingNumber, OrderStatus, OrderNotes 
      FROM orders 
      WHERE OrderID = ?", params = list(sanitized_order_id)
      )
      
      # 如果订单存在，填充对应字段
      if (nrow(existing_order) > 0) {
        # 填充各字段信息
        updateSelectInput(session, "platform", selected = existing_order$Platform[1])
        
        updateTextInput(session, "customer_name", value = existing_order$CustomerName[1])
        # updateTextInput(session, "customer_netname", value = existing_order$CustomerNetName[1]) # 交给网名自动填写功能
        
        updateTextInput(session, "tracking_number", value = existing_order$UsTrackingNumber[1])
        updateTextAreaInput(session, "order_notes", value = existing_order$OrderNotes[1])
        
        # 动态更新按钮为“更新订单”
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "更新订单",
            icon = icon("edit"),
            class = "btn-success",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
        
        showNotification("已找到订单信息！字段已自动填充", type = "message")
      } else {
        # 如果订单记录不存在，清空出order ID以外所有相关字段
        showNotification("未找到对应订单记录，可登记新订单", type = "warning")
        
        # 重置所有输入框
        updateSelectInput(session, "platform", selected = "")
        updateTextInput(session, "customer_name", value = "")
        updateTextInput(session, "customer_netname", value = "")
        updateTextInput(session, "tracking_number", value = "")
        image_sold$reset()
        updateTextAreaInput(session, "order_notes", value = "")
        
        # 动态更新按钮为“登记订单”
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "登记订单",
            icon = icon("plus"),
            class = "btn-primary",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
      }
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("检查订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 登记订单逻辑
  observeEvent(input$register_order_btn, {
    if (is.null(input$order_id) || input$order_id == "") {
      showNotification("订单号不能为空！", type = "error")
      return()
    }
    
    if (is.null(input$platform) || input$platform == "") {
      showNotification("电商平台不能为空，请选择一个平台！", type = "error")
      return()
    }
    
    # 去除空格和#号
    sanitized_order_id <- gsub("#", "", trimws(input$order_id))
    
    # 调用封装函数登记订单
    register_order(
      order_id = sanitized_order_id,
      customer_name = input$customer_name,
      customer_netname = input$customer_netname,
      platform = input$platform,
      order_notes = input$order_notes,
      tracking_number = input$tracking_number,
      image_data = image_sold,
      con = con,
      orders = orders,
      box_items = box_items,
      unique_items_data = unique_items_data
    )
    
    # 更新订单表格
    orders_refresh_trigger(!orders_refresh_trigger())
    
    # reset_order_form(session, image_sold)
  })
  
  # 清空订单信息按钮
  observeEvent(input$clear_order_btn, {
    reset_order_form(session, image_sold)
    showNotification("已清空所有输入！", type = "message")
  })
  
  # 订单关联物品容器
  associated_items <- reactiveVal()
  
  # 商品名自动联想
  autocompleteInputServer("sold", get_suggestions = item_names)  # 返回商品名列表
  
  # 监听订单选择事件
  observeEvent(selected_order_row(), {
    selected_row <- selected_order_row()
    
    # 如果用户选择了订单，获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    customer_name <- selected_order$CustomerName
    order_status <- selected_order$OrderStatus
    
    # 填充左侧订单信息栏
    updateTextInput(session, "order_id", value = order_id)
    
    # 动态更新标题
    output$associated_items_title <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        
        # 左侧标题
        tags$h4(
          sprintf("#%s - %s 的订单物品", order_id, customer_name),
          style = "color: #007BFF; font-weight: bold; margin: 0;"
        ),
        
        # 右侧按钮（仅在订单状态为“预定”时显示）
        if (order_status == "调货") 
        {
          tagList(
            actionButton(
              inputId = "complete_transfer",
              label = "已完成调货",
              class = "btn-success",
              style = "margin-left: auto; font-size: 14px; padding: 5px 10px;"
            ),
            uiOutput("dynamic_download_button_manage")
          )
        }
      )
    })
    
    # 更新关联物品数据
    associated_items <- associated_items(unique_items_data() %>% filter(OrderID == order_id))
  })
  
  observeEvent(input$complete_transfer, {
    # 检查是否选择了订单行
    if (is.null(selected_order_row())) {
      # 提示用户未选择订单
      showNotification("请先点选订单行再执行调货操作！", type = "warning")
      return()
    }
    
    req(selected_order_row())
    
    # 获取选中订单
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    existing_notes <- selected_order$OrderNotes %||% ""  # 若为空，则默认空字符串
    
    # 在 R 中拼接备注内容
    new_notes <- paste(existing_notes, sprintf("【调货完成 %s】", format(Sys.Date(), "%Y-%m-%d")))
    
    tryCatch({
      # 使用拼接后的备注信息进行 SQL 更新
      dbExecute(con, "
      UPDATE orders
      SET OrderStatus = '备货',
          OrderNotes = ?
      WHERE OrderID = ?
    ", params = list(new_notes, order_id))
      
      # 重新加载最新的 orders 数据
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 通知用户操作成功
      showNotification(sprintf("订单 #%s 已更新为备货状态！", order_id), type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(sprintf("更新订单状态时发生错误：%s", e$message), type = "error")
    })
  })

  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame())  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), deletable = FALSE)
  })
  
  # 清空筛选条件逻辑
  observeEvent(input$reset_filter_btn, {
    tryCatch({
      # 重置所有输入框和选择框
      updateTextInput(session, "filter_order_id", value = "")
      updateTextInput(session, "filter_tracking_id", value = "")
      updateTextInput(session, "filter_customer_name", value = "")
      updateTextInput(session, "filter_customer_netname", value = "")
      updateSelectInput(session, "filter_platform", selected = "")
      updateSelectInput(session, "filter_order_status", selected = "")
      updateTextInput(session, "filter_sku", value = "")
      updateTextInput(session, "sold-item_name", value = "")
      
      # 显示成功通知
      showNotification("筛选条件已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并显示通知
      showNotification(paste("清空筛选条件时发生错误：", e$message), type = "error")
    })
  })
  
  # 删除订单逻辑
  observeEvent(input$delete_order_btn, {
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    # 显示确认弹窗
    showModal(
      modalDialog(
        title = "确认删除订单",
        paste0("您确定要删除订单 ", order_id, " 吗？此操作无法撤销！"),
        footer = tagList(
          modalButton("取消"),  # 关闭弹窗按钮
          actionButton("confirm_delete_order_btn", "确认删除", class = "btn-danger")
        )
      )
    )
  })
  
  # 确认删除订单逻辑
  observeEvent(input$confirm_delete_order_btn, {
    removeModal()  # 关闭确认弹窗
    
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    tryCatch({
      # 获取与订单关联的物品
      associated_items <- dbGetQuery(con, "SELECT * FROM unique_items WHERE OrderID = ?", params = list(order_id))
      
      if (nrow(associated_items) > 0) {
        # 遍历关联物品进行逆向操作
        lapply(1:nrow(associated_items), function(i) {
          item <- associated_items[i, ]
          
          # 逆向调整库存
          adjust_inventory_quantity(
            con = con,
            sku = item$SKU,
            adjustment = 1  # 增加库存数量
          )
          
          # 恢复物品状态到“美国入库”
          update_status(
            con = con,
            unique_id = item$UniqueID,
            new_status = "美国入库",
            clear_status_timestamp = "美国售出" # 同时清空美国售出的时间戳
          )
          
          # 清空物品的 OrderID
          update_order_id(
            con = con,
            unique_id = item$UniqueID,
            order_id = NULL  # 清空订单号
          )
        })
      }
      
      # 删除订单记录
      dbExecute(con, "DELETE FROM orders WHERE OrderID = ?", params = list(order_id))
      
      # 通知用户操作结果
      message <- if (nrow(associated_items) > 0) {
        paste("订单", order_id, "已成功删除，订单内物品已返回库存！")
      } else {
        paste("订单", order_id, "已成功删除，没有关联的物品需要处理！")
      }
      showNotification(message, type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 重置输入
      reset_order_form(session, image_sold)
      
      # 清空关联物品表
      output$associated_items_table <- renderDT({ NULL })
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 订单合并
  observeEvent(input$merge_order_btn, {
    tryCatch({
      # 获取用户选中的订单号
      selected_order <- filtered_orders()[selected_order_row(), ]
      selected_order_id <- selected_order$OrderID
      
      if (is.null(selected_order_id) || length(selected_order_id) != 1) {
        showNotification("请选择一个订单进行合并！", type = "error")
        return()
      }
      
      # 检查订单号是否包含 "@"
      if (!grepl("@", selected_order_id)) {
        showNotification("选中的订单不包含识别符 '@'，无法进行合并！", type = "error")
        return()
      }
      
      # 提取主单号
      main_order_id <- sub("@.*", "", selected_order_id)  # 提取 '@' 之前的部分
      
      # 获取可能的子单
      possible_sub_orders <- orders() %>%
        filter(grepl(paste0("^", main_order_id, "@"), OrderID))
      
      # 检查子单是否满足合并条件
      if (nrow(possible_sub_orders) == 0) {
        showNotification("未找到符合条件的子单！", type = "error")
        return()
      }
      
      order_statuses <- unique(possible_sub_orders$OrderStatus)
      tracking_numbers <- unique(possible_sub_orders$UsTrackingNumber)
      platforms <- unique(possible_sub_orders$Platform)
      
      # 检查订单状态、运单号和平台是否满足合并条件
      if (!all(order_statuses == "备货") || length(tracking_numbers) > 1 || length(platforms) > 1) {
        showNotification("子单的订单状态必须全部为 '备货'，运单号和平台必须一致，无法合并！", type = "error")
        return()
      }
      
      # 获取子单的所有物品
      sub_items <- unique_items_data() %>%
        filter(OrderID %in% possible_sub_orders$OrderID)
      
      # 子单物品图片路径拼接
      image_paths <- unique(sub_items$ItemImagePath[!is.na(sub_items$ItemImagePath)])
      
      if (length(image_paths) > 0) {
        # 生成拼接图片路径（带时间戳）
        montage_path <- paste0("/var/www/images/", main_order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        # 调用拼接图片函数
        merged_image_path <- generate_montage(image_paths, montage_path)
      } else {
        merged_image_path <- NA  # 如果没有图片，路径设为 NA
      }
      
      # 更新订单信息
      merged_order <- tibble(
        OrderID = main_order_id,
        Platform = platforms[1],
        UsTrackingNumber = tracking_numbers[1],
        CustomerName = ifelse(length(unique(possible_sub_orders$CustomerName)) > 0,
                              paste(unique(possible_sub_orders$CustomerName), collapse = ", "), NA),
        CustomerNetName = ifelse(length(unique(possible_sub_orders$CustomerNetName)) > 0,
                                 paste(unique(possible_sub_orders$CustomerNetName), collapse = ", "), NA),
        OrderImagePath = merged_image_path,  # 使用拼接后的图片路径
        OrderNotes = ifelse(length(unique(possible_sub_orders$OrderNotes)) > 0,
                            paste(unique(possible_sub_orders$OrderNotes), collapse = " | "), NA),
        OrderStatus = "备货"
      )
      # 更新数据库中的订单
      dbWriteTable(
        con, "orders", merged_order,
        append = TRUE, overwrite = FALSE
      )
      
      # 删除子单
      dbExecute(con, sprintf(
        "DELETE FROM orders WHERE OrderID IN (%s)",
        paste(shQuote(possible_sub_orders$OrderID), collapse = ", ")
      ))
      
      # 更新子单物品的订单号为主单号
      update_order_id(con, sub_items$UniqueID, main_order_id)
      
      showNotification(paste("订单合并成功！主单号为：", main_order_id, ", 共计", nrow(sub_items), "件物品"), type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      orders_refresh_trigger(!orders_refresh_trigger())
      
    }, error = function(e) {
      showNotification(paste("合并订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 动态生成下载运单按钮
  output$dynamic_download_button_manage <- renderUI({
    req(matching_orders(), current_order_id())
    
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    
    if (is.null(current_order)) {
      return(NULL)
    }
    
    # 根据 LabelStatus 动态生成按钮或文本
    label_text <- switch(
      current_order$LabelStatus,
      "已传" = "下载运单文件",
      "印出" = "运单已打印",
      "下载运单文件" # 默认值
    )
    
    if (current_order$LabelStatus == "无") {
      div(
        label_text,
        class = "btn btn-secondary",
        style = "background-color: grey; color: white; cursor: not-allowed; padding: 6px 12px; border-radius: 4px; display: inline-block; text-align: center;"
      )
    } else {
      downloadButton("download_pdf_manage", label = label_text, class = "btn btn-primary")
    }
  })
  
  # 定义下载处理器
  output$download_pdf_manage <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
      tracking_number <- tools::file_path_sans_ext(basename(label_pdf_file_path()))
      # 更新数据库中的 LabelStatus 为 "印出"
      dbExecute(
        con,
        "UPDATE orders SET LabelStatus = '印出' WHERE UsTrackingNumber = ?",
        params = list(tracking_number)
      )
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 物品管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "error")
      return()
    }
    
    # 显示确认框
    showModal(deleteConfirmationModal(length(selected_rows)))
  })
  
  # 确认框内 "确认删除" 按钮逻辑
  observeEvent(input$confirm_delete_final, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("没有选中任何物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
          DELETE FROM unique_items
          WHERE UniqueID = ?", params = list(selected_items$UniqueID[i]))
        
        sku <- selected_items$SKU[i]
        
        remaining_items <- dbGetQuery(con, "
                            SELECT COUNT(*) AS RemainingCount
                            FROM unique_items
                            WHERE SKU = ?", params = list(sku))
        
        if (remaining_items$RemainingCount[1] > 0) {
          # 库存减一
          adjust_inventory_quantity(con, sku, adjustment = -1)
        } else {
          # 如果没有剩余记录，删除 inventory 表中的该 SKU
          dbExecute(con, "
            DELETE FROM inventory
            WHERE SKU = ?", params = list(sku))
        }
        inventory_refresh_trigger(!inventory_refresh_trigger())
      }
      
      dbCommit(con) # 提交事务
      
      # 通知用户成功删除
      showNotification("物品删除成功！", type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
    })
    
    # 关闭确认框
    removeModal()
  })
  
  
  # 采购商品图片处理模块
  image_manage <- imageModuleServer("image_manage")
  
  # Handle image update button click
  observeEvent(input$update_image_btn, {
    # 1. 确保用户选中了单行
    selected_rows <- unique_items_table_manage_selected_row()
    if (length(selected_rows) != 1) {
      showNotification("请确保只选中一行！", type = "error")
      return()
    }
    
    # 从选中的行获取 SKU
    selected_item <- unique_items_data()[selected_rows, ]
    selected_sku <- selected_item$SKU
    
    if (is.null(selected_sku) || selected_sku == "") {
      showNotification("无法获取所选行的 SKU，请检查！", type = "error")
      return()
    }
    
    # 检查 SKU 是否存在于库存表
    existing_inventory_items <- inventory()
    if (!selected_sku %in% existing_inventory_items$SKU) {
      showNotification("库存中无此 SKU 商品，无法更新图片！", type = "error")
      return()
    }
    
    # 获取当前 SKU 的图片路径
    existing_item <- existing_inventory_items[existing_inventory_items$SKU == selected_sku, ]
    existing_image_path <- existing_item$ItemImagePath[1]
    
    # 处理图片上传或粘贴
    updated_image_path <- process_image_upload(
      sku = selected_sku,
      file_data = image_manage$uploaded_file(),
      pasted_data = image_manage$pasted_file(),
      inventory_path = existing_image_path
    )
    
    # 检查处理结果并更新数据库
    if (!is.null(updated_image_path) && !is.na(updated_image_path)) {
      tryCatch({
        # 更新数据库中 SKU 对应的图片路径
        dbExecute(con, "UPDATE inventory 
                    SET ItemImagePath = ? 
                    WHERE SKU = ?",
                  params = list(updated_image_path, selected_sku))
        
        # 更新数据并触发 UI 刷新
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
        
        # 显示成功通知
        showNotification(paste0("SKU ", selected_sku, " 的图片已成功更新！"), type = "message")
      }, error = function(e) {
        # 数据库操作失败时提示错误
        showNotification("图片路径更新失败，请重试！", type = "error")
      })
    } else {
      # 未检测到有效图片数据
      showNotification("未检测到有效的图片数据，请上传或粘贴图片！", type = "error")
    }
    
    # 重置图片上传状态
    image_manage$reset()
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 瑕疵商品分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 处理登记为瑕疵品
  observeEvent(input$register_defective, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品的状态符合要求（Defect == "无瑕" 或 Defect == "修复"）
      invalid_items <- selected_data[!selected_data$Defect %in% c("无瑕", "修复"), ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘无瑕’或‘修复’状态的物品可以登记为瑕疵品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为瑕疵
        update_status(con, unique_id, defect_status = "瑕疵", refresh_trigger = unique_items_data_refresh_trigger)
        
        # 添加备注
        defect_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = defect_notes,
          status_label = "瑕疵",
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为瑕疵品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  # 处理登记为修复品
  observeEvent(input$register_repair, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品都满足条件（Defect == "瑕疵"）
      invalid_items <- selected_data[selected_data$Defect != "瑕疵", ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘瑕疵’状态的物品可以登记为修复品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为修复
        update_status(con, unique_id, defect_status = "修复", refresh_trigger = unique_items_data_refresh_trigger)
        
        # 添加备注
        repair_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = repair_notes,
          status_label = "修复",
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为修复品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  # 监听“仅显示无瑕品”开关的状态变化
  observeEvent(input$show_perfects_only, {
    if (input$show_perfects_only && input$show_defects_only) {
      updateSwitchInput(session, "show_defects_only", value = FALSE)
    }
  })
  
  # 监听“仅显示瑕疵品”开关的状态变化
  observeEvent(input$show_defects_only, {
    if (input$show_defects_only && input$show_perfects_only) {
      updateSwitchInput(session, "show_perfects_only", value = FALSE)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 国际物流管理分页                                           ##
  ##                                                            ##
  ################################################################
  
  # 筛选逻辑
  itemFilterServer(
    id = "logistic_filter",
    makers_items_map = makers_items_map)
  
  # 登记运单信息
  observeEvent(input$register_shipment_btn, {
    req(input$intl_tracking_number, input$intl_shipping_method, input$intl_total_shipping_cost)
    
    # 获取用户输入的值
    tracking_number <- input$intl_tracking_number
    shipping_method <- input$intl_shipping_method
    total_cost <- as.numeric(input$intl_total_shipping_cost)
    
    tryCatch({
      # 更新或插入运单记录
      dbExecute(
        con,
        "INSERT INTO intl_shipments (TrackingNumber, ShippingMethod, TotalCost, Status)
       VALUES (?, ?, ?, '待分配')
       ON DUPLICATE KEY UPDATE 
         ShippingMethod = VALUES(ShippingMethod), 
         TotalCost = VALUES(TotalCost),
         UpdatedAt = CURRENT_TIMESTAMP",
        params = list(tracking_number, shipping_method, total_cost)
      )
      
      showNotification("国际运单登记成功，信息已更新，可执行挂靠操作！", type = "message", duration = 5)
      
      shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 查询运单逻辑
  observeEvent(input$intl_tracking_number, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      # 如果运单号为空，清空相关输入字段并禁用按钮
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
      return()
    }
    
    tracking_number <- input$intl_tracking_number
    
    tryCatch({
      # 查询运单号对应的信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod, TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) > 0) {
        # 如果运单号存在，回填信息
        updateSelectInput(session, "intl_shipping_method", selected = shipment_info$ShippingMethod[1])
        updateNumericInput(session, "intl_total_shipping_cost", value = shipment_info$TotalCost[1])
        shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
        showNotification("已加载运单信息，可执行挂靠操作！", type = "message", duration = 5)
      } else {
        # 如果运单号不存在，清空相关字段并禁用按钮
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
        shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
        showNotification("未找到对应的运单信息，请登记新运单！", type = "warning", duration = 5)
      }
    }, error = function(e) {
      shinyjs::disable("link_tracking_btn")  # 遇到错误时禁用按钮
      showNotification(paste("加载运单信息失败：", e$message), type = "error")
    })
  })
  
  # 货值汇总显示
  observeEvent(input$batch_value_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error", duration = 5)
      return()
    }
    
    tryCatch({
      # 查询与运单号相关的汇总信息
      summary_info <- dbGetQuery(
        con,
        "
      SELECT 
        COUNT(*) AS TotalQuantity,
        SUM(ProductCost) AS TotalValue,
        SUM(DomesticShippingCost) AS TotalDomesticShipping,
        SUM(IntlShippingCost) AS TotalIntlShipping
      FROM unique_items
      WHERE IntlTracking = ?
      ",
        params = list(tracking_number)
      )
      
      # 查询运单号的运输方式
      shipping_method_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(summary_info) == 0 || is.na(summary_info$TotalQuantity[1])) {
        showNotification("未找到与当前运单号相关的货物信息！", type = "warning")
        return()
      }
      
      # 确定运输方式
      shipping_method <- ifelse(nrow(shipping_method_info) > 0, shipping_method_info$ShippingMethod[1], "未知")
      
      # 计算总价值合计
      total_value_sum <- summary_info$TotalValue[1] + summary_info$TotalDomesticShipping[1] + summary_info$TotalIntlShipping[1]
      
      # 格式化汇总信息
      # 格式化汇总信息
      summary_text <- HTML(paste0(
        "<div style='font-family: Arial, sans-serif; line-height: 2;'>",  # 调整行间距
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left; width: 30%;'>运单号:</td>",
        "<td style='text-align: left; color: #000;'>", tracking_number, " <span style='color: #28A745;'>(", shipping_method, ")</span></td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物数量:</td>",
        "<td style='text-align: left;'>", summary_info$TotalQuantity[1], "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物价值:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalValue[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国内运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalDomesticShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国际运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalIntlShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>合计总价值:</td>",
        "<td style='text-align: left; font-size: 18px; font-weight: bold;'>￥", formatC(total_value_sum, format = "f", digits = 2), "</td>",
        "</tr>",
        "</table>",
        "</div>"
      ))
      
      
      # 创建模态对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #007BFF;'>运单货值汇总</strong>"),
        HTML(summary_text),
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 删除运单逻辑
  observeEvent(input$delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error", duration = 5)
      return()
    }
    
    # 弹出确认对话框
    showModal(modalDialog(
      title = HTML("<strong style='color: #C70039;'>确认删除运单</strong>"),
      HTML(paste0(
        "<p>您确定要删除运单号 <strong>", tracking_number, "</strong> 吗？此操作不可逆！</p>"
      )),
      easyClose = FALSE,
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_delete_shipment_btn", "确认删除", class = "btn-danger")
      )
    ))
  })
  
  # 监听确认删除按钮的点击事件
  observeEvent(input$confirm_delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    tryCatch({
      # 开始事务
      dbBegin(con)
      
      # 从 intl_shipments 表中删除对应的运单号
      rows_affected <- dbExecute(
        con,
        "DELETE FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (rows_affected > 0) {
        # 如果删除成功
        showNotification("运单已成功删除！", type = "message", duration = 5)
        
        # 更新 unique_items 表中相关记录的平摊国际运费为 0.00
        dbExecute(
          con,
          "UPDATE unique_items 
         SET IntlShippingCost = 0.00 
         WHERE IntlTracking IS NULL AND IntlShippingCost > 0.00"
        )
        
        # 清空输入框
        updateTextInput(session, "intl_tracking_number", value = "")
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      } else {
        # 如果没有找到对应的运单号
        showNotification("未找到该运单，删除失败！", type = "warning", duration = 5)
      }
      
      # 提交事务
      dbCommit(con)
    }, error = function(e) {
      # 捕获错误并提示用户，回滚事务
      dbRollback(con)
      showNotification(paste("删除失败：", e$message), type = "error")
    })
    
    shinyjs::disable("link_tracking_btn")  # 禁用按钮
    
    # 更新数据并触发 UI 刷新
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    
    # 关闭确认对话框
    removeModal()
  })
  
  # 点击行自动填写运单号
  observeEvent(unique_items_table_logistics_selected_row(), {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      # 如果没有选中行，清空运单号输入框
      updateTextInput(session, "intl_tracking_number", value = "")
      return()
    }
    
    tryCatch({
      # 获取选中行的数据
      selected_data <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 提取所有选中行的国际物流单号（IntlTracking）
      unique_tracking_numbers <- unique(selected_data$IntlTracking)
      
      if (length(unique_tracking_numbers) == 1 && !is.na(unique_tracking_numbers)) {
        # 如果只有一个唯一的物流单号，填写到输入框
        updateTextInput(session, "intl_tracking_number", value = unique_tracking_numbers)
        showNotification("已根据选中行填写运单号！", type = "message")
      } else {
        # 如果有多个物流单号或为空，清空输入框并提示用户
        updateTextInput(session, "intl_tracking_number", value = "")
        showNotification("选中行包含多个不同的物流单号或为空，请检查！", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    tracking_number <- input$intl_tracking_number  # 获取输入的运单号
    shipping_method <- input$intl_shipping_method  # 获取选择的物流方式
    
    # 校验输入和选择
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 检查物流方式是否一致
      inconsistent_methods <- selected_items %>%
        filter(is.na(IntlShippingMethod) | IntlShippingMethod != shipping_method)
      
      if (nrow(inconsistent_methods) > 0) {
        showNotification("选中物品的物流方式与当前选择的物流方式不一致！", type = "error")
        return()
      }
      
      # 批量更新数据库中的 `IntlTracking`
      dbBegin(con)
      for (i in seq_len(nrow(selected_items))) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlTracking = ? WHERE UniqueID = ?",
          params = list(tracking_number, selected_items$UniqueID[i])
        )
      }
      
      # 查询运单的总运费
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        showNotification("未找到该运单的总运费信息，请检查运单号是否正确。", type = "error")
        dbRollback(con)
        return()
      }
      
      total_cost <- as.numeric(shipment_info$TotalCost)
      
      # 查询挂靠到该运单的所有物品
      related_items <- dbGetQuery(
        con,
        "SELECT UniqueID FROM unique_items WHERE IntlTracking = ?",
        params = list(tracking_number)
      )
      
      if (nrow(related_items) == 0) {
        showNotification("未找到挂靠到该运单的物品。", type = "error")
        dbRollback(con)
        return()
      }
      
      # 计算平摊运费并更新到 `unique_items`
      per_item_cost <- total_cost / nrow(related_items)
      dbExecute(
        con,
        "UPDATE unique_items SET IntlShippingCost = ? WHERE IntlTracking = ?",
        params = list(per_item_cost, tracking_number)
      )
      
      dbCommit(con)
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("运单号已成功挂靠，平摊运费已更新！", type = "message")
    }, error = function(e) {
      # 回滚事务并通知用户
      dbRollback(con)
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 解除运单号挂靠逻辑
  observeEvent(input$delete_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要删除运单号的物品！", type = "error")
      return()
    }
    
    tryCatch({
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 解除运单号关联，清零运费数据
      lapply(selected_items$UniqueID, function(unique_id) {
        dbExecute(
          con,
          "UPDATE unique_items 
         SET IntlTracking = NULL, IntlShippingCost = 0.00
         WHERE UniqueID = ?",
          params = list(unique_id)
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("运单号已成功删除！", type = "message")
    }, error = function(e) {
      showNotification(paste("删除运单号失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听主页面和子页面的切换
  observeEvent({
    list(input$inventory_us, input$query_tabs)  # 仅在这些输入发生变化时触发
  }, {
    if (input$inventory_us == "查询" && input$query_tabs == "商品状态") {
      inventory_refresh_trigger(!inventory_refresh_trigger())
      showNotification("库存表已更新！", type = "message")
    }
  }, ignoreInit = TRUE)  # 忽略初始值
  
  # 物品表过滤模块
  itemFilterServer(
    id = "query_filter",
    makers_items_map = makers_items_map
  )
  
  # 根据SKU产生图表
  observe({
    sku <- trimws(input$query_sku)
    
    if (sku == "") {
      output$query_item_info <- renderUI({ div() })
      output$inventory_status_chart <- renderPlotly({ NULL })
      output$defect_status_chart <- renderPlotly({ NULL })
      return()
    }
    
    tryCatch({
      sku_data <- inventory() %>% filter(SKU == sku)
      
      if (nrow(sku_data) == 0) {
        output$query_item_info <- renderUI({
          div(tags$p("未找到该 SKU 对应的商品信息！", style = "color: red; font-size: 16px;"))
        })
        return()
      }
      
      output$query_item_info <- renderUI({
        img_path <- ifelse(
          is.na(sku_data$ItemImagePath[1]),
          placeholder_150px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )
        
        # 从 unique_items_data() 中计算额外信息
        sku_stats <- unique_items_data() %>%
          filter(SKU == sku) %>%
          summarise(
            美国库存数 = sum(Status == "美国入库", na.rm = TRUE),
            在途库存数 = sum(Status == "国内出库", na.rm = TRUE),
            国内库存数 = sum(Status == "国内入库", na.rm = TRUE),
            已售库存数 = sum(Status %in% c("国内售出", "美国售出", "美国调货", "美国发货"), na.rm = TRUE)
          )
        
        # 渲染图片和表格信息
        div(
          style = "display: flex; flex-direction: column; align-items: center; padding: 10px;",
          div(
            style = "text-align: center; margin-bottom: 10px;",
            tags$img(src = img_path, height = "150px", style = "border: 1px solid #ddd; border-radius: 8px;")
          ),
          div(
            style = "width: 100%; padding-left: 10px;",
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$tr(tags$td(tags$b("商品名称：")), tags$td(sku_data$ItemName[1])),
              tags$tr(tags$td(tags$b("供应商：")), tags$td(sku_data$Maker[1])),
              tags$tr(tags$td(tags$b("分类：")), tags$td(paste(sku_data$MajorType[1], "/", sku_data$MinorType[1]))),
              tags$tr(tags$td(tags$b("平均成本：")), tags$td(sprintf("¥%.2f", sku_data$ProductCost[1]))),
              tags$tr(tags$td(tags$b("平均运费：")), tags$td(sprintf("¥%.2f", sku_data$ShippingCost[1]))),
              tags$tr(tags$td(tags$b("国内库存数：")), tags$td(sku_stats$国内库存数)),
              tags$tr(tags$td(tags$b("在途库存数：")), tags$td(sku_stats$在途库存数)),
              tags$tr(tags$td(tags$b("美国库存数：")), tags$td(sku_stats$美国库存数)),
              tags$tr(tags$td(tags$b("已售库存数：")), tags$td(sku_stats$已售库存数)),
              tags$tr(tags$td(tags$b("总库存数：")), tags$td(sku_data$Quantity[1]))
            )
          )
        )
      })
      
      # 渲染库存状态图表
      output$inventory_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          inventory_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Status) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国售出", "美国发货", "退货")
          status_colors <- c("lightgray", "#c7e89b", "#9ca695", "#46a80d", "#6f52ff", "#529aff", "#869bb8", "#faf0d4", "red")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          inventory_status_data <- merge(
            data.frame(Status = status_levels),
            inventory_status_data,
            by = "Status",
            all.x = TRUE
          )
          inventory_status_data$Count[is.na(inventory_status_data$Count)] <- 0
          
          # 按 status_levels 排序，确保颜色对应
          inventory_status_data <- inventory_status_data[match(status_levels, inventory_status_data$Status), ]
          
          if (sum(inventory_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = inventory_status_data,
              labels = ~Status,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = status_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("库存状态图表生成错误：", e$message), type = "error")
          output$inventory_status_chart <- renderPlotly({ NULL })
        })
      })
      
      # 渲染瑕疵情况图表
      output$defect_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          defect_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Defect) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          defect_levels <- c("未知", "无瑕", "瑕疵", "修复")
          defect_colors <- c("darkgray", "green", "red", "orange")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          defect_status_data <- merge(
            data.frame(Defect = defect_levels),
            defect_status_data,
            by = "Defect",
            all.x = TRUE
          )
          defect_status_data$Count[is.na(defect_status_data$Count)] <- 0
          
          # 按 defect_levels 排序，确保颜色对应
          defect_status_data <- defect_status_data[match(defect_levels, defect_status_data$Defect), ]
          
          if (sum(defect_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = defect_status_data,
              labels = ~Defect,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = defect_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("瑕疵情况图表生成错误：", e$message), type = "error")
          output$defect_status_chart <- renderPlotly({ NULL })
        })
      })
      
    }, error = function(e) {
      showNotification(paste("发生错误：", e$message), type = "error")
    })
  })
  
  # 开销统计
  expense_summary_data <- reactive({
    req(input$time_range) # 确保时间范围存在
    data <- unique_items_data()
    
    # 获取时间范围
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    
    # 根据统计单位生成完整时间序列
    time_sequence <- switch(input$precision,
                            "天" = seq.Date(from = start_date, to = end_date, by = "day"),
                            "周" = seq.Date(from = floor_date(start_date, "week"),
                                           to = floor_date(end_date, "week"),
                                           by = "week"),
                            "月" = seq.Date(from = floor_date(start_date, "month"),
                                           to = floor_date(end_date, "month"),
                                           by = "month"),
                            "年" = seq.Date(from = floor_date(start_date, "year"),
                                           to = floor_date(end_date, "year"),
                                           by = "year"))
    
    # 转换为数据框
    time_df <- data.frame(GroupDate = time_sequence)
    
    # 数据过滤并按选择的单位分组
    summarized_data <- data %>%
      mutate(
        GroupDate = case_when(
          input$precision == "天" ~ as.Date(PurchaseTime),
          input$precision == "周" ~ floor_date(as.Date(PurchaseTime), "week"),
          input$precision == "月" ~ floor_date(as.Date(PurchaseTime), "month"),
          input$precision == "年" ~ floor_date(as.Date(PurchaseTime), "year")
        )
      ) %>%
      group_by(GroupDate) %>%
      summarize(
        TotalExpense = sum(ProductCost + DomesticShippingCost + IntlShippingCost, na.rm = TRUE),
        ProductCost = sum(ProductCost, na.rm = TRUE),
        ShippingCost = sum(DomesticShippingCost + IntlShippingCost, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 将时间序列与统计数据合并，填充缺失值为 0
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(TotalExpense = 0, ProductCost = 0, ShippingCost = 0))
    
    complete_data
  })
  
  # 开销柱状图  
  output$bar_chart <- renderPlotly({
    data <- expense_summary_data()
    
    # 根据用户选择的内容决定显示的 Y 轴数据
    y_var <- switch(input$expense_type,
                    "total" = "TotalExpense",
                    "cost" = "ProductCost",
                    "shipping" = "ShippingCost")
    
    color <- switch(input$expense_type,
                    "total" = "#007BFF",
                    "cost" = "#4CAF50",
                    "shipping" = "#FF5733")
    
    # 绘制柱状图
    plot_ly(data, x = ~GroupDate, y = ~get(y_var), type = "bar",
            name = NULL, marker = list(color = color),
            text = ~round(get(y_var), 2), # 显示数值，保留两位小数
            textposition = "outside") %>% # 数值显示在柱顶外侧
      layout(
        xaxis = list(
          title = "", # 移除 X 轴标题
          tickvals = data$GroupDate, # 显示完整时间序列
          ticktext = format(data$GroupDate, "%Y-%m-%d"), # 格式化为日期
          tickangle = -45, # 倾斜日期标签
          tickfont = list(size = 12),
          showgrid = FALSE # 隐藏网格线
        ),
        yaxis = list(
          title = "采购开销（元）", # 隐藏 Y 轴标题
          tickfont = list(size = 12),
          range = c(0, max(data[[y_var]], na.rm = TRUE) * 1.2) # 调整 Y 轴范围，留出空间显示数值
        ),
        margin = list(l = 50, r = 20, t = 20, b = 50), # 调整边距
        showlegend = FALSE, # 隐藏图例
        plot_bgcolor = "#F9F9F9", # 背景颜色
        paper_bgcolor = "#FFFFFF" # 图表纸张背景颜色
      )
  })
  
  # 总开销分布
  output$pie_chart <- renderPlotly({
    data <- expense_summary_data()
    
    # 饼图数据：计算总开销分布
    total_product_cost <- sum(data$ProductCost, na.rm = TRUE)
    total_shipping_cost <- sum(data$ShippingCost, na.rm = TRUE)
    pie_data <- data.frame(
      Category = c("商品成本", "运费开销"),
      Value = c(total_product_cost, total_shipping_cost)
    )
    
    # 获取时间范围
    time_range <- paste(as.Date(input$time_range[1]), "至", as.Date(input$time_range[2]))
    
    # 绘制饼图
    plot_ly(pie_data, labels = ~Category, values = ~Value, type = "pie",
            textinfo = "value", # 仅显示实际数值
            hoverinfo = "label+percent", # 悬停时显示类别和百分比
            insidetextorientation = "radial",
            marker = list(colors = c("#4CAF50", "#FF5733"))) %>%
      layout(
        title = list(
          text = "总采购开销分布",
          font = list(size = 16, color = "#333", family = "Arial")
        ),
        annotations = list(
          x = 0.5, y = -0.1, # 调整注释的位置
          text = paste("统计时间范围：", time_range),
          showarrow = FALSE,
          font = list(size = 12, color = "#666")
        ),
        showlegend = TRUE, # 显示图例
        paper_bgcolor = "#F9F9F9" # 设置整个图表容器背景色
      )
  })
  
  
  # 清空sku输入框
  observeEvent(input$clear_query_sku_btn, {
    updateTextInput(session, "query_sku", value = "")
  })
  
  # 监听查询页选中inventory table (for SKU query and chart summary)
  observeEvent(input$filtered_inventory_table_query_rows_selected, {
    selected_row <- input$filtered_inventory_table_query_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      # 更新 SKU 输入框(生成库存图表用)
      updateTextInput(session, "query_sku", value = selected_data$SKU)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 数据下载分页                                               ##
  ##                                                            ##
  ################################################################
  
  
  # 动态生成供应商筛选器
  output$download_maker_ui <- renderUI({
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    
    createSearchableDropdown(
      input_id = "download_maker",
      label = "选择供应商:",
      data = makers,
      placeholder = "搜索供应商..."
    )
  })
  
  
  # 监听供应商选择变化并动态更新商品名称
  observe({
    req(unique_items_data())  # 确保数据存在
    
    # 获取用户选择的供应商
    selected_makers <- input$download_maker
    
    # 筛选商品名称
    if (!is.null(selected_makers) && length(selected_makers) > 0) {
      filtered_data <- unique_items_data() %>% filter(Maker %in% selected_makers)
    } else {
      filtered_data <- unique_items_data()
    }
    
    # 提取对应的商品名称，并在前面加一个空选项
    item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())
    
    # 更新商品名称选项，默认选中空选项
    updateSelectizeInput(session, "download_item_name", choices = item_names, selected = "")
  })
  
  # 重置筛选逻辑
  observeEvent(input$download_reset_filters, {
    # 重置供应商筛选为全选
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    updateDropdown.shinyInput(
      session = session,
      inputId = "download_maker",
      options = lapply(makers, function(maker) list(key = maker, text = maker)), # 更新选项
      value = NULL # 重置为未选中状态
    )
    
    # 重置商品名称筛选为空选项
    updateSelectizeInput(session, "download_item_name", choices = "", selected = "")
    updateDateRangeInput(session, "download_date_range", start = Sys.Date() - 365, end = Sys.Date())
  })
  
  
  # 下载物品汇总表为 Excel
  output$download_details_xlsx <- downloadHandler(
    filename = function() {
      paste("物品汇总表（按采购日期）-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品汇总表")
      
      # 获取数据
      data <- filtered_unique_items_data_download()
      req(!is.null(data) && nrow(data) > 0)  # 确保数据非空
      
      data <- map_column_names(data, column_mapping = list(
        SKU = "条形码",
        ItemName = "商品名",
        ItemImagePath = "商品图片",
        Maker = "供应商",
        MajorType = "大类",
        MinorType = "小类",
        ProductCost = "单价",
        DomesticShippingCost = "平摊运费",
        PurchaseTime = "采购日",
        Status = "库存态",
        Defect = "瑕疵态"
      ))
      
      # 按 SKU 计算全局库存统计
      sku_inventory_stats <- data %>%
        group_by(`条形码`) %>%
        summarize(
          总剩余库存数 = sum(`库存态` %in% c("国内入库", "国内出库", "美国入库")),
          国内库存数 = sum(`库存态` == "国内入库"),
          在途库存数 = sum(`库存态` == "国内出库"),
          美国库存数 = sum(`库存态` == "美国入库"),
          无瑕 = sum(`瑕疵态` == "无瑕"),
          瑕疵 = sum(`瑕疵态` == "瑕疵"),
          修复 = sum(`瑕疵态` == "修复"),
          .groups = "drop"
        )
      
      # 按条形码和采购日期分组，统计其他信息
      grouped_data <- data %>%
        group_by(`条形码`, `采购日`) %>%
        summarize(
          商品名 = first(`商品名`),
          商品图片 = first(`商品图片`),
          供应商 = first(`供应商`),
          大类 = first(`大类`),
          小类 = first(`小类`),
          批次单价 = mean(`单价`, na.rm = TRUE),
          批次平摊运费 = mean(`平摊运费`, na.rm = TRUE),
          批次采购数 = n(),  # 记录数
          .groups = "drop"
        )
      
      # 合并全局统计到分组数据
      final_data <- grouped_data %>%
        left_join(sku_inventory_stats, by = "条形码")
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品汇总表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "商品图片")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品汇总表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品汇总表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品汇总表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning", duration = 5)
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品汇总表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品汇总表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message", duration = 5)
    }
  )
  
  
  # 下载物品明细表为 Excel
  output$download_summary_xlsx <- downloadHandler(
    filename = function() {
      paste("物品明细表-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品明细表")
      
      # 获取数据
      final_data <- filtered_unique_items_data_download()
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品明细表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "ItemImagePath")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品明细表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品明细表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品明细表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning", duration = 5)
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品明细表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品明细表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message", duration = 5)
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 移库模块（管理员模式）                                     ##
  ##                                                            ##
  ################################################################
  
  # 管理员登录状态
  admin_logged_in <- reactiveVal(FALSE)
  
  # 监听登录按钮
  observeEvent(input$admin_login_btn, {
    if (input$admin_password == admin_password) {
      admin_logged_in(TRUE)
      showNotification("登录成功！", type = "message")
    } else {
      showNotification("密码错误，请重试！", type = "error")
      admin_logged_in(FALSE)
    }
  })
  
  # 渲染管理员控制
  output$admin_controls <- renderUI({
    if (admin_logged_in()) {
      tagList(
        
        tags$h4("修改库存状态", style = "font-weight: bold; color: #28A745;"),
        
        # 目标状态选择
        selectInput("admin_target_status", "目标库存状态改为：", 
                    choices = c('采购','国内入库','国内出库','国内售出','美国入库','美国售出','美国发货','美国调货','退货'), 
                    selected = NULL, width = "100%"),
        
        # 是否记录修改时间
        checkboxInput("admin_record_timestamp", "记录修改时间", value = FALSE),
        
        # 更新选中物品状态
        actionButton("admin_update_status_btn", "更新库存状态", class = "btn-success", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        tags$h4("修改瑕疵品状态", style = "font-weight: bold; color: #007BFF;"),
        
        # 目标状态选择
        selectInput("admin_target_defect", "目标瑕疵状态改为：", 
                    choices = c('未知','无瑕','瑕疵','修复'), 
                    selected = NULL, width = "100%"),
        
        # 更新选中物品瑕疵品状态
        actionButton("admin_update_defect_btn", "更新瑕疵品状态", class = "btn-info", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        tags$h4("修改库存总数", style = "font-weight: bold; color: #FF5733;"),
        
        # 输入新的库存总数
        numericInput("admin_new_total_quantity", "新库存总数：", value = 0, min = 0, width = "100%"),
        
        # 提交修改库存总数的按钮
        actionButton("admin_update_inventory_btn", "修改库存总数", class = "btn-warning", style = "width: 100%; margin-top: 10px;")
      )
    } else {
      div(tags$p("请输入密码以访问管理员功能", style = "color: red; font-weight: bold; text-align: center;"))
    }
  })
  
  # 使用 uniqueItemsTableServer 渲染表格
  unique_items_table_admin_selected_row <- callModule(uniqueItemsTableServer, "admin_items_table", 
                                                      column_mapping = c(common_columns, list(
                                                        Defect = "瑕疵态",
                                                        UsEntryTime = "入库日",
                                                        UsShippingTime = "出库日",
                                                        IntlShippingMethod = "国际运输",
                                                        OrderID = "订单号"
                                                      )), 
                                                      selection = "multiple", 
                                                      data = unique_items_data)
  
  # 更新库存状态按钮
  observeEvent(input$admin_update_status_btn, {
    req(input$admin_target_status, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取是否记录修改时间的选项
      record_timestamp <- input$admin_record_timestamp
      
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        new_status <- input$admin_target_status
        
        # 调用 update_status 更新物品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = new_status,
          refresh_trigger = unique_items_data_refresh_trigger,
          update_timestamp = record_timestamp  # 使用用户选择的值
        )
      })
      
      # 通知成功并刷新数据
      showNotification("库存状态更新成功！", type = "message")
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("库存状态更新失败：", e$message), type = "error")
    })
  })
  
  observeEvent(input$admin_update_defect_btn, {
    req(input$admin_target_defect, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行瑕疵品状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        target_defect <- input$admin_target_defect  # 获取目标瑕疵品状态
        
        # 调用 update_status 更新瑕疵品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = NULL,  # 不更新物品状态
          defect_status = target_defect,  # 更新瑕疵品状态
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 通知成功并刷新数据
      showNotification("瑕疵品状态更新成功！", type = "message")
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("瑕疵品状态更新失败：", e$message), type = "error")
    })
  })
  
  
  # 更新总库存数按钮
  observeEvent(input$admin_update_inventory_btn, {
    # 获取点选的行数据
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 校验是否有选中物品
    if (is.null(selected_rows) || nrow(selected_items) == 0) {
      showNotification("请先选择至少一件物品！", type = "error")
      return()
    }
    
    # 获取选中物品的 SKU 列表
    sku_counts <- selected_items %>%
      group_by(SKU) %>%
      summarize(SelectedCount = n(), .groups = "drop")  # 按 SKU 聚合
    
    # 获取新库存总数
    new_total_quantity <- input$admin_new_total_quantity
    
    # 校验库存输入
    if (is.null(new_total_quantity) || new_total_quantity < 0) {
      showNotification("库存总数必须为非负数！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历每个 SKU，更新库存总数
      lapply(1:nrow(sku_counts), function(i) {
        sku <- sku_counts$SKU[i]
        selected_count <- sku_counts$SelectedCount[i]
        
        # 检查 SKU 是否存在
        existing_record <- dbGetQuery(con, "SELECT SKU, Quantity FROM inventory WHERE SKU = ?", params = list(sku))
        if (nrow(existing_record) == 0) {
          showNotification(paste0("SKU ", sku, " 不存在！"), type = "error")
          return(NULL)
        }
        
        # 更新库存总数为新值
        dbExecute(con, "
        UPDATE inventory
        SET Quantity = ?
        WHERE SKU = ?",
                  params = list(new_total_quantity, sku)
        )
        
        showNotification(
          paste0("SKU ", sku, " 的库存总数已更新为 ", new_total_quantity, "！"),
          type = "message"
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("修改库存总数时发生错误：", e$message), type = "error")
    })
  })

  
  
  
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}