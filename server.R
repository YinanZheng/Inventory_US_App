# Define server logic
server <- function(input, output, session) {
  # Database
  con <- db_connection()
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 存储 maker_list 数据
  maker_list <- reactiveVal()
  
  # ReactiveVal 用于存储 inventory 数据
  inventory <- reactiveVal(NULL)  # 初始为 NULL，安全处理未加载数据的情况
  
  # ReactiveVal 用于存储 orders 数据
  orders <- reactiveVal()
  
  # ReactiveVal 用于存储 unique item 数据
  unique_item_for_report <- reactiveVal()
  
  # 声明一个 reactiveVal 用于触发unique_items_data刷新
  unique_items_data_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储 PDF 文件路径
  select_pdf_file_path <- reactiveVal(NULL)
  
  # 存储条形码是否已生成的状态
  barcode_generated <- reactiveVal(FALSE)  # 初始化为 FALSE
  
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
  
  # 应用启动时加载数据: inventory
  observe({
    tryCatch({
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))  # 存储到 reactiveVal
    }, error = function(e) {
      inventory(NULL)  # 如果失败，设为空
      showNotification("Initiation: Failed to load inventory data.", type = "error")
    })
  })
  
  # 应用启动时加载数据: orders
  observe({
    tryCatch({
      orders(dbGetQuery(con, "SELECT * FROM orders"))  # 存储到 reactiveVal
    }, error = function(e) {
      orders(NULL)  # 如果失败，设为空
      showNotification("Initiation: Failed to load orders data.", type = "error")
    })
  })
  
  # PDF下载按钮默认禁用
  session$onFlushed(function() {
    shinyjs::disable("download_select_pdf")
  })
  
  ####################################################################################################################################
  ###################################################                              ###################################################
  ###################################################             渲染             ###################################################
  ###################################################                              ###################################################
  ####################################################################################################################################
  
  # 采购商品添加表（临时）
  added_items <- reactiveVal(create_empty_inventory() %>% select(-ShippingCost))
  
  # Render added items table
  output$added_items_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "入库数量",
      ProductCost = "采购单价"
    )
    
    render_table_with_images(
      data = added_items(),
      column_mapping = column_mapping,
      selection = "multiple",
      options = list(fixedHeader = TRUE),
      image_column = "ItemImagePath"  # Specify the correct image column
    )$datatable
  })
  
  ####################################################################################################################################
  
  # 库存表 （过滤）
  filtered_inventory <- reactive({
    
    result <- inventory()
    
    # Return empty inventory if no results
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    result <- result[order(result$updated_at, decreasing = TRUE), ]
    
    return(result)
  })
  
  # Render filtered inventory with column name mapping
  output$filtered_inventory_table_query <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      ProductCost = "平均单价",
      ShippingCost = "平均运费"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      options = list(fixedHeader = TRUE),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )$datatable
  })
  
  ####################################################################################################################################
  
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
      unique_items.IntlAirTracking,
      unique_items.IntlSeaTracking,
      unique_items.PurchaseTime,
      unique_items.DomesticEntryTime,
      unique_items.DomesticExitTime,
      unique_items.DomesticSoldTime,
      unique_items.UsEntryTime,
      unique_items.UsCheckTime,
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
  
  # 采购页过滤
  filtered_unique_items_data_purchase <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "new_maker",
      item_name_input_id = "new_name"
    )
  })
  
  # 入库页过滤
  filtered_unique_items_data_inbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 默认过滤条件 Status 为 “采购” 或 “国内入库”
    data <- data[data$Status %in% c("采购", "国内入库"), ]
    
    filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "inbound_filter-maker",
      item_name_input_id = "inbound_filter-name"
    )
  })
  
  # 出库页过滤
  filtered_unique_items_data_outbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 默认过滤条件：Status  为 “国内入库” 或 “国内出库”
    data <- data[data$Status %in% c("国内入库", "国内出库"), ]
    
    filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "outbound_filter-maker",
      item_name_input_id = "outbound_filter-name"
    )
  })
  
  # 售出页过滤
  filtered_unique_items_data_sold <- reactive({
    req(unique_items_data())
    data <- unique_items_data()

    # 默认过滤条件：Status  为 “国内入库” 或 “美国入库“ 或 "美国调货“ 或 “国内售出”
    data <- data[data$Status %in% c("国内入库", "美国入库", "美国调货", "国内售出"), ]

    filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "sold_filter-maker",
      item_name_input_id = "sold_filter-name"
    )
  })
  
  # 瑕疵品管理页过滤
  filtered_unique_items_data_defect <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 默认过滤条件：状态为“国内入库”且 Defect 不为“未知”
    data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "国内入库", ]
    
    # 处理开关互斥逻辑
    if (isTRUE(input$show_defects_only)) {
      # 如果仅显示瑕疵品
      data <- data[data$Defect == "瑕疵", ]
    } else if (isTRUE(input$show_perfects_only)) {
      # 如果仅显示无瑕品
      data <- data[data$Defect == "无瑕", ]
    }
    
    # 返回过滤后的数据
    data
  })
  
  
  
  # 根据物流方式筛选物品数据
  filtered_unique_items_data_logistics <- reactive({
    data <- unique_items_data()
    shipping_method <- input$intl_shipping_method 
    
    if (!is.null(shipping_method) && shipping_method != "全部") {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
    data
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
      date_range_input_id = "download_date_range"
    )
  })
  
  
  # 缓存目前数据库已有物品的makers：makers_df
  makers_df <- reactive({
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    
    if (!is.null(makers) && length(makers) > 0) {
      data.frame(Maker = makers, stringsAsFactors = FALSE) %>%
        mutate(Pinyin = remove_tone(stringi::stri_trans_general(Maker, "Latin")))
    } else {
      data.frame(Maker = character(), Pinyin = character(), stringsAsFactors = FALSE)
    }
  })
  
  # 订单管理页订单过滤
  filtered_orders <- reactive({
    req(orders())  # 确保数据存在
    
    data <- orders()
    
    # 根据筛选条件动态过滤
    if (input$filter_order_id != "") {
      data <- data %>% filter(grepl(input$filter_order_id, OrderID, ignore.case = TRUE))
    }
    
    if (input$filter_customer_name != "") {
      data <- data %>% filter(grepl(input$filter_customer_name, CustomerName, ignore.case = TRUE))
    }
    
    if (input$filter_platform != "") {
      data <- data %>% filter(Platform == input$filter_platform)
    }
    
    # 动态移除全空的列
    if (all(data$UsTrackingNumber2 == "" | is.na(data$UsTrackingNumber2))) {
      data <- data %>% select(-UsTrackingNumber2)
    }
    
    if (all(data$UsTrackingNumber3 == "" | is.na(data$UsTrackingNumber3))) {
      data <- data %>% select(-UsTrackingNumber3)
    }
    
    data
  })

  
  
  # 渲染物品追踪数据表
  unique_items_table_purchase_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_purchase",
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日期")
                                                         ), data = filtered_unique_items_data_purchase)
  
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- c(common_columns, list(
                                                          PurchaseTime = "采购日期",
                                                          DomesticEntryTime = "入库日期",
                                                          DefectNotes = "瑕疵品备注")
                                                        ), selection = "multiple", data = filtered_unique_items_data_inbound)
  
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
                                                         PurchaseTime = "采购日期",
                                                         DomesticEntryTime = "入库日期",
                                                         DomesticExitTime = "出库日期",
                                                         DomesticSoldTime = "售出日期")
                                                       ), selection = "multiple", data = unique_items_data)
  
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping <- c(common_columns, list(
                                                         PurchaseTime = "采购日期",
                                                         DomesticEntryTime = "入库日期",
                                                         DefectNotes = "瑕疵品备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect)
  
  unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", 
                                                         column_mapping <- c(common_columns, list(
                                                           IntlShippingMethod = "国际运输",
                                                           PurchaseTime = "采购日期",
                                                           DomesticEntryTime = "入库日期",
                                                           DomesticExitTime = "出库日期")
                                                         ), data = filtered_unique_items_data_outbound)
  
  unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold",
                                                     column_mapping <- c(common_columns, list(
                                                       IntlShippingMethod = "国际运输",
                                                       PurchaseTime = "采购日期",
                                                       DomesticSoldTime = "售出日期",
                                                       OrderID = "订单号")
                                                     ), data = filtered_unique_items_data_sold)
  
  unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
                                                          column_mapping = c(common_columns, list(
                                                            IntlShippingMethod = "国际运输",
                                                            PurchaseTime = "采购日期",
                                                            IntlAirTracking = "国际空运单号",
                                                            IntlSeaTracking = "国际海运单号"
                                                          )), selection = "multiple",
                                                          data = filtered_unique_items_data_logistics)
  
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日期",
                                                           DomesticEntryTime = "入库日期",
                                                           DomesticExitTime = "出库日期",
                                                           DomesticSoldTime = "售出日期")
                                                         ), data = filtered_unique_items_data_download)
  
  # 调用模块化的服务器逻辑
  selected_order_row <- callModule(orderTableServer, "orders_table_module",
                                   column_mapping = list(
                                     OrderID = "订单号",
                                     OrderImagePath = "订单图",
                                     CustomerName = "姓名",
                                     Platform = "平台",
                                     UsTrackingNumber1 = "运单",
                                     UsTrackingNumber2 = "运单2",
                                     UsTrackingNumber3 = "运单3",
                                     OrderNotes = "备注"
                                   ),
                                   data = filtered_orders,  # 数据源
                                   selection = "single"  # 单选模式
  )
  
  ####################################################################################################################################
  
  # 显示总采购开销（含运费）
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$ProductCost) + input$new_shipping_cost
    paste0("请核实本次采购总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
           "（其中包含运费: ¥", input$new_shipping_cost, ")")
  })
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 采购分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 供应商模块
  supplierModuleServer(input, output, session, con, maker_list)
  
  # 物品大小类模块
  typeModuleServer("type_module", con, item_type_data)
  
  # Automatically generate SKU when relevant inputs change
  observeEvent({
    input[["type_module-new_major_type"]]
    input[["type_module-new_minor_type"]]
    input$new_name
    input$new_maker
  }, {
    # 安全检查 input$new_name 是否为列表，以及 text 字段是否存在
    new_name_text <- if (is.list(input$new_name) && !is.null(input$new_name$text)) {
      input$new_name$text
    } else {
      NULL
    }
    
    # 判断是否需要清空 SKU
    if (is.null(input$new_maker) || input$new_maker == "" || 
        is.null(new_name_text) || new_name_text == "") {
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      return()
    }
    
    # Dynamically generate SKU
    sku <- generate_sku(
      item_type_data = item_type_data(),
      major_type = input[["type_module-new_major_type"]],
      minor_type = input[["type_module-new_minor_type"]],
      item_name = new_name_text,
      maker = input$new_maker
    )
    
    # Update the SKU input field
    updateTextInput(session, "new_sku", value = sku)
  })
  
  # 缓存商品名，安全处理空值
  item_names <- reactive({
    inventory_data <- inventory()
    if (is.null(inventory_data) || nrow(inventory_data) == 0) {
      return(list())  # 如果没有数据，返回空选项
    }
    lapply(inventory_data$ItemName, function(name) list(key = name, text = name))
  })
  
  # 动态生成ComboBox组件
  output$new_name_combo_box_ui <- renderUI({
    div(
      Label("商品名:", styles = list(
        root = list(
          fontSize = 15,        # 设置字体大小为16px
          fontWeight = "bold",  # 字体加粗
          paddingTop = 0
        )
      )),  # 添加标签
      ComboBox.shinyInput(
        inputId = "new_name",
        value = input$new_name %||% "",        # 默认初始值为空字符串
        options = item_names(),         # 动态加载的选项
        allowFreeform = TRUE,           # 允许用户输入自定义值
        placeholder = "请输入商品名...",
        styles = list(
          root = list(height = 42)
        )
      )
    )
  })
  
  # 采购商品图片处理模块
  image_purchase <- imageModuleServer("image_purchase")
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    # 提取并验证商品名称
    new_name_text <- if (is.list(input$new_name) && !is.null(input$new_name$text)) {
      input$new_name$text
    } else {
      NULL
    }
    
    # 验证输入
    if (is.null(new_name_text) || new_name_text == "") {
      showNotification("请填写正确商品名称！", type = "error")
      return()
    }
    if (is.null(input$new_quantity) || input$new_quantity <= 0) {
      showNotification("请填写正确商品数量！", type = "error")
      return()
    }
    if (is.null(input$new_product_cost) || input$new_product_cost < 0 || input$new_product_cost > 999) {
      showNotification("请填写正确商品单价！", type = "error")
      return()
    }
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常显示！", type = "error")
      return()
    }
    
    # 检查是否存在该 SKU 的库存记录
    inventory_item <- tryCatch({
      dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", params = list(input$new_sku))
    }, error = function(e) {
      showNotification("检查库存时发生错误！", type = "error")
      return(data.frame())
    })
    
    existing_inventory_path <- if (nrow(inventory_item) > 0) inventory_item$ItemImagePath[1] else NULL
    
    # 上传或粘贴图片处理
    new_image_path <- process_image_upload(
      sku = input$new_sku,
      file_data = image_purchase$uploaded_file(),
      pasted_data = image_purchase$pasted_file(),
      inventory_path = existing_inventory_path
    )
    
    # 添加或更新记录
    existing_items <- added_items()
    existing_skus <- existing_items$SKU
    if (input$new_sku %in% existing_skus) {
      sku_index <- which(existing_skus == input$new_sku)
      current_image_path <- existing_items$ItemImagePath[sku_index]
      final_image_path <- if (!is.na(new_image_path) && new_image_path != "") {
        new_image_path
      } else {
        current_image_path
      }
      existing_items[sku_index, ] <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input$new_name$text,
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = final_image_path,
        stringsAsFactors = FALSE
      )
      added_items(existing_items)
      showNotification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # 添加新记录
      new_item <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input$new_name$text,
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = new_image_path,
        stringsAsFactors = FALSE
      )
      added_items(bind_rows(existing_items, new_item))
      showNotification(paste("SKU 已添加:", input$new_sku, "商品名:", input$new_name$text), type = "message")
    }
    
    # 重置
    image_purchase$reset()
  })
  
  # Confirm button: Update database and handle images
  observeEvent(input$confirm_btn, {
    tryCatch({
      if (nrow(added_items()) == 0) {
        showNotification("请先录入至少一个商品!", type = "error")
        return()
      }
      
      added_items_df <- added_items()
      
      # Retrieve total package shipping cost from the UI
      total_shipping_cost <- input$new_shipping_cost
      if (is.null(total_shipping_cost) || total_shipping_cost <= 0) {
        total_shipping_cost <- 0  # Default to 0 if invalid
      }
      
      unit_shipping_cost <- total_shipping_cost / sum(added_items_df$Quantity)
      
      for (i in 1:nrow(added_items_df)) {
        adjust_inventory(
          con = con,
          sku = added_items_df$SKU[i],
          adjustment = 0,  # 采购物品尚未入库，库存不变
          maker = added_items_df$Maker[i],
          major_type = added_items_df$MajorType[i],
          minor_type = added_items_df$MinorType[i],
          item_name = added_items_df$ItemName[i],
          quantity = added_items_df$Quantity[i],
          product_cost = added_items_df$ProductCost[i],
          unit_shipping_cost = unit_shipping_cost,
          image_path = added_items_df$ItemImagePath[i]
        )
      }
      
      # 更新 inventory, unique_items数据并触发 UI 刷新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      # 同时添加信息到 unique_items 表中
      purchase_date <- format(as.Date(input$purchase_date), "%Y-%m-%d")
      
      batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
        sku <- added_items_df$SKU[i]
        quantity <- added_items_df$Quantity[i]
        product_cost <- added_items_df$ProductCost[i]
        
        # Create rows for each quantity
        t(replicate(quantity, c(
          uuid::UUIDgenerate(),
          as.character(sku),
          as.numeric(product_cost),
          as.numeric(unit_shipping_cost),
          "采购",
          "未知",
          purchase_date
        )))
      }))
      
      # Validate data
      if (is.null(batch_data) || nrow(batch_data) == 0) {
        showNotification("采购数据无效，请检查输入！", type = "error")
        return()
      }
      
      # Convert to data frame
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
      
      # Insert into database
      dbBegin(con)
      tryCatch({
        for (i in 1:nrow(batch_data)) {
          dbExecute(con, "INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, PurchaseTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
                    unname(as.vector(batch_data[i, ])))
        }
        dbCommit(con)
        showNotification("所有采购货物已成功登记！", type = "message")
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      }, error = function(e) {
        dbRollback(con)
        showNotification(paste("采购登记失败:", e$message), type = "error")
      })
      
      # Clear added items and reset input fields
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复单价默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateSelectizeInput(session, "new_name", choices = c("", inventory()$ItemName), selected = "")
      image_purchase$reset() # 重置图片
      
      added_items(create_empty_inventory()) #清空添加表
      
    }, error = function(e) {
      showNotification(paste("发生错误:", e$message), type = "error")
    })
  })
  
  # 监听采购页选中items_table
  observeEvent(unique_items_table_purchase_selected_row(), {
    if (!is.null(unique_items_table_purchase_selected_row()) && length(unique_items_table_purchase_selected_row()) > 0) {
      selected_data <- filtered_unique_items_data_purchase()[unique_items_table_purchase_selected_row(), ]
      
      # showNotification(paste("Selected MajorType:", selected_data$MajorType))
      # showNotification(paste("Selected MinorType:", selected_data$MinorType))
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      shinyjs::delay(100, {  # 延迟 100 毫秒
        updateSelectInput(session, "type_module-new_minor_type", selected = selected_data$MinorType)
      })
      updateComboBox.shinyInput(session, "new_name", value = list(key = selected_data$ItemName, text = selected_data$ItemName))
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost) 
      updateNumericInput(session, "new_shipping_cost", value = 0)
    }
  })
  
  # 监听采购页选中added_items_table 用来更改添加数据
  observeEvent(input$added_items_table_rows_selected, {
    selected_row <- input$added_items_table_rows_selected

    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_data <- added_items()[last_selected, ] # 提取最后一个选择的数据

      # 更新侧边栏的输入字段
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      shinyjs::delay(100, {  # 延迟 100 毫秒
        updateSelectInput(session, "type_module-new_minor_type", selected = selected_data$MinorType)
      })
      updateComboBox.shinyInput(session, "new_name", value = list(key = selected_data$ItemName, text = selected_data$ItemName))
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  # Delete selected item
  observeEvent(input$delete_btn, {
    selected_row <- input$added_items_table_rows_selected
    if (length(selected_row) > 0) {
      current_items <- added_items()
      updated_items <- current_items[-selected_row, ]  # Remove selected row
      added_items(updated_items)  # Update reactive value
      showNotification("记录已成功删除", type = "message")
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
  })
  
  # 清空输入
  observeEvent(input$reset_btn, {
    tryCatch({
      # 清空输入控件
      update_maker_choices(session, "new_maker", maker_list())
      updateComboBox.shinyInput(session, "new_name", value = "")
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复单价默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      
      # 重置图片控件
      image_purchase$reset()
      
      # 通知用户
      showNotification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification("清空输入时发生错误，请重试！", type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "inbound_filter",
    makers_df = makers_df,
    unique_items_data = unique_items_data,
    filtered_unique_items_data = filtered_unique_items_data_inbound,
    unique_items_table_selected_row = unique_items_table_inbound_selected_row
  )
  
  # 监听 SKU 输入
  observeEvent(input$inbound_sku, {
    handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    unique_ID <- handleOperation(
      operation_name = "入库",
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      query_status = "采购",
      update_status_value = "国内入库",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      refresh_trigger = NULL,
      session = session,
      input = input
    )
    
    # 检查 unique_ID 是否为空
    if (is.null(unique_ID) || unique_ID == "") {
      return()
    }
    
    # 根据 unique_ID 查询对应的物品信息
    item_info <- dbGetQuery(con, "SELECT SKU FROM unique_items WHERE UniqueID = ?", 
                            params = list(unique_ID))
    
    if (nrow(item_info) == 0) {
      showNotification("未找到对应的物品信息！", type = "error")
      return()
    }
    
    # 提取 SKU 信息
    sku <- item_info$SKU[1]
    
    # 调用 adjust_inventory 增加库存，忽略成本参数
    adjust_result <- adjust_inventory(
      con = con,
      sku = sku,
      adjustment = 1  # 入库时库存增加 1
    )
    
    if (!adjust_result) {
      showNotification("库存增加失败！", type = "error")
      return()
    }
    
    # 检查是否启用了瑕疵品选项
    defective_item <- input$defective_item
    defect_notes <- trimws(input$defective_notes)
    
    # 如果选中瑕疵品并填写了备注
    if (defective_item && defect_notes != "") {
      tryCatch({
        # 调用 add_defective_note 更新备注
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
      # 如果勾选了瑕疵品但没有填写备注
      showNotification("无瑕疵品备注！", type = "warning")
    } else {
      # 未选择瑕疵品时，正常完成入库
      showNotification("入库操作完成！", type = "message")
    }
    
    # 更新 inventory, unique_items数据并触发 UI 刷新
    inventory(dbGetQuery(con, "SELECT * FROM inventory"))
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
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
      select_pdf_file_path(pdf_file)  # 保存生成的 PDF 路径
      
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
      basename(select_pdf_file_path())  # 生成文件名
    },
    content = function(file) {
      file.copy(select_pdf_file_path(), file, overwrite = TRUE)
      shinyjs::disable("download_select_pdf")  # 禁用下载按钮
      select_pdf_file_path(NULL)  # 清空路径
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 出库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "outbound_filter",
    makers_df = makers_df,
    unique_items_data = unique_items_data,
    filtered_unique_items_data = filtered_unique_items_data_outbound,
    unique_items_table_selected_row = unique_items_table_outbound_selected_row
  )
  
  # 监听出库 SKU 输入
  observeEvent(input$outbound_sku, {
    handleSkuInput(
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 确认出库逻辑
  observeEvent(input$confirm_outbound_btn, {
    handleOperation(
      operation_name = "出库",
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      refresh_trigger = unique_items_data_refresh_trigger,
      session = session,
      input = input
    )
  })
  
  # 监听选中行并更新出库 SKU
  observeEvent(unique_items_table_outbound_selected_row(), {
    if (!is.null(unique_items_table_outbound_selected_row()) && length(unique_items_table_outbound_selected_row()) > 0) {
      selected_sku <- filtered_unique_items_data_outbound()[unique_items_table_outbound_selected_row(), "SKU", drop = TRUE]
      updateTextInput(session, "outbound_sku", value = selected_sku)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 售出分页                                                   ##
  ##                                                            ##
  ################################################################

  # 物品表过滤模块
  itemFilterServer(
    id = "sold_filter",
    makers_df = makers_df,
    unique_items_data = unique_items_data,
    filtered_unique_items_data = filtered_unique_items_data_sold,
    unique_items_table_selected_row = unique_items_table_sold_selected_row
  )
  
  ######
  
  # 定义存储运单号动态行数的 reactiveVal，初始值为 1
  tracking_rows <- reactiveVal(1)
  
  # 动态生成运单号输入框
  output$additional_tracking_numbers <- renderUI({
    rows <- tracking_rows()
    
    # 确保至少返回一个空的 UI，否则初始时页面可能为空
    if (rows < 2) {
      return(tagList())
    }
    
    # 动态生成从运单号2开始的输入框
    tracking_inputs <- lapply(2:rows, function(i) {
      textInput(paste0("tracking_number", i), paste0("运单号 ", i), placeholder = "请输入运单号", width = "100%")
    })
    do.call(tagList, tracking_inputs)
  })
  
  # 监听增加运单号按钮点击
  observeEvent(input$add_tracking_btn, {
    rows <- tracking_rows()
    if (rows < 3) {  # 最多允许添加2个运单号
      tracking_rows(rows + 1)
    } else {
      showNotification("最多只能添加 2 个运单号！", type = "warning")
    }
  })
  
  # 响应输入或扫描的 SKU，更新货架上的物品
  observeEvent(input[["sold_filter-sku"]], {
    sku <- trimws(input[["sold_filter-sku"]])  # 清理条形码输入空格
    if (is.null(sku) || sku == "") {
      return()
    }
    
    tryCatch({
      # 从 unique_items_data 获取符合条件的货架物品
      all_shelf_items <- unique_items_data() %>%
        filter(SKU == sku, Status == "国内入库", Defect != "瑕疵") %>%
        select(SKU, UniqueID, ItemName, ProductCost, ItemImagePath) %>%
        arrange(ProductCost)  # 按单价从低到高排序
      
      if (nrow(all_shelf_items) == 0) {
        showNotification("未找到符合条件的物品！", type = "error")
        return()
      }
      
      # 从箱子中获取当前 SKU 的已选数量
      box_data <- box_items()
      box_sku_count <- sum(box_data$SKU == sku)
      
      # 扣除已移入箱子的物品
      if (box_sku_count > 0) {
        all_shelf_items <- all_shelf_items %>%
          slice((box_sku_count + 1):n())  # 移除前 box_sku_count 条记录
      }
      
      # 更新货架
      shelf_items(all_shelf_items)
      showNotification(paste("已加载 SKU:", sku, "的货架物品！"), type = "message")
    }, error = function(e) {
      showNotification(paste("加载货架时发生错误：", e$message), type = "error")
    })
  })
  
  # 出售订单图片处理模块
  image_sold <- imageModuleServer("image_sold")
  
  # 在输入订单号时检查订单信息并填充
  observeEvent(input$order_id, {
    # 检查订单号是否为空
    req(input$order_id)  # 如果订单号为空，停止执行
    
    tryCatch({
      # 查询订单信息，包含新增字段
      existing_order <- dbGetQuery(con, "
      SELECT CustomerName, Platform, UsTrackingNumber1, UsTrackingNumber2, UsTrackingNumber3, OrderNotes 
      FROM orders 
      WHERE OrderID = ?", 
                                   params = list(input$order_id)
      )
      
      # 如果订单存在，填充对应字段
      if (nrow(existing_order) > 0) {
        showNotification("已找到订单信息！字段已自动填充。", type = "message")
        
        # 填充各字段信息
        updateTextInput(session, "customer_name", value = existing_order$CustomerName[1])
        updateSelectInput(session, "platform", selected = existing_order$Platform[1])
        updateTextInput(session, "tracking_number1", value = existing_order$UsTrackingNumber1[1])
        updateTextAreaInput(session, "order_notes", value = existing_order$OrderNotes[1])
        
        # 根据是否存在运单号动态调整输入框显示
        tracking_numbers <- c(
          existing_order$UsTrackingNumber1[1],
          existing_order$UsTrackingNumber2[1],
          existing_order$UsTrackingNumber3[1]
        )
        
        # 动态调整运单号输入栏的数量
        valid_tracking_count <- sum(!is.na(tracking_numbers) & tracking_numbers != "")
        tracking_rows(max(valid_tracking_count, 1))  # 至少显示一个运单号输入框
        
        # 更新运单号 2 和 3
        if (valid_tracking_count > 1) {
          updateTextInput(session, "tracking_number2", value = tracking_numbers[2])
        }
        if (valid_tracking_count > 2) {
          updateTextInput(session, "tracking_number3", value = tracking_numbers[3])
        }
      } else {
        # 如果订单记录不存在，清空所有相关字段
        showNotification("未找到对应订单记录，可登记新订单。", type = "warning")
        
        updateTextInput(session, "customer_name", value = "")
        updateSelectInput(session, "platform", selected = "")
        updateTextInput(session, "tracking_number1", value = "")
        updateTextInput(session, "tracking_number2", value = "")
        updateTextInput(session, "tracking_number3", value = "")
        updateTextAreaInput(session, "order_notes", value = "")
        
        # 隐藏动态运单号输入框
        tracking_rows(1)
      }
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("检查订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 登记订单逻辑
  observeEvent(input$register_order_btn, {
    # 检查订单号是否为空
    if (is.null(input$order_id) || input$order_id == "") {
      showNotification("订单号不能为空！", type = "error")
      return()
    }
    
    # 检查电商平台是否为空
    if (is.null(input$platform) || input$platform == "") {
      showNotification("电商平台不能为空，请选择一个平台！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询是否已有相同订单号的记录
      existing_order <- dbGetQuery(con, "SELECT OrderImagePath FROM orders WHERE OrderID = ?", params = list(input$order_id))
      
      # 确定库存路径（若已有订单记录）
      existing_orders_path <- if (nrow(existing_order) > 0) existing_order$OrderImagePath[1] else NULL
      
      # 处理订单图片
      order_image_path <- process_image_upload(
        sku = input$order_id,
        file_data = image_sold$uploaded_file(),
        pasted_data = image_sold$pasted_file(),
        inventory_path = existing_orders_path
      )
      print(paste("订单图片路径:", order_image_path))  # 调试信息
      
      # 使用 %||% 确保所有参数为长度为 1 的值
      tracking_number1 <- input$tracking_number1 %||% NA
      tracking_number2 <- input$tracking_number2 %||% NA
      tracking_number3 <- input$tracking_number3 %||% NA
      order_notes <- input$order_notes %||% NA
      customer_name <- input$customer_name %||% NA
      platform <- input$platform  # 此时 platform 已验证非空，无需使用 %||%
      
      if (nrow(existing_order) > 0) {
        # 如果订单号已存在，更新图片和其他信息
        dbExecute(con, "
      UPDATE orders 
      SET OrderImagePath = COALESCE(?, OrderImagePath), 
          UsTrackingNumber1 = COALESCE(?, UsTrackingNumber1), 
          UsTrackingNumber2 = COALESCE(?, UsTrackingNumber2),
          UsTrackingNumber3 = COALESCE(?, UsTrackingNumber3),
          OrderNotes = COALESCE(?, OrderNotes),
          CustomerName = COALESCE(?, CustomerName),
          Platform = COALESCE(?, Platform)
      WHERE OrderID = ?",
                  params = list(
                    order_image_path, 
                    tracking_number1, 
                    tracking_number2,
                    tracking_number3,
                    order_notes,
                    customer_name,
                    platform,
                    input$order_id
                  )
        )
        showNotification("订单信息已更新！", type = "message")
      } else {
        # 如果订单号不存在，插入新订单记录
        dbExecute(con, "
      INSERT INTO orders (OrderID, UsTrackingNumber1, UsTrackingNumber2, UsTrackingNumber3, OrderNotes, CustomerName, Platform, OrderImagePath)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                  params = list(
                    input$order_id,
                    tracking_number1,
                    tracking_number2,
                    tracking_number3,
                    order_notes,
                    customer_name,
                    platform,
                    order_image_path
                  )
        )
        showNotification("订单已成功登记！", type = "message")
      }
      
      # 更新订单表格
      orders(dbGetQuery(con, "SELECT * FROM orders"))
      
      # 清空表单内容
      image_sold$reset()  # 清空上传或粘贴的图片
      
    }, error = function(e) {
      # 错误处理
      showNotification(paste("登记订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 监听清空按钮的点击事件
  observeEvent(input$clear_order_btn, {
    # 重置所有输入框
    updateSelectInput(session, "platform", selected = "")
    updateTextInput(session, "order_id", value = "")
    updateTextInput(session, "customer_name", value = "")
    updateTextInput(session, "tracking_number1", value = "")
    
    # 动态隐藏运单号 2 和 3
    tracking_rows(1)  # 重置动态行数
    
    # 清空备注和图片模块
    updateTextAreaInput(session, "order_notes", value = "")
    image_sold$reset()
    
    showNotification("已清空所有输入！", type = "message")
  })
  
  ######
  
  # 渲染货架
  output$shelf_table <- renderDT({
    render_table_with_images(shelf_items(), 
                             column_mapping = list(
                               SKU = "SKU",
                               ItemImagePath = "图片",
                               ItemName = "商品名称",
                               ProductCost = "单价"
                             ), 
                             selection = "single",
                             image_column = "ItemImagePath",
                             options = list(fixedHeader = TRUE))$datatable
  })
  
  # 渲染箱子
  output$box_table <- renderDT({
    render_table_with_images(box_items(), 
                             column_mapping = list(
                               SKU = "SKU",
                               ItemImagePath = "图片",
                               ItemName = "商品名称",
                               ProductCost = "单价"
                             ), 
                             selection = "single",
                             image_column = "ItemImagePath",
                             options = list(fixedHeader = TRUE))$datatable
  })
  
  # 点击货架物品，移入箱子
  observeEvent(input$shelf_table_rows_selected, {
    selected_row <- input$shelf_table_rows_selected
    shelf_data <- shelf_items()
    
    if (!is.null(selected_row) && nrow(shelf_data) >= selected_row) {
      selected_item <- shelf_data[selected_row, ]  # 获取选中的物品
      
      # 更新箱子内容
      current_box <- box_items()
      box_items(bind_rows(current_box, selected_item))
      
      # 更新货架上的物品，移除已选的
      updated_shelf <- shelf_data[-selected_row, ]
      shelf_items(updated_shelf)
      
      showNotification("物品已移入箱子！", type = "message")
    }
  })
  
  # 点击箱子物品，还回货架
  observeEvent(input$box_table_rows_selected, {
    selected_row <- input$box_table_rows_selected
    box_data <- box_items()
    
    if (!is.null(selected_row) && nrow(box_data) >= selected_row) {
      selected_item <- box_data[selected_row, ]  # 获取选中的物品
      
      # 更新货架内容
      current_shelf <- shelf_items()
      shelf_items(bind_rows(current_shelf, selected_item))
      
      # 更新箱子内的物品，移除已选的
      updated_box <- box_data[-selected_row, ]
      box_items(updated_box)
      
      showNotification("物品已还回货架！", type = "message")
    }
  })
  
  # 确认售出
  observeEvent(input$confirm_order_btn, {
    if (is.null(input$order_id) || nrow(box_items()) == 0) {
      showNotification("订单号或箱子内容不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历箱子内物品，减库存并更新物品状态
      lapply(1:nrow(box_items()), function(i) {
        item <- box_items()[i, ]
        sku <- item$SKU
        
        # 调整库存：减少数量
        adjustment_result <- adjust_inventory(
          con = con,
          sku = sku,
          adjustment = -1  # 减少 1 的库存数量
        )
        
        inventory(dbGetQuery(con, "SELECT * FROM inventory"))
        
        if (!adjustment_result) {
          showNotification(paste("库存调整失败：SKU", sku, "，操作已终止！"), type = "error")
          stop("库存调整失败")
        }
        
        # 更新 unique_items 表中的状态
        update_status(
          con = con,
          unique_id = item$UniqueID,
          new_status = "国内售出",
          shipping_method = input$sold_shipping_method,
          refresh_trigger = unique_items_data_refresh_trigger
        )
        
        # 更新订单号
        update_order_id(
          con = con,
          unique_id = item$UniqueID,
          order_id = input$order_id
        )
      })
      
      # 更新 inventory, unique_items数据并触发 UI 刷新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      
      showNotification("订单已完成售出并更新状态！", type = "message")
      
      # 清空箱子
      box_items(create_empty_shelf_box())
      
      # 重置所有输入框
      updateSelectInput(session, "platform", selected = "")
      updateTextInput(session, "order_id", value = "")
      updateTextInput(session, "customer_name", value = "")
      updateTextInput(session, "tracking_number1", value = "")
      
      # 动态隐藏运单号 2 和 3
      tracking_rows(1)  # 重置动态行数
      
      # 清空备注和图片模块
      updateTextAreaInput(session, "order_notes", value = "")
      image_sold$reset()
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  
  
  ###################################################################################################################
  ###################################################################################################################
  ###################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 物品管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 删除选定物品
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- unique_items_data()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
        DELETE FROM unique_items
        WHERE UniqueID = ?", params = list(selected_items$UniqueID[i]))
        
        # 重新计算平均 ProductCost 和 ShippingCost
        sku <- selected_items$SKU[i]
        
        remaining_items <- dbGetQuery(con, "
        SELECT AVG(ProductCost) AS AvgProductCost, 
               AVG(DomesticShippingCost) AS AvgShippingCost,
               COUNT(*) AS RemainingCount
        FROM unique_items
        WHERE SKU = ?", params = list(sku))
        
        if (remaining_items$RemainingCount[1] > 0) {
          # 更新 inventory 表的平均单价和库存数量
          dbExecute(con, "
          UPDATE inventory
          SET Quantity = ?, 
              ProductCost = ?, 
              ShippingCost = ?
          WHERE SKU = ?", 
                    params = list(
                      remaining_items$RemainingCount[1],
                      remaining_items$AvgProductCost[1],
                      remaining_items$AvgShippingCost[1],
                      sku
                    ))
        } else {
          # 如果没有剩余记录，删除 inventory 表中的该 SKU
          dbExecute(con, "
          DELETE FROM inventory
          WHERE SKU = ?", params = list(sku))
        }
      }
      
      dbCommit(con) # 提交事务
      
      showNotification("物品删除成功！", type = "message")
      
      # 更新 inventory, unique_items数据并触发 UI 刷新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
    })
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
        
        # 更新 inventory, unique_items数据并触发 UI 刷新
        inventory(dbGetQuery(con, "SELECT * FROM inventory"))
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
        update_status(con, unique_id, "国内入库", defect_status = "瑕疵", refresh_trigger = unique_items_data_refresh_trigger)
        
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
        update_status(con, unique_id, "国内入库", defect_status = "修复", refresh_trigger = unique_items_data_refresh_trigger)
        
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
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()
    tracking_number <- input$intl_tracking_number
    shipping_method <- input$intl_shipping_method
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中的物品
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 检查运输方式一致性
      inconsistent_methods <- selected_items %>%
        filter(is.na(IntlShippingMethod) | IntlShippingMethod != shipping_method)
      
      if (nrow(inconsistent_methods) > 0) {
        showNotification("选中物品的物流方式与当前选择不符！", type = "error")
        return()
      }
      
      # 准备批量更新的参数
      update_data <- selected_items %>%
        mutate(
          IntlAirTracking = ifelse(shipping_method == "空运", tracking_number, NA),
          IntlSeaTracking = ifelse(shipping_method == "海运", tracking_number, NA)
        ) %>%
        select(UniqueID, IntlAirTracking, IntlSeaTracking)
      
      # 批量更新数据库
      dbBegin(con)
      for (i in 1:nrow(update_data)) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlAirTracking = ?, IntlSeaTracking = ? WHERE UniqueID = ?",
          params = list(update_data$IntlAirTracking[i], update_data$IntlSeaTracking[i], update_data$UniqueID[i])
        )
      }
      dbCommit(con)
      
      # 刷新数据
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      showNotification("运单号已成功挂靠！", type = "message")
      
    }, error = function(e) {
      dbRollback(con)  # 如果发生错误，回滚事务
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 删除运单号逻辑
  observeEvent(input$delete_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要删除运单号的物品！", type = "error")
      return()
    }
    
    tryCatch({
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 删除运单号
      lapply(selected_items$UniqueID, function(unique_id) {
        dbExecute(
          con,
          "UPDATE unique_items 
         SET IntlAirTracking = NULL, IntlSeaTracking = NULL 
         WHERE UniqueID = ?",
          params = list(unique_id)
        )
      })
      
      # 刷新数据
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      showNotification("运单号已成功删除！", type = "message")
      
    }, error = function(e) {
      showNotification(paste("删除运单号失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 订单管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 监听筛选条件变化
  observe({
    input$filter_order_id
    input$filter_customer_name
    input$filter_platform
    filtered_orders()  # 自动触发数据刷新
  })
  
  # 订单图片处理模块
  image_order_manage <- imageModuleServer("image_order_manage")
  
  # 选择某个订单后，渲染关联物品表
  observeEvent(selected_order_row(), {
    selected_row <- selected_order_row()
    req(selected_row)  # 确保用户选择了一行
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]  
    order_id <- selected_order$OrderID
    customer_name <- selected_order$CustomerName
    
    # 填充左侧订单信息栏
    updateTextInput(session, "update_customer_name", value = customer_name)
    updateSelectInput(session, "update_platform", selected = selected_order$Platform)
    updateTextInput(session, "update_tracking_number1", value = selected_order$UsTrackingNumber1)
    updateTextInput(session, "update_tracking_number2", value = selected_order$UsTrackingNumber2)
    updateTextInput(session, "update_tracking_number3", value = selected_order$UsTrackingNumber3)
    updateTextAreaInput(session, "update_order_notes", value = selected_order$OrderNotes)
    
    # 动态更新标题
    output$associated_items_title <- renderUI({
      tags$h4(
        sprintf("#%s - %s 的订单物品", order_id, customer_name),
        style = "color: #007BFF; font-weight: bold;"
      )
    })
    
    # 渲染关联物品表
    associated_items <- reactive({
      # 根据订单号筛选关联物品
      items <- unique_items_data() %>% filter(OrderID == order_id)
      
      # 动态移除列
      if (all(items$IntlShippingMethod == "空运" | is.na(items$IntlShippingMethod))) {
        items <- items %>% select(-IntlSeaTracking)  # 移除海运单号列
      } else if (all(items$IntlShippingMethod == "海运" | is.na(items$IntlShippingMethod))) {
        items <- items %>% select(-IntlAirTracking)  # 移除空运单号列
      }
      
      items
    })
    
    # 使用 uniqueItemsTableServer 渲染关联物品表
    callModule(uniqueItemsTableServer, "associated_items_table_module",
               column_mapping = c(common_columns, list(
                 PurchaseTime = "采购日期",
                 IntlShippingMethod = "国际运输",
                 IntlAirTracking = "国际空运单号",
                 IntlSeaTracking = "国际海运单号"
               )),
               data = associated_items)
  })
  
  # 更新订单逻辑
  observeEvent(input$update_order_btn, {
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    # 获取当前订单的图片路径
    existing_image_path <- selected_order$OrderImagePath
    
    # 处理图片上传或粘贴
    updated_image_path <- process_image_upload(
      sku = order_id,  # 使用订单号作为 SKU
      file_data = image_order_manage$uploaded_file(),
      pasted_data = image_order_manage$pasted_file(),
      inventory_path = existing_image_path
    )
    
    tryCatch({
      # 更新订单信息
      dbExecute(con, "
      UPDATE orders 
      SET CustomerName = ?, 
          Platform = ?, 
          UsTrackingNumber1 = ?, 
          UsTrackingNumber2 = ?, 
          UsTrackingNumber3 = ?, 
          OrderNotes = ?, 
          OrderImagePath = ?
      WHERE OrderID = ?",
                params = list(
                  input$update_customer_name,   # 更新的顾客姓名
                  input$update_platform,        # 更新的电商平台
                  input$update_tracking_number1, # 更新的运单号1
                  input$update_tracking_number2, # 更新的运单号2
                  input$update_tracking_number3, # 更新的运单号3
                  input$update_order_notes,      # 更新的备注
                  updated_image_path,           # 更新的图片路径
                  order_id                      # 更新的订单号
                )
      )
      showNotification("订单信息已成功更新！", type = "message")
      
      # 更新orders，触发表格刷新
      orders(dbGetQuery(con, "SELECT * FROM orders"))
      
      # 清空左侧输入栏
      updateTextInput(session, "update_customer_name", value = "")
      updateSelectInput(session, "update_platform", selected = "")
      updateTextInput(session, "update_tracking_number1", value = "")
      updateTextInput(session, "update_tracking_number2", value = "")
      updateTextInput(session, "update_tracking_number3", value = "")
      updateTextAreaInput(session, "update_order_notes", value = "")
      image_order_manage$reset()  # 重置图片模块
    }, error = function(e) {
      showNotification(paste("更新订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 删除订单逻辑
  observeEvent(input$delete_order_btn, {
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
          adjust_inventory(
            con = con,
            sku = item$SKU,
            adjustment = 1  # 增加库存数量
          )
          
          # 恢复物品状态到“国内入库”
          update_status(
            con = con,
            unique_id = item$UniqueID,
            new_status = "国内入库"
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
      
      # 刷新数据
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      orders(dbGetQuery(con, "SELECT * FROM orders"))
      
      # 清空左侧输入栏
      updateTextInput(session, "update_customer_name", value = "")
      updateSelectInput(session, "update_platform", selected = "")
      updateTextInput(session, "update_tracking_number1", value = "")
      updateTextInput(session, "update_tracking_number2", value = "")
      updateTextInput(session, "update_tracking_number3", value = "")
      updateTextAreaInput(session, "update_order_notes", value = "")
      image_order_manage$reset()  # 重置图片模块
      
      # 清空关联物品表
      output$associated_items_table <- renderDT({ NULL })
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
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
      # 查询 SKU 数据
      sku_query <- "
      SELECT
        ItemName, Maker, MajorType, MinorType, Quantity,
        ProductCost, ShippingCost, ItemImagePath
      FROM inventory
      WHERE SKU = ?"
      sku_data <- dbGetQuery(con, sku_query, params = list(sku))
      
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
        div(
          style = "display: flex; align-items: center; padding: 10px;",
          div(style = "flex: 1; text-align: center; margin-right: 20px;",
              tags$img(src = img_path, height = "150px", style = "border: 1px solid #ddd; border-radius: 8px;")),
          div(style = "flex: 2; display: flex; flex-direction: column; justify-content: center;",
              tags$p(tags$b("商品名称："), sku_data$ItemName[1]),
              tags$p(tags$b("供应商："), sku_data$Maker[1]),
              tags$p(tags$b("分类："), paste(sku_data$MajorType[1], "/", sku_data$MinorType[1])),
              tags$p(tags$b("总库存数："), sku_data$Quantity[1]),
              tags$p(tags$b("平均单价："), sprintf("¥%.2f", sku_data$ProductCost[1])),
              tags$p(tags$b("平均运费："), sprintf("¥%.2f", sku_data$ShippingCost[1]))
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
          status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货")
          status_colors <- c("lightgray", "#c7e89b", "#4B4B4B", "#46a80d", "#173b02", "#A9A9A9", "red")
          
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
                showlegend = TRUE, # 显示图例
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
                showlegend = TRUE, # 显示图例
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
        TotalExpense = sum(ProductCost + DomesticShippingCost, na.rm = TRUE),
        ProductCost = sum(ProductCost, na.rm = TRUE),
        ShippingCost = sum(DomesticShippingCost, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 将时间序列与统计数据合并，填充缺失值为 0
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(TotalExpense = 0, ProductCost = 0, ShippingCost = 0))
    
    complete_data
  })
  
  # 
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
  
  #
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
    maker_options <- lapply(makers, function(maker) list(key = maker, text = maker))
    
    div(
      style = "padding-bottom: 15px;", # 外层 div 设置内边距和字体大小
      Dropdown.shinyInput(
        inputId = "download_maker",
        label = "选择供应商:",
        options = maker_options,
        multiSelect = TRUE,
        placeholder = "请选择供应商..."
      )
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
    updateSelectizeInput(session, "download_maker", choices = makers, selected = makers)
    
    # 重置商品名称筛选为空选项
    item_names <- c("")  # 仅包含空选项
    updateSelectizeInput(session, "download_item_name", choices = item_names, selected = "")
  })
  
  # 下载物品表为 Excel
  output$download_unique_items_xlsx <- downloadHandler(
    filename = function() {
      paste("unique_items-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品明细表")
      
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
        PurchaseTime = "采购日期",
        Status = "库存状态",
        Defect = "物品状态"
      ))
      
      # 按 SKU 计算全局库存统计
      sku_inventory_stats <- data %>%
        group_by(`条形码`) %>%
        summarize(
          总剩余库存数 = sum(`库存状态` %in% c("国内入库", "国内出库", "美国入库")),
          国内库存数 = sum(`库存状态` == "国内入库"),
          在途库存数 = sum(`库存状态` == "国内出库"),
          美国库存数 = sum(`库存状态` == "美国入库"),
          无瑕 = sum(`物品状态` == "无瑕"),
          瑕疵 = sum(`物品状态` == "瑕疵"),
          修复 = sum(`物品状态` == "修复"),
          .groups = "drop"
        )
      
      # 按条形码和采购日期分组，统计其他信息
      grouped_data <- data %>%
        group_by(`条形码`, `采购日期`) %>%
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
      writeData(wb, "物品明细表", final_data, startCol = 1, startRow = 1)
      
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
        tags$h4("物品状态管理", style = "color: #007BFF;"),
        
        # 输入 SKU 自动过滤物品
        textInput("admin_input_sku", "", placeholder = "请输入 SKU", width = "100%"),
        
        tags$hr(),
        
        # 目标状态选择
        selectInput("admin_target_status", "目标状态改为：", 
                    choices = c("采购", "国内入库", "国内出库", "国内售出", "美国入库", "美国售出", "退货"), 
                    selected = NULL, width = "100%"),
        
        # 是否记录修改时间
        checkboxInput("admin_record_timestamp", "记录修改时间", value = FALSE),
        
        # 更新选中物品状态
        actionButton("admin_update_status_btn", "更新选中物品状态", class = "btn-success", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        actionButton("oauth_btn", "登录并授权 USPS", icon = icon("sign-in-alt")),
        h4("授权结果"),
        verbatimTextOutput("oauth_status")
        
      )
    } else {
      div(tags$p("请输入密码以访问管理员功能", style = "color: red; font-weight: bold; text-align: center;"))
    }
  })
  
  # 通过 SKU 筛选物品数据
  filtered_unique_items_data_admin <- reactive({
    data <- unique_items_data()
    sku <- trimws(input$admin_input_sku)
    
    # 自动过滤 SKU，同时排除 NA 值
    if (!is.null(sku) && sku != "") {
      data <- data %>% filter(!is.na(SKU) & SKU == sku)
    }
    
    data
  })
  
  # 使用 uniqueItemsTableServer 渲染表格
  unique_items_table_admin_selected_row <- callModule(uniqueItemsTableServer, "admin_items_table", 
                                                      column_mapping = c(common_columns, list(
                                                        PurchaseTime = "采购日期",
                                                        DomesticEntryTime = "入库日期",
                                                        DomesticExitTime = "出库日期",
                                                        DomesticSoldTime = "出售日期",
                                                        IntlShippingMethod = "国际运输",
                                                        OrderID = "订单号"
                                                      )), 
                                                      selection = "multiple", 
                                                      data = filtered_unique_items_data_admin)
  
  # 监听更新状态按钮
  observeEvent(input$admin_update_status_btn, {
    req(input$admin_target_status, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- filtered_unique_items_data_admin()[selected_rows, ]
    
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
      showNotification("物品状态更新成功！", type = "message")
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("状态更新失败：", e$message), type = "error")
    })
  })
  
  
  usps_auth_url <- "https://developer.usps.com/oauth/authorize"
  usps_token_url <- "https://developer.usps.com/oauth/token"
  client_id <- "KNqnm8z0iVobHrDWVMhxoDM1R3FiMbTh"  # 替换为实际的 Key
  client_secret <- "AgBRw1H1pdWcB97Y"  # 替换为实际的 Secret
  redirect_uri <- "http://54.254.120.88:3838/inventory/callback"
  
  observeEvent(input$oauth_btn, {
    auth_url <- paste0(
      usps_auth_url, "?response_type=code&client_id=", client_id,
      "&redirect_uri=", URLencode(redirect_uri)
    )
    session$sendCustomMessage(type = "navigate", message = auth_url)
  })
  
  observe({
    # 监听回调 URL 参数
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # 获取授权码
      auth_code <- query$code
      output$oauth_status <- renderText(paste("授权码：", auth_code))
      
      # 使用授权码换取访问令牌
      response <- httr::POST(
        url = usps_token_url,
        body = list(
          client_id = client_id,
          client_secret = client_secret,
          code = auth_code,
          redirect_uri = redirect_uri,
          grant_type = "authorization_code"
        ),
        encode = "form"
      )
      
      # 解析访问令牌
      if (response$status_code == 200) {
        token_data <- httr::content(response, "parsed")
        access_token <- token_data$access_token
        output$oauth_status <- renderText(paste("访问令牌：", access_token))
      } else {
        output$oauth_status <- renderText("无法获取访问令牌，请检查配置。")
      }
    }
  })

  
  
  
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}