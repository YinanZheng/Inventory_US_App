# Define server logic
server <- function(input, output, session) {
  
  source("global.R", local = TRUE)
  
  ##############################################################################
  
  # 显示加载动画
  plan(multicore)  # 让数据加载异步执行，避免阻塞 UI
  shinyjs::show("loading-screen")  # 显示加载界面
  
  future({
    return(TRUE)  # 任务完成
  }) %>% 
    promises::then(function(result) {
      shinyjs::runjs("$('#loading-screen').fadeOut(1000);")  # 1秒淡出加载界面
    })
  
  ##############################################################################
  
  # Database
  con <- db_connection()
  
  # 初始化 requests_data 和 unique_items_data
  requests_data <- reactiveVal(NULL)
  unique_items_data <- reactiveVal(NULL)
  
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
  
  # 创建全局环境变量用于存储缓存数据
  cache_env <- new.env()
  
  ####################################################################################################################################
  
  observeEvent(input$user_timezone, {
    req(input$user_timezone)  # 确保 input$user_timezone 已经获取
    
    # 服务器 UTC 时间
    utc_time <- Sys.time()
    
    # 转换 UTC 时间到用户本地时间
    user_time <- format(as.POSIXct(utc_time, tz = "UTC"), tz = input$user_timezone, usetz = TRUE)
    
    time_info <- HTML(paste0(
      "📌 <b>服务器 UTC 时间:</b><br> ", format(utc_time, "%Y-%m-%d %H:%M:%S UTC"), "<br><br>",
      "🌎 <b>你的时区:</b><br> ", input$user_timezone, "<br><br>",
      "⏰ <b>本地时间:</b><br> ", user_time
    ))
    
    showNotification(time_info, type = "message", duration = 10)
  })
  
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
    inventory_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # 商品名自动联想
  item_names <- reactive({
    req(inventory())
    unique(inventory()$ItemName)  # 提取唯一的商品名
  })
  
  # 物品追踪表
  unique_items_data <- reactivePoll(
    intervalMillis = poll_interval, 
    session = session,       # 绑定 Shiny session，确保只在活跃时运行
    
    # **检查是否需要更新**（返回最近更新时间）
    checkFunc = function() {
      db_time <- dbGetQuery(con, "SELECT last_updated FROM update_log WHERE table_name = 'unique_items'")[[1]]
      trigger_val <- unique_items_data_refresh_trigger()
      paste(db_time, trigger_val)
    },
    
    # **获取最新数据**
    valueFunc = function() {
      result <- dbGetQuery(con, "
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
        unique_items.PurchaseCheck,
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
      
      dbWithTransaction(con, {
        # **当 `unique_items` 变更时，自动更新 `inventory`**
        dbExecute(con, "
          UPDATE inventory i
          JOIN (
            SELECT 
              SKU,
              AVG(ProductCost) AS AvgProductCost,
              AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost,
              SUM(Status IN ('国内入库', '国内出库', '美国入库')) AS TotalQuantity,
              SUM(Status = '国内入库') AS DomesticQuantity,
              SUM(Status = '国内出库') AS TransitQuantity,
              SUM(Status = '美国入库') AS UsQuantity,
              MAX(updated_at) AS LatestUpdateTime
            FROM unique_items
            GROUP BY SKU
          ) u ON i.SKU = u.SKU
          SET 
            i.ProductCost = ROUND(u.AvgProductCost, 2),
            i.ShippingCost = ROUND(u.AvgShippingCost, 2),
            i.Quantity = u.TotalQuantity,
            i.DomesticQuantity = u.DomesticQuantity,
            i.TransitQuantity = u.TransitQuantity,
            i.UsQuantity = u.UsQuantity,
            i.updated_at = u.LatestUpdateTime
        ")
        
        # 删除不存在的 SKU
        dbExecute(con, "
          DELETE i FROM inventory i
          LEFT JOIN unique_items u ON i.SKU = u.SKU
          WHERE u.SKU IS NULL
        ")
      })
      return(result)
    }
  )
  
  # 加载当前已有的 makers 和 item names 的对应关系
  observe({
    unique_data <- unique_items_data()  # 数据源
    makers_items <- unique_data %>%
      select(Maker, ItemName) %>%  # 选择需要的列
      distinct()                   # 确保唯一性
    
    makers_items_map(makers_items)  # 更新 reactiveVal
  })
  
  ####################################################################################################################################
  
  # 订单表
  orders <- reactive({
    # 当 refresh_trigger 改变时触发更新
    orders_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM orders")
  })
  
  ####################################################################################################################################

  clear_invalid_item_status_history(con)
  
  ####################################################################################################################################
  
  
  
  ############################################
  ######   unique_items_data 表的过滤   ######
  ############################################
  
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
      status_input_id = "inbound_filter-status",
      item_name_input_id = "inbound_filter-name",
      other_input_id = "inbound_filter-other",
      purchase_date_range_id = "inbound_filter-purchase_date_range"
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
  
  # 订单管理页订单过滤 （万能搜素框）
  debounced_filter_combined <- debounce(
    reactive({ trimws(input$filter_combined) }),  # Trim whitespace from input
    millis = 500  # Set debounce delay to 500 milliseconds
  )
  
  filtered_orders <- reactive({
    req(orders())  # 确保订单数据存在
    
    data <- orders()  # 获取所有订单数据
    
    # 组合搜索逻辑，使用防抖输入
    search_term <- debounced_filter_combined()
    if (!is.null(search_term) && length(search_term) > 0 && nzchar(search_term)) {
      # 判断是否可能是运单号：仅包含数字且长度合理
      cleaned_search_term <- gsub("[^0-9]", "", trimws(search_term))
      is_tracking_like <- nchar(cleaned_search_term) >= 22 && cleaned_search_term == trimws(search_term)
      
      if (is_tracking_like) {
        # 特殊情况：按运单号匹配
        data <- match_tracking_number(data, "UsTrackingNumber", search_term)
      } else {
        # 普通搜索逻辑
        # Step 1: 直接过滤主要字段
        main_filtered <- data %>% filter(
          grepl(search_term, OrderID, ignore.case = TRUE) |
            grepl(search_term, UsTrackingNumber, ignore.case = TRUE) |
            grepl(search_term, CustomerName, ignore.case = TRUE) |
            grepl(search_term, CustomerNetName, ignore.case = TRUE) |
            grepl(search_term, OrderNotes, ignore.case = TRUE)
        )
        
        # Step 2: 使用 unique_items_data 过滤 SKU 或 ItemName
        req(unique_items_data())
        sku_or_item_orders <- unique_items_data() %>%
          filter(
            grepl(search_term, SKU, ignore.case = TRUE) |
              grepl(search_term, ItemName, ignore.case = TRUE)
          ) %>%
          pull(OrderID) %>%
          unique()
        
        # Step 3: 合并结果 - 主字段或 SKU/ItemName 匹配的订单
        data <- data %>% filter(
          OrderID %in% sku_or_item_orders | 
            OrderID %in% main_filtered$OrderID
        )
      }
    }
    
    # 按平台过滤
    if (!is.null(input$filter_platform) && input$filter_platform != "") {
      data <- data %>% filter(Platform == input$filter_platform)
    }
    
    # 按订单状态过滤
    if (!is.null(input$filter_order_status) && input$filter_order_status != "") {
      data <- data %>% filter(OrderStatus == input$filter_order_status)
    }
    
    # 按创建日期过滤
    if (!is.null(input$filter_order_date) && length(input$filter_order_date) >= 2) {
      start_date <- input$filter_order_date[[1]]
      end_date <- input$filter_order_date[[2]]
      data <- data %>% filter(created_at >= start_date & created_at <= end_date)
    }
    
    # 按创建日期降序排序
    data <- data %>% arrange(desc(created_at))
    
    data
  })
  
  # 已经到齐订单
  filtered_orders_arrived <- reactive({
    req(orders(), unique_items_data())  # 确保订单和物品数据存在
    
    # 获取订单和物品数据
    data_orders <- orders()
    data_items <- unique_items_data()
    
    # 筛选订单状态为“备货”的订单
    data_orders <- data_orders %>%
      filter(OrderStatus == "备货")
    
    # 条件 1：订单内所有物品都有国际运单号
    all_with_tracking_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        all_with_tracking = all(!is.na(IntlTracking) & IntlTracking != "")  # 所有物品都有运单号
      ) %>%
      filter(all_with_tracking) %>%  # 筛选符合条件的订单
      pull(OrderID)
    
    # 条件 2：订单内没有任何物品，备注有调货完成记录
    no_items_with_transfer_note_orders <- data_orders %>%
      filter(
        !(OrderID %in% data_items$OrderID) &  # 订单内没有任何物品
          grepl("【调货完成 \\d{4}-\\d{2}-\\d{2}】", OrderNotes)  # 备注包含调货完成记录
      ) %>%
      pull(OrderID)
    
    # 合并两种符合条件的订单
    valid_order_ids <- union(all_with_tracking_orders, no_items_with_transfer_note_orders)
    
    # 返回筛选后的订单
    filtered_orders <- data_orders %>%
      filter(OrderID %in% valid_order_ids)
    
    return(filtered_orders)
  })
  
  # 物品没到齐订单
  filtered_orders_waiting <- reactive({
    req(orders(), unique_items_data())  # 确保订单和物品数据存在
    
    # 获取订单和物品数据
    data_orders <- orders()
    data_items <- unique_items_data()
    
    # 筛选订单状态为“备货”的订单
    data_orders <- data_orders %>%
      filter(OrderStatus == "备货")
    
    # 条件 1：部分物品有国际运单号，部分没有的订单
    partial_tracking_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        has_tracking = any(!is.na(IntlTracking) & IntlTracking != ""),  # 至少一个物品有运单号
        no_tracking = any(is.na(IntlTracking) | IntlTracking == "")    # 至少一个物品没有运单号
      ) %>%
      filter(has_tracking & no_tracking) %>%  # 同时满足上述两种情况
      pull(OrderID)  # 提取符合条件的 OrderID
    
    # 条件 2：所有物品都没有国际运单号，但备注中有调货操作记录的订单
    no_tracking_with_transfer_note_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        all_no_tracking = all(is.na(IntlTracking) | IntlTracking == "")  # 所有物品都没有运单号
      ) %>%
      filter(all_no_tracking) %>%
      pull(OrderID) %>%
      intersect(  # 交集筛选，订单备注包含指定格式的调货记录
        data_orders %>%
          filter(grepl("【调货完成 \\d{4}-\\d{2}-\\d{2}】", OrderNotes)) %>%  # 正则匹配
          pull(OrderID)
      )
    
    # 合并两种符合条件的订单
    valid_order_ids <- union(partial_tracking_orders, no_tracking_with_transfer_note_orders)
    
    # 返回筛选后的订单
    filtered_orders <- data_orders %>%
      filter(OrderID %in% valid_order_ids)
    
    return(filtered_orders)
  })
  
  # 物品管理页过滤
  filtered_unique_items_data_manage <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "manage_filter-maker",
      status_input_id = "manage_filter-status",
      item_name_input_id = "manage_filter-name",
      other_input_id = "manage_filter-other",
      purchase_date_range_id = "manage_filter-purchase_date_range"
    )
    
    data
  })
  
  # 瑕疵品管理页过滤
  filtered_unique_items_data_defect <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "defect_filter-maker",
      item_name_input_id = "defect_filter-name",
      other_input_id = "defect_filter-other",
      purchase_date_range_id = "defect_filter-purchase_date_range"
    )
    
    # 默认过滤条件：状态为“国内入库”且 Defect 不为“未知”
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
    
    # # 只显示本页相关状态
    # data <- data %>%
    #   filter(Status %in% c("国内出库", "国内售出"), Defect != "瑕疵")
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "logistic_filter-maker",
      status_input_id = "logistic_filter-status",
      item_name_input_id = "logistic_filter-name",
      other_input_id = "logistic_filter-other",
      sold_date_range_id = "logistic_filter-sold_date_range",
      only_show_sold_id = "logistic_filter-only_show_sold",
      exit_date_range_id = "logistic_filter-exit_date_range",
      only_show_exit_id = "logistic_filter-only_show_exit"
    )
    
    shipping_method <- input$intl_shipping_method
    
    # 判断并根据物流方式筛选
    if (!is.null(shipping_method)) {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
    # 优先显示没有国际运单号的物品
    data <- data %>% arrange(desc(is.na(IntlTracking)), IntlTracking)
    
    data
  })
  
  # 查询页过滤-库存表
  filtered_inventory <- reactive({
    req(inventory(), unique_items_data()) # 确保数据存在
    
    data <- inventory()
    
    # 如果库存为空，返回空库存表
    if (nrow(data) == 0) {
      return(create_empty_inventory())
    }
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "query_filter-maker",
      item_name_input_id = "query_filter-name",
      other_input_id = "query_filter-other",
      source_type = "inventory"
    )
    
    # 根据售罄筛选
    if (!is.null(input$query_stock_status) && input$query_stock_status != "none") {
      if (input$query_stock_status == "us") {
        data <- data %>% filter(UsQuantity == 0 & DomesticQuantity > 0)  # 美国库存为 0
      } else if (input$query_stock_status == "domestic") {
        data <- data %>% filter(DomesticQuantity == 0 & UsQuantity > 0)  # 国内库存为 0
      } else if (input$query_stock_status == "all") {
        data <- data %>% filter(Quantity == 0)  # 全库存售罄
      }
    }
    
    data <- data[order(data$updated_at, decreasing = TRUE), ]
    return(data)
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
      other_input_id = "download_sku",
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
                                   column_mapping = orders_table_columns,
                                   data = filtered_orders,  # 数据源
                                   user_timezone = input$user_timezone,
                                   selection = "single" # 单选模式
  )
  
  selected_orders_table_arrived_row <- callModule(orderTableServer, "orders_table_arrived",
                                                  column_mapping = orders_table_columns,
                                                  options = modifyList(table_default_options, list(scrollY = "650px")),
                                                  data = filtered_orders_arrived,  # 数据源
                                                  selection = "single" # 单选模式
  )
  
  selected_orders_table_waiting_row <- callModule(orderTableServer, "orders_table_waiting",
                                                  column_mapping = orders_table_columns,
                                                  options = modifyList(table_default_options, list(scrollY = "650px")),
                                                  data = filtered_orders_waiting,  # 数据源
                                                  selection = "single" # 单选模式
  )
  
  # selected_orders_table_relocation_row <- callModule(orderTableServer, "orders_relocation",
  #                                                 column_mapping = orders_table_columns,
  #                                                 options = modifyList(table_default_options, list(scrollY = "650px")),
  #                                                 data = filtered_orders_relocation,  # 数据源
  #                                                 selection = "single" # 单选模式
  # )
  
  # 物品管理分页物品表
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
                                                         PurchaseTime = "采购日",
                                                         DomesticEntryTime = "入库日",
                                                         DomesticExitTime = "出库日",
                                                         DomesticSoldTime = "售出日",
                                                         UsEntryTime = "美入库日",
                                                         UsRelocationTime = "美调货日",
                                                         UsShippingTime = "美发货日",
                                                         OrderID = "订单号")
                                                       ), selection = "multiple", data = filtered_unique_items_data_manage,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = FALSE)))
  
  # 瑕疵品管理分页物品表
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping = c(common_columns, list(
                                                         UsEntryTime = "美入库日",
                                                         Defect = "瑕疵态",
                                                         DefectNotes = "瑕疵备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = FALSE)))
  
  # 国际物流管理分页物品表
  unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
                                                          column_mapping = c(common_columns, list(
                                                            IntlShippingMethod = "国际运输",
                                                            DomesticSoldTime = "售出日",
                                                            DomesticExitTime = "出库日",
                                                            IntlShippingCost = "国际运费",
                                                            IntlTracking = "国际运单"
                                                          )), selection = "multiple",
                                                          data = filtered_unique_items_data_logistics,
                                                          option = modifyList(table_default_options, list(scrollY = "730px", 
                                                                                                          searching = FALSE, 
                                                                                                          paging = TRUE,
                                                                                                          pageLength = 30,
                                                                                                          lengthMenu = c(30, 100, 200, 500, 1000),
                                                                                                          dom = 'lftip')))
  
  # 查询分页库存表
  output$filtered_inventory_table_query <- renderDT({  # input$filtered_inventory_table_query_rows_selected
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      DomesticQuantity = "国内库存数",
      TransitQuantity = "在途库存数",
      UsQuantity = "美国库存数",
      ProductCost = "平均成本",
      ShippingCost = "平均运费"
    )

    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )$datatable
  })
  
  # 下载分页物品表
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           Defect = "瑕疵态",
                                                           PurchaseTime = "采购日",
                                                           UsEntryTime = "美入库日",                                                           
                                                           UsRelocationTime = "美调货日",
                                                           UsShippingTime = "美发货日")
                                                         ), data = filtered_unique_items_data_download)
  
  ####################################################################################################################################
  
  observeEvent(input$refresh_item_table, {
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())  # 触发数据刷新
    orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
  })
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 协作分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 渲染初始供应商筛选器（只定义一次）
  output$supplier_filter <- renderUI({
    selectizeInput(
      inputId = "selected_supplier",
      label = NULL,
      choices = NULL,  # 初始为空，动态更新
      selected = NULL, # 初始无选择
      options = list(
        placeholder = "筛选供应商...",
        searchField = "value",
        maxOptions = 1000,
        create = TRUE,
        allowEmptyOption = TRUE
      )
    )
  })
  
  # 动态更新筛选器选项
  observe({
    current_value <- input$collaboration_tabs
    
    # 映射 tab value 到 RequestType
    tab_value_to_request_type <- list(
      "purchase" = "采购",
      "arranged" = "安排",
      "completed" = "完成",
      "outbound" = "出库",
      "new_product" = "新品"
    )
    
    request_type <- tab_value_to_request_type[[current_value]] %||% "采购"  # 默认值
    
    req(requests_data())
    
    current_requests <- requests_data() %>% filter(RequestType == request_type)
    suppliers <- unique(current_requests$Maker)
    
    # 获取当前选择
    current_selection <- isolate(input$selected_supplier)
    
    # 更新选项，但避免不必要的重新选择
    updateSelectizeInput(
      session,
      inputId = "selected_supplier",
      choices = c("全部供应商", suppliers),
      selected = if (is.null(current_selection) || !current_selection %in% c("全部供应商", suppliers)) NULL else current_selection,
      options = list(
        placeholder = "筛选供应商...",
        create = TRUE,
        allowEmptyOption = TRUE
      )
    )
  }, priority = 10)  # 提高优先级，确保先于其他观察者执行
  
  # 重置按钮逻辑
  observeEvent(input$reset_supplier, {
    updateSelectizeInput(
      session,
      "selected_supplier",
      selected = "全部供应商"
    )
  }, priority = 0)  # 较低优先级，避免干扰选项更新
  
  # 定期检查数据库更新
  poll_requests <- reactivePoll(
    intervalMillis = 20000,
    session = session,
    checkFunc = function() {
      last_updated <- dbGetQuery(con, "SELECT MAX(UpdatedAt) AS last_updated FROM requests")$last_updated[1]
      if (is.null(last_updated)) Sys.time() else last_updated
    },
    valueFunc = function() {
      dbGetQuery(con, "SELECT * FROM requests")
    }
  )
  
  # 使用 debounce 限制轮询频率
  poll_requests_debounced <- debounce(poll_requests, millis = 20000)
  
  observeEvent(poll_requests_debounced(), {
    requests <- poll_requests_debounced()
    requests_data(requests)
    # 确保 input$selected_supplier 已定义
    req(input$selected_supplier)
    refresh_board_incremental(requests, output, input)
  }, priority = 10)
  
  # 初始化时绑定所有按钮
  observeEvent(requests_data(), {
    requests <- requests_data()
    lapply(requests$RequestID, function(request_id) {
      bind_buttons(request_id, requests_data, input, output, session, con)
    })
  }, ignoreInit = FALSE, once = TRUE)
  
  # 使用 observe 监听 requests_data() 和 input$selected_supplier
  observe({
    req(requests_data(), input$selected_supplier)
    requests <- requests_data()
    refresh_board_incremental(requests, output, input, page_size = 30)  # 设置每页大小
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_sku, {
    # 如果 SKU 搜索框有值，则清空物品名称搜索框
    if (input$search_sku != "") {
      updateTextInput(session, "search_name", value = "")  # 清空物品名称搜索框
    }
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_name, {
    # 如果物品名称搜索框有值，则清空 SKU 搜索框
    if (input$search_name != "") {
      updateTextInput(session, "search_sku", value = "")  # 清空 SKU 搜索框
    }
  })
  
  # SKU 和物品名称搜索预览
  observeEvent(c(input$search_sku, input$search_name), {
    # 如果两个输入框都为空，则清空预览
    if (input$search_sku == "" && input$search_name == "") {
      output$item_preview <- renderUI({ NULL })
      return()  # 结束逻辑
    }
    
    req(input$search_sku != "" | input$search_name != "")  # 确保至少一个搜索条件不为空
    
    # 获取清理后的输入值
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 使用 unique_items_data() 进行过滤和统计
    result <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      group_by(SKU, ItemName, Maker, ItemImagePath) %>%
      summarise(
        DomesticStock = sum(Status == "国内入库", na.rm = TRUE),  # 国内库存
        InTransitStock = sum(Status == "国内出库", na.rm = TRUE),  # 在途库存
        UsStock = sum(Status == "美国入库", na.rm = TRUE),  # 美国库存
        .groups = "drop"
      )
    
    # 动态更新预览界面
    if (nrow(result) > 0) {
      output$item_preview <- renderUI({
        div(
          style = "max-height: 320px; overflow-y: auto; padding: 10px; border: 1px solid #e0e0e0; border-radius: 8px; background-color: #f9f9f9;",
          lapply(1:nrow(result), function(i) {
            item <- result[i, ]
            img_path <- ifelse(
              is.na(item$ItemImagePath),
              placeholder_150px_path,  # 占位符路径
              paste0(host_url, "/images/", basename(item$ItemImagePath))  # 构建完整路径
            )
            div(
              style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #ccc;",
              tags$img(src = img_path, height = "150px", style = "display: block; margin: auto;"),
              tags$h5(item$ItemName, style = "text-align: center; margin-top: 10px;"),
              tags$h5(item$Maker, style = "text-align: center; margin-top: 10px;"),
              tags$h5(item$SKU, style = "text-align: center; margin-top: 10px;"),
              div(
                style = "text-align: center; font-size: 12px;",
                tags$span(paste("国内库存:", item$DomesticStock), style = "margin-right: 10px;"),
                tags$span(paste("在途库存:", item$InTransitStock), style = "margin-right: 10px;"),
                tags$span(paste("美国库存:", item$UsStock))
              )
            )
          })
        )
      })
    } else {
      output$item_preview <- renderUI({
        div(tags$p("未找到匹配的物品", style = "color: red; text-align: center;"))
      })
    }
  })
  
  # 库存品请求按钮
  observeEvent(input$add_request, {
    req(input$request_quantity > 0)  # 确保输入合法
    
    # 获取用户输入
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 检索数据并插入到数据库
    filtered_data <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      distinct(SKU, Maker, ItemName, ItemImagePath)  # 去重
    
    tryCatch({
      # 主逻辑
      if (nrow(filtered_data) == 1) {
        request_id <- uuid::UUIDgenerate()
        
        item_image_path <- ifelse(is.na(filtered_data$ItemImagePath[1]), placeholder_150px_path, filtered_data$ItemImagePath[1])
        item_description <- ifelse(is.na(filtered_data$ItemName[1]), "未知", filtered_data$ItemName[1])
        
        dbExecute(con, 
                  "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
         VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')", 
                  params = list(request_id, filtered_data$SKU, filtered_data$Maker, item_image_path, item_description, 
                                input$request_quantity, format_remark(input$request_remark, system_type)))
        
        bind_buttons(request_id, requests_data, input, output, session, con)
        
        updateTextInput(session, "search_sku", value = "")
        updateTextInput(session, "search_name", value = "")
        updateNumericInput(session, "request_quantity", value = 1)
        updateTextAreaInput(session, "request_remark", value = "")
        
        showNotification("请求已成功创建", type = "message")
      } else if (nrow(filtered_data) > 1) {
        showNotification("搜索结果不唯一，请更精确地搜索 SKU 或物品名称", type = "error")
      } else {
        showNotification("未找到匹配的物品，请检查搜索条件", type = "error")
      }
      # 手动刷新
      refresh_board_incremental(dbGetQuery(con, "SELECT * FROM requests"), output, input)
    }, error = function(e) {
      # 捕获错误并打印详细信息
      showNotification(e, type = "error")
    })
  })
  
  # 初始化图片上传模块
  image_requests <- imageModuleServer("image_requests")
  
  # 新商品采购请求按钮
  observeEvent(input$submit_custom_request, {
    # 确保必要字段已填写
    req(input$custom_quantity > 0)
    
    # 获取用户输入
    custom_description <- trimws(input$custom_description)
    custom_quantity <- input$custom_quantity
    
    # 使用图片上传模块的返回数据
    custom_image_path <- process_image_upload(
      sku = "New-Request",  # 自定义物品没有 SKU，可以设置为固定值或动态生成
      file_data = image_requests$uploaded_file(),
      pasted_data = image_requests$pasted_file()
    )
    
    # 检查图片路径是否有效
    req(!is.null(custom_image_path) && !is.na(custom_image_path))
    
    # 生成唯一 RequestID
    request_id <- uuid::UUIDgenerate()
    
    # 将数据插入到数据库
    dbExecute(con, 
              "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
             VALUES (?, ?, '待定', ?, ?, ?, '待处理', ?, '新品')", 
              params = list(request_id, "New-Request", custom_image_path, custom_description, custom_quantity, format_remark(input$custom_remark, system_type)))
    
    bind_buttons(request_id, requests_data, input, output, session, con)  
    
    # 清空输入字段
    updateTextInput(session, "custom_description", value = "")
    updateNumericInput(session, "custom_quantity", value = 1)
    updateTextAreaInput(session, "custom_remark", value = "")
    image_requests$reset()
    showNotification("自定义请求已成功提交", type = "message")
    # 手动刷新
    refresh_board_incremental(dbGetQuery(con, "SELECT * FROM requests"), output, input)
  })
  
  # 点击请求图片看大图
  observeEvent(input$view_request_image, {
    showModal(modalDialog(
      title = "请求物品图片",
      div(
        style = "overflow: auto; max-height: 700px; text-align: center;",        
        tags$img(src = input$view_request_image, style = "max-width: 100%; height: auto; display: inline-block;")
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # 鼠标悬停请求图片显示库存分布
  output$colab_inventory_status_chart <- renderPlotly({
    req(input$hover_sku, input$hover_sku != "New-Request")  # 直接跳过 "New-Request"
    
    tryCatch({
      data <- unique_items_data()
      
      inventory_status_data <- data %>%
        filter(SKU == isolate(input$hover_sku)) %>%
        group_by(Status) %>%
        summarise(Count = n(), .groups = "drop")
      
      if (nrow(inventory_status_data) == 0) {
        return(NULL)
      }
      
      # 确保所有状态都存在，并填充 0
      inventory_status_data <- data.frame(Status = status_levels) %>%
        left_join(inventory_status_data, by = "Status") %>%
        mutate(Count = replace_na(Count, 0))
      
      # 过滤掉数量为 0 的状态
      inventory_status_data <- inventory_status_data %>% filter(Count > 0)
      
      # 重新匹配颜色：只取 **inventory_status_data$Status** 里有的颜色
      matched_colors <- status_colors[match(inventory_status_data$Status, status_levels)]
      
      plot_ly(
        data = inventory_status_data,
        labels = ~Status,
        values = ~Count,
        type = "pie",
        textinfo = "label+value",
        marker = list(colors = matched_colors)
      ) %>%
        layout(showlegend = FALSE, margin = list(l = 5, r = 5, t = 5, b = 5))
    }, error = function(e) {
      showNotification("库存状态图表生成错误", type = "error")
      return(NULL)
    })
  })
  
  outputOptions(output, "colab_inventory_status_chart", suspendWhenHidden = FALSE)
  
  # 自动转换 RequestType
  observe({
    invalidateLater(10000, session)
    
    dbWithTransaction(con, {
      # "安排" -> "完成"
      dbExecute(con, "
      UPDATE requests r
      JOIN (
        SELECT SKU, COUNT(*) AS procure_count
        FROM unique_items
        WHERE Status = '采购'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      SET r.RequestType = '完成', r.UpdatedAt = NOW()
      WHERE r.RequestType = '安排' AND u.procure_count >= r.Quantity
    ")
      
      # "完成" -> "出库"
      dbExecute(con, "
      UPDATE requests r
      JOIN (
        SELECT SKU, COUNT(*) AS domestic_count
        FROM unique_items
        WHERE Status = '国内入库'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      SET r.RequestType = '出库', r.RequestStatus = '已完成', r.UpdatedAt = NOW()
      WHERE r.RequestType = '完成' AND u.domestic_count >= r.Quantity
    ")
      
      # "出库" -> 删除
      dbExecute(con, "
      DELETE r FROM requests r
      JOIN (
        SELECT SKU, COUNT(*) AS transit_count
        FROM unique_items
        WHERE Status = '国内出库'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      WHERE r.RequestType = '出库' AND u.transit_count >= r.Quantity
    ")
      
      # 更新 requests_data
      requests_data(dbGetQuery(con, "SELECT * FROM requests"))
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################

  # 记录当前视图模式，初始为表格模式
  view_mode <- reactiveVal("table_mode")
  
  # 视图模式状态
  observeEvent(input$toggle_view, {
    # 切换视图
    shinyjs::toggle(id = "table_mode")
    shinyjs::toggle(id = "image_mode")
    
    # 根据当前模式更新 view_mode 变量
    if (view_mode() == "table_mode") {
      view_mode("image_mode")
      updateActionButton(session, "toggle_view", label = "切换至：图表模式")
    } else {
      view_mode("table_mode")
      updateActionButton(session, "toggle_view", label = "切换至：大图模式")
    }
  })
  
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
  
  # SKU 清除
  observeEvent(input$clear_inbound_sku, {
    updateTextInput(session, "inbound_sku", value = "")
  })
  
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
      host_url = host_url,
      image_mode = TRUE
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound) {
      req(input$inbound_sku)
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "国内出库",
        update_status_value = "美国入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL,      
        con,                  
        input, output, session
      )
      
      if (!is.null(result)) {
        if (input$speak_item_name) {  # 只有勾选“念出商品名”才朗读
          js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', result$item_name)
          
          shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读
        } else {
          runjs("playInboundSuccessSound()")  # 播放成功音效
        }
      } else {
        runjs("playInboundErrorSound()")  # 播放失败音效
        updateTextInput(session, "inbound_sku", value = "")
        runjs("document.getElementById('inbound_sku').focus();")
        return()
      }
      
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
  
  # 确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    # 从输入中获取入库数量，确保为正整数
    inbound_quantity <- as.integer(input$inbound_quantity)
    if (is.na(inbound_quantity) || inbound_quantity <= 0) {
      showNotification("入库数量必须是一个正整数！", type = "error")
      runjs("playInboundErrorSound()")  # 播放失败音效
      return()
    }
    
    # 批量处理入库逻辑
    for (i in seq_len(inbound_quantity)) {
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "国内出库",
        update_status_value = "美国入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = unique_items_data_refresh_trigger,      
        con,                  
        input, output, session
      )
      
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
            refresh_trigger = unique_items_data_refresh_trigger
          )
          showNotification("瑕疵品备注已成功添加！", type = "message")
        }, error = function(e) {
          showNotification(paste("添加备注时发生错误：", e$message), type = "error")
        })
      } else if (defective_item) {
        showNotification("无瑕疵品备注！", type = "warning")
      }
    }

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

  
  
  ################################################################
  ##                                                            ##
  ## 发货分页                                                   ##
  ##                                                            ##
  ################################################################
  
  zero_stock_items <- reactiveVal(list())  # 用于存储库存为零的物品
  
  # 抽取检测美国售罄物品并弹窗创建采购请求的公共函数
  check_us_stock_and_request_purchase <- function(order_items) {
    req(order_items, nrow(order_items) > 0)
    
    tryCatch({
      sku_list_str <- paste0("'", paste(unique(order_items$SKU), collapse = "','"), "'")
      
      latest_unique_items <- dbGetQuery(con, paste0("
        SELECT ui.SKU, inv.ItemName, inv.ItemImagePath, inv.Maker,
               SUM(CASE WHEN ui.Status = '美国入库' THEN 1 ELSE 0 END) AS UsStock,
               ROUND(AVG(ui.ProductCost), 2) AS AvgCost
        FROM unique_items AS ui
        JOIN inventory AS inv ON ui.SKU = inv.SKU
        WHERE ui.SKU IN (", sku_list_str, ")
        GROUP BY ui.SKU, inv.ItemName, inv.ItemImagePath, inv.Maker
        "))
      
      latest_unique_items$UsStock[is.na(latest_unique_items$UsStock)] <- 0  # 处理 NA
      
      zero_items <- latest_unique_items %>% filter(UsStock == 0)  # 筛选美国库存为 0 的物品
      
      if (nrow(zero_items) == 0) {
        showNotification("所有物品库存充足，无需采购！", type = "message")
        return(FALSE)
      }
      
      # **确保 zero_items 是 list**
      zero_stock_items(split(zero_items, seq(nrow(zero_items))))  # 转换为 list 并存入 reactiveVal
      
      existing_requests <- dbGetQuery(con, paste0("
        SELECT SKU, RequestType, Quantity FROM requests 
        WHERE SKU IN (", sku_list_str, ")
        "))
      
      # **弹出采购请求模态框**
      modal_content <- tagList(
        tags$div(
          style = "padding: 10px; background-color: #ffe6e6; border-radius: 8px; margin-bottom: 20px;",
          tags$h4("需要采购补货：", style = "color: red; margin-bottom: 15px;"),
          tags$div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
            lapply(zero_stock_items(), function(item) {  # 遍历 list 而非 data.frame
              existing_request <- existing_requests %>% filter(SKU == item$SKU)
              request_exists <- nrow(existing_request) > 0
              
              div(
                style = "background: white; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; display: flex; flex-direction: column; align-items: center;",
                tags$img(src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
                         style = "width: 150px; height: 150px; object-fit: cover; border-radius: 8px; margin-bottom: 10px;"),
                tags$p(tags$b("物品名："), item$ItemName, style = "margin: 5px 0;"),
                tags$p(tags$b("SKU："), item$SKU, style = "margin: 5px 0;"),
                tags$p(tags$b("供应商："), item$Maker, style = "margin: 5px 0;"),
                tags$p(tags$b("平均成本："), sprintf("￥%.2f", as.numeric(item$AvgCost)), style = "margin: 5px 0;"),
                
                if (request_exists) {
                  tagList(
                    tags$div(
                      style = "border: 2px solid #007BFF; border-radius: 8px; padding: 10px; background-color: #f0f8ff; margin: 0 auto 10px auto; width: 50%; text-align: center;",                        
                      tags$p(tags$b("采购请求已存在："), style = "color: blue; margin: 5px 0;"),
                      tags$p(paste0("当前请求状态：", existing_request$RequestType), style = "margin: 2px 0;"),
                      tags$p(paste0("当前请求数量：", existing_request$Quantity), style = "margin: 2px 0;")
                    ),
                    numericInput(paste0("purchase_qty_", item$SKU), "追加数量", value = 1, min = 1, width = "50%"),
                    textAreaInput(paste0("purchase_remark_input_", item$SKU), "留言（可选）", placeholder = "输入留言...", width = "50%", rows = 2),
                    actionButton(paste0("create_request_purchase_", item$SKU), "追加采购请求", class = "btn-primary", style = "margin-top: 10px; width: 50%;")
                  )
                } else {
                  tagList(
                    numericInput(paste0("purchase_qty_", item$SKU), "请求数量", value = 1, min = 1, width = "50%"),
                    textAreaInput(paste0("purchase_remark_input_", item$SKU), "留言（可选）", placeholder = "输入留言...", width = "50%", rows = 2),
                    actionButton(paste0("create_request_purchase_", item$SKU), "发出采购请求", class = "btn-primary", style = "margin-top: 10px; width: 50%;")
                  )
                }
              )
            })
          )
        )
      )
      
      showModal(modalDialog(
        title = "处理采购请求",
        div(style = "max-height: 650px; overflow-y: auto;", modal_content),
        easyClose = FALSE,
        footer = tagList(actionButton("complete_requests", "关闭", class = "btn-success"))
      ))
      
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("[Function]check_us_stock_and_request_purchase 发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  
    })
  }
  
  ###############################################
  
  # 页面切换时的聚焦
  observeEvent({
    req(input$inventory_us, input$shipping_tabs) # 确保两个输入都有效
    list(input$inventory_us, input$shipping_tabs)
  }, {
    if (input$inventory_us == "发货" && input$shipping_tabs == "国内售出发货") {
      runjs("document.getElementById('shipping_bill_number').focus();")
    }
    if (input$inventory_us == "发货" && input$shipping_tabs == "美国售出发货") {
      runjs("document.getElementById('us_shipping_bill_number').focus();")
    }
  })
  
  #############################################  数据准备
  
  # 当前订单ID
  current_order_id <- reactiveVal()
  
  # 装载匹配运单号的订单
  matching_orders <- reactive({
    # 如果运单号为空，返回空数据框
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      return(data.frame())  # 返回空数据框
    }
    
    data <- match_tracking_number(orders(), "UsTrackingNumber", input$shipping_bill_number)
    
    data %>% arrange(OrderStatus == "装箱")
  })
  
  # 自动装载订单ID：current_order_id
  observe({
    req(matching_orders())  # 确保 matching_orders 存在
    
    if (nrow(matching_orders()) > 0) {
      # 设置第一个订单的 OrderID 为当前订单 ID
      current_order_id(matching_orders()$OrderID[1])
    }
  })
  
  # 计算订单内物品数量和截取运单后四位
  order_info <- reactive({
    req(current_order_id())  # 确保订单 ID 存在
    order_id <- current_order_id()
    
    # 获取当前订单的所有物品
    items_in_order <- unique_items_data() %>% filter(OrderID == order_id)
    
    # 计算订单物品总数
    item_count <- nrow(items_in_order)
    
    # 查询 `orders` 表，获取该订单的运单号
    tracking_num <- orders() %>%
      filter(OrderID == order_id) %>%
      pull(UsTrackingNumber)  # 取运单号
    
    # 获取运单号的最后四位（如果运单号为空，则返回"无"）
    tracking_last4 <- if (!is.null(tracking_num) && nchar(tracking_num) >= 4) {
      substr(tracking_num, nchar(tracking_num) - 3, nchar(tracking_num))
    } else {
      "无"
    }
    
    # 返回包含物品数量和运单号后四位的列表
    list(
      item_count = item_count,
      tracking_last4 = tracking_last4
    )
  })
  
  # 装载当前订单物品信息
  order_items <- reactive({
    # 如果当前订单 ID 为空，返回空数据框
    if (is.null(current_order_id()) || trimws(current_order_id()) == "") {
      return(data.frame())  # 返回空数据框
    }
    # 筛选当前订单的物品
    unique_items_data() %>% filter(OrderID == current_order_id())
  })
  
  
  #############################################  渲染
  
  # 渲染订单信息卡片
  observe({
    req(input$shipping_bill_number, orders())
    
    if (nrow(matching_orders()) == 0) {
      renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      current_order_id(NULL)  # 清空当前订单 ID
      return()
    }
    
    renderOrderInfo(output, "order_info_card", matching_orders())
    
    all_packed <- all(matching_orders()$OrderStatus == "装箱")
    if (all_packed) {
      # showModal(modalDialog(
      #   title = "运单完成提示",
      #   "当前运单号所对应的所有订单已完成装箱操作！",
      #   easyClose = TRUE,
      #   footer = NULL  # 不需要关闭按钮
      # ))
      # 
      updateTextInput(session, "shipping_bill_number", value = "")
      runjs("document.getElementById('shipping_bill_number').focus();")
      # 
      # # 延迟 2 秒后自动关闭弹窗
      # shinyjs::delay(2000, removeModal())
      showNotification("当前运单号所对应的所有订单已完成装箱操作！", type = "message")
    }
  })
  
  # 渲染订单物品标题
  observe({
    req(input$shipping_bill_number)
    
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
    req(input$shipping_bill_number, order_items())
    
    if (nrow(order_items()) == 0) {
      renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    
    renderOrderItems(output, "shipping_order_items_cards", order_items(), con)
  })
  
  
  #############################################  逻辑
  
  # 延迟响应输入订单号，给手动输入留出空间
  debounced_order_id <- debounce(reactive(input$order_id_input), millis = 1000)  # 延迟 1000 毫秒
  
  # 输入订单号填写运单号
  observe({
    req(debounced_order_id())  # 确保输入框非空
    
    order_id <- trimws(debounced_order_id())
    
    result <- orders() %>%
      filter(OrderID == order_id) %>%
      select(UsTrackingNumber)
    
    # 更新运单号
    if (!is.null(result) && nrow(result) > 0) {
      updateTextInput(session, "shipping_bill_number", value = result$UsTrackingNumber[1])
      showNotification("运单号更新成功！", type = "message")
    } else {
      speak_text("未找到相关订单")
    }
    updateTextInput(session, "order_id_input", value = "")
  })
  
  # 清空运单号逻辑
  observeEvent(input$shipping_bill_number, {
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      output$dynamic_ship_button <- renderUI({ NULL })
      label_pdf_file_path(NULL)  # 清空运单文件路径
      
      shinyjs::delay(3000, {
        current_order_id(NULL)  # 清空当前订单 ID
        output$order_items_title <- renderUI({ NULL })  # 清空标题
        renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
        renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      })
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
  
  # 监视订单信息状态，提示操作，动态显示按钮
  observe({
    req(input$shipping_bill_number, unique_items_data(), matching_orders(), current_order_id())
    
    # 获取当前选中订单信息
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    # 确保选中订单存在
    req(nrow(current_order) > 0)
    
    # 存储运单文件路径
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(current_order$UsTrackingNumber, ".pdf")))
    
    # 获取当前订单内的物品
    current_items <- order_items()
    
    # 提示操作或警告
    if (current_order$OrderStatus != "装箱") {
      if (current_order$OrderStatus != "备货") {
        speak_text(paste0("当前订单状态为 '", current_order$OrderStatus, "' ，操作可能受限！请核对后继续。"))
      } else { #如果订单状态为备货
        # 如果订单内无物品
        if (nrow(current_items) == 0) {
          showModal(modalDialog(
            title = "订单内无物品",
            div(
              "当前订单内未检测到任何物品，请核对订单信息无误后手动发货",
              style = "font-size: 16px; margin-bottom: 10px;"
            ),
            footer = NULL,
            easyClose = TRUE
          ))
          shinyjs::delay(2000, removeModal())
        } else { # 如果订单内有物品
          runjs("document.getElementById('sku_input').focus();")
          showNotification(
            paste0("请为订单 ", current_order_id(), " 扫描或输入SKU条码！"),
            type = "message"
          )
          
          # 检查是否符合装箱条件
          if (all(current_items$Status == "美国发货")) {
            order_notes <- current_order$OrderNotes
            has_transfer_note <- grepl("【调货", order_notes, fixed = TRUE)
            
            if (has_transfer_note) {
              showModal(modalDialog(
                title = "调货物品",
                easyClose = FALSE,
                div(
                  style = "padding: 10px; font-size: 16px; color: #FF0000;",
                  paste0("订单 ", current_order_id(), " 含调货物品！运单尾号 "),
                  
                  # 运单号后四位，22px、加粗、红色
                  tags$span(
                    style = "font-size: 22px; font-weight: bold; color: red;",
                    order_info()$tracking_last4
                  ),
                  
                  "，共 ",
                  
                  # 订单物品总数，22px、加粗、蓝色
                  tags$span(
                    style = "font-size: 22px; font-weight: bold; color: blue;",
                    paste0(order_info()$item_count, " 件")
                  ),
                  
                  "。请核对物品备齐后手动发货。"
                ),
                footer = tagList(
                  modalButton("关闭")
                )
              ))
            } else {
              showModal(modalDialog(
                title = "确认装箱",
                easyClose = FALSE,
                div(
                  style = "padding: 10px; font-size: 16px;",
                  paste0("订单 ", current_order_id(), " 的所有物品已完成入箱扫描！运单尾号 "),
                  
                  # 运单号后四位，22px、加粗、红色
                  tags$span(
                    style = "font-size: 22px; font-weight: bold; color: red;",
                    order_info()$tracking_last4
                  ),
                  
                  "，共 ",
                  
                  # 订单物品总数，22px、加粗、蓝色
                  tags$span(
                    style = "font-size: 22px; font-weight: bold; color: blue;",
                    paste0(order_info()$item_count, " 件")
                  ),
                  
                  "。"
                ),
                footer = tagList(
                  actionButton("confirm_shipping_btn", "确认装箱", icon = icon("check"), class = "btn-primary")
                )
              ))
            }
          }
        }
      }
    }
    
    # 动态显示下载运单按钮
    output$dynamic_label_download_button <- renderUI({
      req(label_pdf_file_path())  # 确保 label_pdf_file_path 不为空
      
      label_text <- switch(
        current_order$LabelStatus,
        "无" = "无运单文件",
        "已上传" = "下载运单",
        "已打印" = "运单已打印",
        "无运单文件" # 默认值
      )
      
      if (current_order$LabelStatus == "无") {
        div(
          label_text,
          class = "btn btn-secondary",
          style = "background-color: grey; color: white; cursor: not-allowed; padding: 6px 12px; border-radius: 4px; display: inline-block; text-align: center;"
        )
      } else {
        downloadButton("download_shipping_label_pdf", label = label_text, class = "btn btn-primary")
      }
    })
    
    # 动态显示手动发货按钮
    output$dynamic_ship_button <- renderUI({
      if (current_order$OrderStatus == "装箱") {
        return(NULL)
      }
      
      order_notes <- current_order$OrderNotes
      has_transfer_note <- grepl("调货", order_notes, fixed = TRUE)
      
      if (nrow(current_items) == 0 || (all(current_items$Status == "美国发货") && has_transfer_note)) {
        return(actionButton("ship_order_btn", "手动发货", icon = icon("paper-plane"), class = "btn-success", style = "margin-top: 10px;", width = "100%"))
      }
      return(NULL)
    })
  })
  
  # SKU 输入逻辑
  observeEvent(input$sku_input, {
    req(input$shipping_bill_number, input$sku_input)
    
    sku <- trimws(input$sku_input)
    
    # 查找SKU对应的物品
    matching_item <- order_items() %>% filter(SKU == sku)
    
    # 如果未找到对应的 SKU
    if (nrow(matching_item) == 0) {
      speak_text("未找到商品，请检查输入的商品是否存在于本订单！")
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
    
    # 自动更新物品状态为“美国发货”
    tryCatch({
      js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', paste0(next_item$ProductCost, "元"))
      
      shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读物品价格
      
      update_status(
        con = con,
        unique_id = next_item$UniqueID,
        new_status = "美国发货",
        refresh_trigger = NULL
      )
      
      # 清空输入框
      updateTextInput(session, "sku_input", value = "")
      
    }, error = function(e) {
      showNotification(paste("更新状态时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 确认装箱逻辑
  observeEvent(input$confirm_shipping_btn, {
    tryCatch({
      # 更新订单状态为“装箱”
      update_order_status(order_id = current_order_id(), new_status = "装箱", refresh_trigger = orders_refresh_trigger, con = con)
      
      # **获取当前订单下的所有物品**
      order_items <- unique_items_data() %>% filter(OrderID == current_order_id())
      
      # **调用公共方法检测美国库存并弹出采购请求**
      has_request <- check_us_stock_and_request_purchase(order_items)
      
      # 如果库存充足没有发生采购请求，关闭弹窗
      if(!has_request) removeModal()
      
    }, error = function(e) {
      showNotification(paste("确认装箱发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  
  # 清空国内售出发货填写逻辑
  observeEvent(input$clear_shipping_bill_btn, {
    updateTextInput(session, "shipping_bill_number", value = "")
    label_pdf_file_path(NULL)  # 清空运单文件路径
    showNotification("运单号和 SKU 输入框已清空！", type = "message")
  })
  
  # 手动发货按钮功能
  observeEvent(input$ship_order_btn, {
    update_order_status(
      order_id = current_order_id(),
      new_status = "装箱",
      refresh_trigger = orders_refresh_trigger,
      con = con
    )
    
    # **获取当前订单下的所有物品**
    order_items <- unique_items_data() %>% filter(OrderID == current_order_id())
    
    # **调用公共方法检测美国库存并弹出采购请求**
    check_us_stock_and_request_purchase(order_items)
  })
  
  # 定义运单下载处理器
  output$download_shipping_label_pdf <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
      tracking_number <- tools::file_path_sans_ext(basename(label_pdf_file_path()))
      # 更新数据库中的 LabelStatus 为 "已打印"
      dbExecute(
        con,
        "UPDATE orders SET LabelStatus = '已打印' WHERE UsTrackingNumber = ?",
        params = list(tracking_number)
      )
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  )
  
  #####################
  ### 美国发货部分  ###
  #####################
  
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
    renderOrderItems(output, "shipping_order_items_cards", new_order_items(), con, deletable = TRUE)
  })
  
  observeEvent(input$us_shipping_sku_input, {
    req(input$us_shipping_sku_input)
    
    # 获取输入 SKU
    new_sku <- trimws(input$us_shipping_sku_input)
    
    # 校验 SKU 是否有效
    valid_sku <- stock_data() %>% filter(SKU == new_sku)
    if (nrow(valid_sku) == 0) {
      speak_text("输入的SKU不存在或状态不为美国入库")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 获取当前物品
    current_items <- new_order_items()
    if (!is.null(current_items)) {
      existing_count <- sum(current_items$SKU == new_sku)
      if (existing_count >= valid_sku$StockQuantity[1]) {
        speak_text("输入的SKU已达到库存上限")
        updateTextInput(session, "us_shipping_sku_input", value = "")
        return()
      }
    }
    
    # 筛选未被选择的物品
    available_items <- unique_items_data() %>%
      filter(SKU == new_sku & Status == "美国入库" & !(UniqueID %in% current_items$UniqueID))
    
    if (nrow(available_items) == 0) {
      speak_text("该SKU的库存已用尽")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 添加未被选择的第一件物品
    item_info <- available_items %>% slice(1)
    
    js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', paste0(item_info$ProductCost, "元"))
    
    shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读物品价格
    
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
      renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
      shinyjs::hide("us_shipping_sku_input")
    } else {
      # 延迟后执行的逻辑
      shinyjs::show("us_shipping_sku_input")
      runjs("document.getElementById('us_shipping_sku_input').focus();")
    }
  })
  
  # 美国售出发货按钮
  observeEvent(input$us_ship_order_btn, {
    req(new_order(), new_order_items())
    
    order <- new_order()
    items <- new_order_items()
    
    if (nrow(items) == 0) {
      speak_text("没有物品需要发货")
      return()
    }
    
    tryCatch({
      # 生成订单拼图
      combined_image_paths <- items$ItemImagePath[!is.na(items$ItemImagePath) & items$ItemImagePath != ""]
      order_image_path <- ifelse(length(combined_image_paths) == 0, "", generate_montage(combined_image_paths, paste0("/var/www/images/", order$OrderID, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")))
      
      # 插入订单到 `orders` 表
      dbExecute(con, "INSERT INTO orders (OrderID, UsTrackingNumber, CustomerName, CustomerNetName, Platform, OrderImagePath, OrderNotes, OrderStatus, created_at, updated_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                params = list(order$OrderID, order$UsTrackingNumber, order$CustomerName, order$CustomerNickname, order$Platform, order_image_path, order$OrderNotes, "装箱"))
      
      # 更新物品状态和订单号
      # lapply(seq_len(nrow(items)), function(i) {
      #   update_status(con = con, unique_id = items$UniqueID[i], new_status = "美国发货", refresh_trigger = NULL)
      #   update_order_id(con = con, unique_id = items$UniqueID[i], order_id = order$OrderID)
      # })
      # 更新物品状态和订单号
      update_status(con = con, unique_ids = items$UniqueID, new_status = "美国发货", refresh_trigger = NULL)
      update_order_id(con = con, unique_ids = items$UniqueID, order_id = order$OrderID)
      
      # **调用公共方法检测美国库存并弹出采购请求**
      check_us_stock_and_request_purchase(items)
      
      showNotification(paste0("订单已成功发货！订单号：", order$OrderID, "，共发货 ", nrow(items), " 件。"), type = "message")
    }, error = function(e) {
      showNotification(paste("美国售出发货失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
    
    # 延迟 2 秒清空输入框
    shinyjs::delay(2000, {
      updateTextInput(session, "us_shipping_bill_number", value = "")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      updateSelectInput(session, "us_shipping_platform", selected = "TikTok")
    })
    
    runjs("document.getElementById('us_shipping_bill_number').focus();")  # 聚焦输入框
    new_order_items(NULL)  # 清空物品列表
  })
  
  # 用于记录已绑定的请求按钮
  observed_request_buttons <- reactiveValues(registered = character())
  
  # 监听添加请求按钮
  observe({
    request_buttons <- grep("^create_request_purchase_", names(input), value = TRUE)
    new_buttons <- setdiff(request_buttons, observed_request_buttons$registered)
    
    lapply(new_buttons, function(button_id) {
      observeEvent(input[[button_id]], {
        sku <- sub("create_request_purchase_", "", button_id)  # 提取 SKU
        items <- zero_stock_items()  # 获取需要采购的物品
        item <- items[[which(sapply(items, function(x) x$SKU == sku))]]  # 找到匹配的物品
        
        qty <- input[[paste0("purchase_qty_", sku)]]
        formatted_remark <- format_remark(input[[paste0("purchase_remark_input_", sku)]], system_type)
        request_id <- uuid::UUIDgenerate()
        
        tryCatch({
          # **查询是否已有请求**
          existing_request <- dbGetQuery(con, "SELECT Quantity, Remarks FROM requests WHERE SKU = ?", params = list(sku))
          
          if (nrow(existing_request) > 0) {
            # **如果已有请求，追加数量和备注**
            new_qty <- existing_request$Quantity[1] + qty
            new_remark <- ifelse(is.na(existing_request$Remarks[1]) || existing_request$Remarks[1] == "",
                                 formatted_remark,
                                 paste(existing_request$Remarks[1], formatted_remark, sep = ";"))
            
            dbExecute(con, "UPDATE requests SET Quantity = ?, Remarks = ? WHERE SKU = ?",
                      params = list(new_qty, new_remark, sku))
            
            showNotification(paste0("采购请求追加成功，SKU：", sku, "，总数量：", new_qty), type = "message")
            
          } else {
            # **如果没有已有请求，新建请求**
            dbExecute(con,
                      "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
                     VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
                      params = list(request_id, sku, item$Maker, item$ItemImagePath, item$ItemName, qty, formatted_remark))
            
            showNotification(paste0("已发出采购请求，SKU：", sku, "，数量：", qty), type = "message")
          }
          
          # 更新 UI：按钮变绿，避免重复提交
          updateActionButton(session, inputId = button_id, label = HTML("<i class='fa fa-check'></i> 采购请求已发送"))
          runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", button_id))
          shinyjs::disable(button_id)
          
        }, error = function(e) {
          showNotification(paste("发出采购请求失败：", e$message), type = "error")
          runjs("playErrorSound()")  # 播放错误音效
        })
      }, ignoreInit = TRUE)  # 忽略初始绑定时的触发
    })
    
    # 更新已注册的按钮 ID
    observed_request_buttons$registered <- union(observed_request_buttons$registered, new_buttons)
  })
  
  # 监听 "完成请求" 按钮事件
  observeEvent(input$complete_requests, {
    zero_stock_items(list())  # 清空补货物品列表
    removeModal()  # 关闭模态框
    runjs("document.getElementById('us_shipping_bill_number').focus();")  # 聚焦运单号输入框
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
    updateTextAreaInput(session, "us_shipping_order_notes", value = "")
    new_order_items(NULL)  # 清空物品列表
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 退货分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 存储 UniqueID
  selected_return_id <- reactiveVal(NULL)  
  
  # 监听 SKU / 物品名输入框的变化，自动触发查询
  observeEvent(input$return_sku_itemname, {
    req(input$return_sku_itemname)
    
    # 查询物品信息
    search_query <- trimws(input$return_sku_itemname)
    return_item <- unique_items_data() %>%
      filter(Status == "交易完毕", SKU == search_query | grepl(search_query, ItemName, ignore.case = TRUE)) %>%
      arrange(UsShippingTime) %>%  # 按发货时间排序，优先显示最早发货的
      slice(1)  # 取匹配的第一条
    
    if (nrow(return_item) == 0) {
      showNotification("未找到可退货的物品！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      output$return_order_info <- renderUI({ NULL })
      output$return_item_info <- renderUI({ NULL })
      output$return_order_image <- renderUI({ NULL })
      output$return_item_image <- renderUI({ NULL })
      return()
    }
    
    # 物品对应订单信息
    return_order <- orders() %>%
      filter(OrderID == return_item$OrderID)
    
    # 渲染订单图片
    output$return_order_image <- renderUI({
      img_src <- ifelse(
        is.na(return_order$OrderImagePath) | return_order$OrderImagePath == "",
        placeholder_300px_path,  # 默认占位图片
        paste0(host_url, "/images/", basename(return_order$OrderImagePath))
      )
      tags$img(src = img_src, height = "300px", style = "border-radius: 8px; border: 1px solid #ddd;")
    })
    
    # 渲染订单信息
    output$return_order_info <- renderUI({
      if (nrow(return_order) == 0) {
        return(tags$p("该物品未关联任何订单", style = "color: red; font-size: 16px;"))
      }
      
      div(style = "font-size: 18px; line-height: 1.8;",
        tags$p(tags$b("订单号："), return_order$OrderID),
        tags$p(tags$b("客户："), return_order$CustomerName),
        tags$p(tags$b("平台："), return_order$Platform),
        tags$p(tags$b("订单状态："), return_order$OrderStatus)
      )
    })
    
    # 渲染物品图片
    output$return_item_image <- renderUI({
      img_src <- ifelse(
        is.na(return_item$ItemImagePath) | return_item$ItemImagePath == "",
        placeholder_300px_path,  # 默认占位图片
        paste0(host_url, "/images/", basename(return_item$ItemImagePath))
      )
      tags$img(src = img_src, height = "300px", style = "border-radius: 8px; border: 1px solid #ddd;")
    })
    
    # 渲染物品信息
    output$return_item_info <- renderUI({
      div(style = "font-size: 18px; line-height: 1.8;",
        tags$p(tags$b("SKU："), return_item$SKU),
        tags$p(tags$b("物品名称："), return_item$ItemName),
        tags$p(tags$b("当前状态："), return_item$Status),
        tags$p(tags$b("美国发货日期："), return_item$UsShippingTime)
      )
    })
    
    # 存储选中的物品 ID
    selected_return_id(return_item$UniqueID)
  })
  
  observeEvent(input$confirm_return_btn, {
    req(selected_return_id())
    
    tryCatch({
      dbExecute(con, "
      UPDATE unique_items 
      SET OrderID = NULL, UsShippingTime = NULL, Status = '美国入库'
      WHERE UniqueID = ?", params = list(selected_return_id()))
      
      showNotification("退货操作成功，物品状态已更新为 '美国入库'！", type = "message")
      
      # 刷新 UI
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      output$return_order_info <- renderUI({ NULL })
      output$return_item_info <- renderUI({ NULL })
      output$return_order_image <- renderUI({ NULL })
      output$return_item_image <- renderUI({ NULL })
      updateTextInput(session, "return_sku_itemname", value = "")
      runjs("document.getElementById('return_sku_itemname').focus();")
    }, error = function(e) {
      showNotification(paste("退货失败:", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  
  
  ################################################################
  ##                                                            ##
  ## 订单管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 订单关联物品容器
  associated_items <- reactiveVal()
  
  # 商品名自动联想
  autocompleteInputServer("sold", get_suggestions = item_names)  # 返回商品名列表
  
  # 手动刷新订单表
  observeEvent(input$refresh_orders, {
    orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
    showNotification("订单数据已刷新！", type = "message")
  })
  
  # 根据订单种类筛选的“订单查询”动态分页标题
  observe({
    order_status <- input$filter_order_status
    new_tab_title <- ifelse(is.null(order_status) || order_status == "", "订单查询", paste0("订单查询（", order_status, "）"))
    
    # 只更新 UI 显示的标题，不改变 Tab ID
    output$dynamic_order_tab_title <- renderText({ new_tab_title })
  })
  
  # 监听订单选择事件
  observeEvent(selected_order_row(), {
    selected_row <- selected_order_row()
    
    # 如果用户选择了订单，获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    customer_name <- selected_order$CustomerName
    order_status <- selected_order$OrderStatus
    
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(selected_order$UsTrackingNumber, ".pdf")))
    
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
        
        # 右侧按钮
        if (order_status == "调货") {
          actionButton("complete_transfer", "已完成调货", class = "btn-success",
                       style = "margin-left: auto; font-size: 14px; padding: 5px 10px;")
        },
        
        if (order_status == "调货") {
          tags$input(id = "transfer_notes", type = "text", placeholder = "添加调货备注（可选）",
                     style = "margin-left: 5px; margin-right: 5px; font-size: 14px; padding: 5px 10px; width: 200px;")
        },
        
        if (selected_order$LabelStatus != "无") {
          downloadButton("download_shipping_label_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                         style = "height: 34px; font-size: 14px; padding: 5px 10px;")
        }
      )
    })
    
    # 更新关联物品数据
    associated_items <- associated_items(unique_items_data() %>% filter(OrderID == order_id))
  })
  
  observeEvent(input$complete_transfer, {
    req(selected_order_row())
    
    # 获取选中订单
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    existing_notes <- selected_order$OrderNotes %||% ""  # 若为空，则默认空字符串
    new_notes_input <- input$transfer_notes %||% ""
    
    if (new_notes_input != "") new_notes_input <- paste0(" 圳备注：", new_notes_input)
    
    # 在 R 中拼接备注内容
    new_notes <- paste(existing_notes, sprintf("【调货完成 %s】", format(Sys.Date(), "%Y-%m-%d")), new_notes_input)
    
    update_order_status(order_id = order_id, 
                        new_status = "备货", 
                        updated_notes = new_notes, 
                        refresh_trigger = orders_refresh_trigger,
                        con = con)
  })
  
  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), con, deletable = FALSE)
  })
  
  # 清空筛选条件逻辑
  observeEvent(input$reset_filter_btn, {
    tryCatch({
      # 重置所有输入框和选择框
      updateTextInput(session, "filter_combined", value = "")  # 重置合并的搜索框
      updateSelectInput(session, "filter_platform", selected = "")  # 重置电商平台选择
      updateSelectInput(session, "filter_order_status", selected = "")  # 重置订单状态选择
      updateDateRangeInput(session, "filter_order_date", 
                           start = Sys.Date() - 90, 
                           end = Sys.Date() + 1)  # 重置日期范围到默认值
      
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
      associated_items <- unique_items_data() %>% filter(OrderID == order_id)
      
      if (nrow(associated_items) > 0) {
        # 遍历关联物品进行逆向操作
        lapply(1:nrow(associated_items), function(i) {
          item <- associated_items[i, ]
          
          # 查询物品的原始状态
          original_state <- dbGetQuery(con, paste0(
            "SELECT * FROM item_status_history WHERE UniqueID = '", item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
          ))
          
          if (nrow(original_state) > 0) {
            # 恢复物品状态
            update_status(
              con = con,
              unique_id = item$UniqueID,
              new_status = original_state$previous_status,
              clear_status_timestamp = item$Status
            )
            
            # 清空物品的 OrderID
            update_order_id(
              con = con,
              unique_id = item$UniqueID,
              order_id = NULL  # 清空订单号
            )
          } else {
            showNotification(paste0("物品 ", item$UniqueID, " 无状态历史记录，无法恢复。"), type = "warning")
            runjs("playWarningSound()")  # 播放警告音效
          }
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
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 清空关联物品表
      output$associated_items_table <- renderDT({ NULL })
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 订单合并
  observeEvent(input$merge_order_btn, {
    tryCatch({
      # 获取用户选中的订单号
      selected_order <- filtered_orders()[selected_order_row(), ]
      selected_order_id <- selected_order$OrderID
      
      if (is.null(selected_order_id) || length(selected_order_id) != 1) {
        showNotification("请选择一个订单进行合并！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 判断选中的订单是否包含 '@'，如果没有 '@'，则其本身就是主单
      main_order_id <- ifelse(grepl("@", selected_order_id), sub("@.*", "", selected_order_id), selected_order_id)
      
      # 获取可能的子单，包括主单本身和所有 `@` 子单
      possible_sub_orders <- orders() %>%
        filter(grepl(paste0("^", main_order_id, "(@\\d+)?$"), OrderID))
      
      # 如果只找到 **1 个** 订单，且它本身就是主单（无 `@`），则不能合并
      if (nrow(possible_sub_orders) == 1 && !grepl("@", selected_order_id)) {
        showNotification("当前订单未找到可合并的子单！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 获取所有子单的订单状态、运单号和平台信息
      order_statuses <- unique(possible_sub_orders$OrderStatus)
      tracking_numbers <- unique(possible_sub_orders$UsTrackingNumber)
      platforms <- unique(possible_sub_orders$Platform)
      
      # 检查订单状态、运单号和平台是否满足合并条件
      if (!all(order_statuses == "备货") || length(tracking_numbers) > 1 || length(platforms) > 1) {
        showNotification("子单的订单状态必须全部为 '备货'，运单号和平台必须一致才可合并！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 获取子单的所有物品
      sub_items <- unique_items_data() %>%
        filter(OrderID %in% possible_sub_orders$OrderID)
      
      # 处理子单物品图片路径拼接
      image_paths <- unique(sub_items$ItemImagePath[!is.na(sub_items$ItemImagePath)])
      merged_image_path <- if (length(image_paths) > 0) {
        montage_path <- paste0("/var/www/images/", main_order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        generate_montage(image_paths, montage_path)
      } else {
        NA
      }
      
      # 获取最早的 `created_at` 时间
      earliest_created_at <- min(possible_sub_orders$created_at, na.rm = TRUE)
      
      # **先删除所有子单（包括可能存在的主单）**
      dbExecute(con, sprintf(
        "DELETE FROM orders WHERE OrderID IN (%s)",
        paste(shQuote(possible_sub_orders$OrderID), collapse = ", ")
      ))
      
      # **插入合并后的主订单**
      merged_order <- tibble(
        OrderID = main_order_id,
        Platform = platforms[1],
        UsTrackingNumber = tracking_numbers[1],
        CustomerName = ifelse(length(unique(possible_sub_orders$CustomerName)) > 0,
                              paste(unique(possible_sub_orders$CustomerName), collapse = ", "), NA),
        CustomerNetName = ifelse(length(unique(possible_sub_orders$CustomerNetName)) > 0,
                                 paste(unique(possible_sub_orders$CustomerNetName), collapse = ", "), NA),
        OrderImagePath = merged_image_path,  # 合并图片路径
        OrderNotes = ifelse(length(unique(possible_sub_orders$OrderNotes)) > 0,
                            paste(unique(possible_sub_orders$OrderNotes), collapse = " | "), NA),
        OrderStatus = "备货",
        created_at = earliest_created_at,  # 使用子单中最早的创建时间
        updated_at = Sys.time()
      )
      
      dbWriteTable(
        con, "orders", merged_order,
        append = TRUE, overwrite = FALSE
      )
      
      # 更新子单物品的订单号为主单号
      update_order_id(con, sub_items$UniqueID, main_order_id)
      
      showNotification(paste("订单合并成功！主单号为：", main_order_id, ", 共计", nrow(sub_items), "件物品"), type = "message")
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      
    }, error = function(e) {
      showNotification(paste("合并订单时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 定义运单下载处理器
  output$download_shipping_label_pdf_manage <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
      tracking_number <- tools::file_path_sans_ext(basename(label_pdf_file_path()))
      # 更新数据库中的 LabelStatus 为 "已打印"
      dbExecute(
        con,
        "UPDATE orders SET LabelStatus = '已打印' WHERE UsTrackingNumber = ?",
        params = list(tracking_number)
      )
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  )
  
  # 监听 "已经到到齐" 表格行的点击事件
  observeEvent(selected_orders_table_arrived_row(), {
    selected_row <- selected_orders_table_arrived_row() 
    req(selected_row) 
    
    tracking_number <- filtered_orders_arrived()[selected_row, "UsTrackingNumber"]
    
    # 如果运单号为空或缺失，显示提示信息
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("未找到运单号，请检查", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()  # 终止后续操作
    }
    
    updateTabsetPanel(session, "inventory_us", selected = "发货") # 跳转到“发货”页面
    
    updateTextInput(session, "shipping_bill_number", value = tracking_number)
  })
  
  # 监听 "没有到齐" 表格行的点击事件
  observeEvent(selected_orders_table_waiting_row(), {
    selected_row <- selected_orders_table_waiting_row() 
    req(selected_row)
    
    tracking_number <- filtered_orders_waiting()[selected_row, "UsTrackingNumber"]
    
    # 如果运单号为空或缺失，显示提示信息
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("未找到运单号，请检查", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()  # 终止后续操作
    }
    
    updateTabsetPanel(session, "inventory_us", selected = "发货") # 跳转到“发货”页面
    
    updateTextInput(session, "shipping_bill_number", value = tracking_number)
  })
  
  # order信息筛选清除
  observeEvent(input$clear_filter_combined, {
    updateTextInput(session, "filter_combined", value = "")
  })
  
  # 状态更新逻辑
  observeEvent(input$update_order_status_btn, {
    req(selected_order_row())  # 确保用户选择了一行订单
    req(input$update_order_status)  # 确保用户选择了新状态
    
    # 获取选中订单的行
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    new_status <- input$update_order_status
    
    # 处理备注输入，防止 NULL 赋值错误
    updated_notes <- NULL
    if (!is.null(input$update_order_notes) && trimws(input$update_order_notes) != "") {
      updated_notes <- input$update_order_notes
    }
    
    # 调用 update_order_status 函数
    update_order_status(
      order_id = order_id, 
      new_status = new_status, 
      updated_notes = updated_notes, 
      refresh_trigger = orders_refresh_trigger, 
      con = con
    )
    
    # 清空备注输入框
    updateTextAreaInput(session, "update_order_notes", value = "")
  })
  
  observe({
    req(selected_order_row())  # 确保有选中的订单
    
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    
    # 更新 UI 中的状态选择框
    updateSelectInput(session, "update_order_status", selected = selected_order$OrderStatus)
    
    # 预填充备注
    updateTextAreaInput(session, "update_order_notes", value = selected_order$OrderNotes %||% "")
  })
  
  
  ################################################################
  ##                                                            ##
  ## 物品管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "manage_filter",
    makers_items_map = makers_items_map
  )
  
  # 采购商品图片处理模块
  image_manage <- imageModuleServer("image_manage")
  
  # 处理更新图片
  observeEvent(input$update_image_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    if (length(selected_rows) != 1) {
      showNotification("请确保只选中一行！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    # 从选中的行获取 SKU
    selected_item <- filtered_unique_items_data_manage()[selected_rows, ]
    selected_sku <- selected_item$SKU
    
    if (is.null(selected_sku) || selected_sku == "") {
      showNotification("无法获取所选行的 SKU，请检查！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    # 检查 SKU 是否存在于库存表
    existing_inventory_items <- inventory()
    if (!selected_sku %in% existing_inventory_items$SKU) {
      showNotification("库存中无此 SKU 商品，无法更新图片！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
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
        
        # 更新inventory数据需要手动触发刷新
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
        
        # 显示成功通知
        showNotification(paste0("SKU ", selected_sku, " 的图片已成功更新！"), type = "message")
      }, error = function(e) {
        # 数据库操作失败时提示错误
        showNotification("图片路径更新失败，请重试！", type = "error")
        runjs("playErrorSound()")  # 播放错误音效
      })
    } else {
      # 未检测到有效图片数据
      showNotification("未检测到有效的图片数据，请上传或粘贴图片！", type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    }
    
    # 重置图片上传状态
    image_manage$reset()
  })
  
  # 处理更新物品信息
  observeEvent(input$update_info_btn, {
    # 获取所有选中行索引
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 验证是否有选中行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请至少选中一行进行更新！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    # 获取过滤后的数据
    selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
    
    # 验证用户输入的新数据
    new_product_cost <- input$update_product_cost
    new_shipping_cost <- input$update_shipping_cost
    new_purchase_date <- input$update_purchase_date
    
    if (is.null(new_product_cost) || new_product_cost < 0) {
      showNotification("请输入有效的单价！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    if (is.null(new_shipping_cost) || new_shipping_cost < 0) {
      showNotification("请输入有效的国内运费！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    if (is.null(new_purchase_date) || !lubridate::is.Date(as.Date(new_purchase_date))) {
      showNotification("请输入有效的采购日期！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    # 遍历选中行并更新数据库
    tryCatch({
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        
        # 更新数据库
        dbExecute(
          con,
          "UPDATE unique_items 
                 SET ProductCost = ?, DomesticShippingCost = ?, PurchaseTime = ? 
                 WHERE UniqueID = ?",
          params = list(new_product_cost, new_shipping_cost, as.Date(new_purchase_date), unique_id)
        )
      })
      
      # 显示成功通知
      showNotification(paste0("成功更新了 ", nrow(selected_items), " 项物品的信息！"), type = "message")
    }, error = function(e) {
      showNotification(paste("更新失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 点击填写物品信息
  observeEvent(unique_items_table_manage_selected_row(), {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 检查是否有选中行
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      # 获取最新点击的行索引
      latest_row <- tail(selected_rows, n = 1)
      
      # 获取过滤后的数据
      data <- filtered_unique_items_data_manage()
      
      # 确保数据框不为空且行索引有效
      if (!is.null(data) && nrow(data) >= latest_row) {
        selected_data <- data[latest_row, ]  # 提取最新点击的行数据
        
        # 更新输入框
        updateNumericInput(session, "update_product_cost", value = selected_data$ProductCost)
        updateNumericInput(session, "update_shipping_cost", value = selected_data$DomesticShippingCost)
        updateDateInput(session, "update_purchase_date", value = as.Date(selected_data$PurchaseTime))
        
      } else {
        showNotification("选中的行无效或数据为空！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
      }
    } else {
      showNotification("未选中任何行！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
    }
  })
  
  # 清空
  observeEvent(input$clear_info_btn, {
    # 清空单价和运费输入框
    updateNumericInput(session, "update_product_cost", value = "")
    updateNumericInput(session, "update_shipping_cost", value = "")
    updateDateInput(session, "update_purchase_date", value = Sys.Date())
    
    showNotification("商品信息已清空！", type = "message")
  })
  
  ###
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
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
      showNotification("没有选中任何物品！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        unique_id <- selected_items$UniqueID[i]
        sku <- selected_items$SKU[i]
        status <- selected_items$Status[i]  # 获取物品状态
        
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
              DELETE FROM unique_items
              WHERE UniqueID = ?", params = list(unique_id))
        
        # 删除 item_status_history 中对应的历史状态记录
        dbExecute(con, "
              DELETE FROM item_status_history
              WHERE UniqueID = ?", params = list(unique_id))
      }
      
      dbCommit(con) # 提交事务
      
      # 通知用户成功删除
      showNotification("物品及其历史状态记录删除成功！", type = "message")
      
      # 删除物品需要手动触发更新inventory
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
    
    # 关闭确认框
    removeModal()
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 瑕疵商品分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "defect_filter",
    makers_items_map = makers_items_map
  )
  
  # 处理登记为瑕疵品
  observeEvent(input$register_defective, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品的状态符合要求（Defect == "无瑕" 或 Defect == "修复"）
      invalid_items <- selected_data[!selected_data$Defect %in% c("无瑕", "修复"), ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘无瑕’或‘修复’状态的物品可以登记为瑕疵品！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为瑕疵
        update_status(con, unique_id, defect_status = "瑕疵", refresh_trigger = NULL)
        
        # 添加备注
        defect_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = defect_notes,
          status_label = "瑕疵",
          refresh_trigger = NULL
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为瑕疵品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 处理登记为修复品
  observeEvent(input$register_repair, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品都满足条件（Defect == "瑕疵"）
      invalid_items <- selected_data[selected_data$Defect != "瑕疵", ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘瑕疵’状态的物品可以登记为修复品！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为修复
        update_status(con, unique_id, defect_status = "修复", refresh_trigger = NULL)
        
        # 添加备注
        repair_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = repair_notes,
          status_label = "修复",
          refresh_trigger = NULL
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为修复品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
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
  
  ######################
  ### 国际运单登记分页
  ######################
  
  # 登记运单信息
  observeEvent(input$register_shipment_btn, {
    req(input$intl_tracking_number, input$intl_shipping_method, input$intl_total_shipping_cost)
    
    # 获取用户输入的值
    tracking_number <- trimws(input$intl_tracking_number)
    shipping_method <- input$intl_shipping_method
    total_cost <- as.numeric(input$intl_total_shipping_cost)
    
    tryCatch({
      # 更新或插入运单记录
      dbExecute(
        con,
        "INSERT INTO intl_shipments (TrackingNumber, ShippingMethod, TotalCost, Status)
       VALUES (?, ?, ?, '运单创建')
       ON DUPLICATE KEY UPDATE 
         ShippingMethod = VALUES(ShippingMethod), 
         TotalCost = VALUES(TotalCost),
         UpdatedAt = CURRENT_TIMESTAMP",
        params = list(tracking_number, shipping_method, total_cost)
      )
      
      # # 生成交易记录的备注
      # remarks <- paste0("[国际运费登记]", " 运单号：", tracking_number, " 运输方式：", shipping_method)
      # 
      # # 生成交易记录的 ID
      # transaction_id <- generate_transaction_id("一般户卡", total_cost, remarks, Sys.time())
      # 
      # # 插入交易记录到“一般户卡”
      # dbExecute(
      #   con,
      #   "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionTime) 
      #  VALUES (?, ?, ?, ?, ?)",
      #   params = list(
      #     transaction_id,
      #     "一般户卡", 
      #     -total_cost,  # 转出金额为负值
      #     remarks,
      #     Sys.time()
      #   )
      # )
      # 
      # showNotification("国际运单登记成功，相关费用已记录到'一般户卡（541）'！", type = "message")
      # 
      # # 重新计算所有balance记录
      # update_balance("一般户卡", con)
      
      shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
    }, error = function(e) {
      showNotification(paste("登记国际运单操作失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
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
      output$intl_status_display <- renderText({ "" })  # 清空状态显示
      return()
    }
    
    tryCatch({
      # 查询运单号对应的信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod, TotalCost, Status FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) > 0) {
        # 如果运单号存在，回填信息
        updateSelectInput(session, "intl_shipping_method", selected = shipment_info$ShippingMethod[1])
        updateNumericInput(session, "intl_total_shipping_cost", value = shipment_info$TotalCost[1])
        shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
        
        # 显示物流状态
        output$intl_status_display <- renderText({
          paste("物流状态:", shipment_info$Status[1])
        })
        
      } else {
        # 如果运单号不存在，清空相关字段并禁用按钮
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
        shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
        
        # 提示未找到状态
        output$intl_status_display <- renderText({
          "未找到对应的运单信息，可以登记新运单！"
        })
      }
    }, error = function(e) {
      # 遇到错误时禁用按钮并清空状态显示
      shinyjs::disable("link_tracking_btn")
      output$intl_status_display <- renderText({
        paste("查询失败：", e$message)
      })
      showNotification(paste("加载运单信息失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 货值汇总显示
  observeEvent(input$batch_value_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
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
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 确定运输方式
      shipping_method <- ifelse(nrow(shipping_method_info) > 0, shipping_method_info$ShippingMethod[1], "未知")
      
      # 计算总价值合计
      total_value_sum <- summary_info$TotalValue[1] + summary_info$TotalDomesticShipping[1] + summary_info$TotalIntlShipping[1]
      
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
      showNotification(paste("货值汇总操作失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 删除运单逻辑
  observeEvent(input$delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "warning")
      runjs("playWarningSound()")  # 播放警告音效
      return()
    }
    
    tryCatch({
      # 检查运单是否存在于 intl_shipments 表中
      shipment_exists <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS count FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (shipment_exists$count == 0) {
        showNotification("运单号不存在，无法删除！", type = "warning")
        runjs("playWarningSound()")  # 播放警告音效
        return()
      }
      
      # 如果运单存在，弹出确认对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #C70039;'>确认删除国际运单</strong>"),
        HTML(paste0(
          "<p>您确定要删除国际运单号 <strong>", tracking_number, "</strong> 吗？关联物品的国际运单信息也会被同时清空。此操作不可逆！</p>"
        )),
        easyClose = FALSE,
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_delete_shipment_btn", "确认删除", class = "btn-danger")
        )
      ))
    }, error = function(e) {
      showNotification(paste("删除运单发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  
  # 监听确认删除运单按钮的点击事件
  observeEvent(input$confirm_delete_shipment_btn, {
    tracking_number <- trimws(input$intl_tracking_number)
    
    tryCatch({
      # 开始事务
      dbBegin(con)
      
      # 清空 unique_items 表中与运单号相关的运费
      dbExecute(con, "UPDATE unique_items SET IntlShippingCost = 0.00 WHERE IntlTracking = ?", params = list(tracking_number))
      
      # 从 intl_shipments 表中删除对应的运单号 (unique_items表会同时触发运单删除操作)
      dbExecute(con, "DELETE FROM intl_shipments WHERE TrackingNumber = ?", params = list(tracking_number))
      
      # # 删除 transactions 表中与运单号相关的记录
      # dbExecute(con, "DELETE FROM transactions WHERE Remarks LIKE ?", params = list(paste0("%[国际运费登记] 运单号：", tracking_number, "%")))
  
      # 提示删除成功
      showNotification("运单与关联的物品信息已成功删除！", type = "message")
      
      # 重新计算所有balance记录
      update_balance("一般户卡", con)
      
      # 清空输入框和相关字段
      updateTextInput(session, "intl_tracking_number", value = "")
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      
      # 提交事务
      dbCommit(con)
    }, error = function(e) {
      # 捕获错误并提示用户，回滚事务
      dbRollback(con)
      showNotification(paste("删除国际运单失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
    
    # 禁用挂靠按钮
    shinyjs::disable("link_tracking_btn")
    
    # 关闭确认对话框
    removeModal()
  })
  
  # 清空填写按钮逻辑
  observeEvent(input$clean_shipment_btn, {
    # 清空输入字段
    updateTextInput(session, "intl_tracking_number", value = "")  # 清空国际运单号
    updateSelectInput(session, "intl_shipping_method", selected = "空运")  # 重置国际运输方式为默认值
    updateNumericInput(session, "intl_total_shipping_cost", value = 0)  # 重置国际物流总运费为 0
    output$intl_status_display <- renderText({ "" })  # 清空状态显示
    
    # 提示用户清空完成
    showNotification("填写内容已清空！", type = "message")
  })
  
  # 点击行自动填写运单号
  observeEvent(unique_items_table_logistics_selected_row(), {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      # 如果没有选中行，清空运单号输入框，并禁用挂靠按钮
      updateTextInput(session, "intl_tracking_number", value = "")
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      return()
    }
    
    tryCatch({
      # 获取选中行的数据
      selected_data <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 提取所有选中行的国际物流单号（IntlTracking）
      unique_tracking_numbers <- unique(selected_data$IntlTracking)
      
      # 检查选中的物品是否已经挂靠国际运单
      if (any(!is.na(selected_data$IntlTracking))) {
        # 如果所有物品都未挂靠国际运单
        if (length(unique_tracking_numbers) == 1 && !is.na(unique_tracking_numbers)) {
          # 如果只有一个唯一的物流单号，填写到输入框
          updateTextInput(session, "intl_tracking_number", value = unique_tracking_numbers)
          showNotification("已根据选中行填写运单号！", type = "message")
        } else {
          # 如果没有唯一物流单号，取最新点击的那个
          updateTextInput(session, "intl_tracking_number", value = selected_data$IntlTracking[nrow(selected_data)])
        }
        # 如果选中物品中存在已挂靠国际运单的物品
        shinyjs::disable("link_tracking_btn")  # 禁用按钮
        shinyjs::enable("unlink_tracking_btn")  # 启用按钮
      } else {
        # 如果所有物品都未挂靠国际运单
        shinyjs::enable("link_tracking_btn")  # 启用按钮
        shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      }
    }, error = function(e) {
      # 捕获错误并提示
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      showNotification(paste("国际运单自动填写失败：", e$message), type = "error")
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  
  
  ######################
  ### 挂靠管理分页
  ######################
  
  # 监听页面切换事件
  observeEvent(input$intl_shipment_tabs, {
    if (input$intl_shipment_tabs == "link_management") {
      tryCatch({
        # 查询数据库中状态为“运单新建”的最新运单
        latest_shipment <- dbGetQuery(
          con,
          "SELECT TrackingNumber
         FROM intl_shipments
         WHERE Status = '运单创建'
         ORDER BY CreatedAt DESC
         LIMIT 1"
        )
        
        if (nrow(latest_shipment) > 0) {
          # 填写到 intl_link_tracking_number
          updateTextInput(session, "intl_link_tracking_number", value = latest_shipment$TrackingNumber[1])
          showNotification("已自动填充最新的‘运单创建’状态的运单号！", type = "message")
        } else {
          # 未找到符合条件的运单
          updateTextInput(session, "intl_link_tracking_number", value = "")
          showNotification("未找到状态为‘运单创建’的运单！", type = "warning")
          runjs("playWarningSound()")  # 播放警告音效
        }
      }, error = function(e) {
        # 捕获错误并提示
        showNotification(paste("检查运单状态时发生错误：", e$message), type = "error")
        runjs("playErrorSound()")  # 播放错误音效
      })
    }
  })
  
  # 监听待挂靠运单号输入
  observeEvent(input$intl_link_tracking_number, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    if (is.null(tracking_number) || tracking_number == "") {
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      
      output$intl_link_display <- renderText({
        "请输入运单号以查看运单信息"
      })
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT Status, TotalCost, ShippingMethod, CreatedAt FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        shinyjs::disable("link_tracking_btn")  # 禁用按钮
        shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
        
        output$intl_link_display <- renderText({
          "未找到对应的运单信息，请检查"
        })
        return()
      }
      
      # 显示运单状态和运费
      output$intl_link_display <- renderUI({
        HTML(paste0(
          "物流状态:   ", shipment_info$Status[1], "<br>",
          "运输方式：  ", shipment_info$ShippingMethod[1], "<br>",
          "国际运费:   ￥", format(shipment_info$TotalCost[1], big.mark = ",", nsmall = 2), "<br>",
          "创建日期:   ", format(as.Date(shipment_info$CreatedAt[1]), "%Y-%m-%d")
        ))
      })
    }, error = function(e) {
      output$intl_link_display <- renderText({
        paste("查询运单信息失败：", e$message)
      })
      runjs("playErrorSound()")  # 播放错误音效
    })
  })
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要挂靠的物品行！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 更新挂靠信息
      dbBegin(con)
      for (i in seq_len(nrow(selected_items))) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlTracking = ? WHERE UniqueID = ?",
          params = list(tracking_number, selected_items$UniqueID[i])
        )
      }
      
      # 查询挂靠到该运单的所有物品
      related_items <- dbGetQuery(
        con,
        "SELECT UniqueID FROM unique_items WHERE IntlTracking = ?",
        params = list(tracking_number)
      )
      
      if (nrow(related_items) == 0) {
        showNotification("当前运单号没有关联的物品！", type = "warning")
        dbRollback(con)
        return()
      }
      
      # 计算平摊运费
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      total_cost <- as.numeric(shipment_info$TotalCost)
      per_item_cost <- total_cost / nrow(related_items)
      
      # 更新平摊运费
      dbExecute(
        con,
        "UPDATE unique_items SET IntlShippingCost = ? WHERE IntlTracking = ?",
        params = list(per_item_cost, tracking_number)
      )
      dbCommit(con)
      
      showNotification("运单号挂靠成功，平摊运费已更新！", type = "message")
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 解除运单号挂靠逻辑
  observeEvent(input$unlink_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    # 校验用户选择的物品行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要解除挂靠的物品行！", type = "error")
      return()
    }
    
    # 校验运单号
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        showNotification("未找到对应的运单信息，请检查输入的运单号！", type = "error")
        return()
      }
      
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      selected_tracking_numbers <- unique(na.omit(selected_items$IntlTracking))
      
      # 开启事务处理
      dbBegin(con)
      
      # 批量解除挂靠并清零运费
      dbExecute(
        con,
        "UPDATE unique_items 
         SET IntlTracking = NULL, IntlShippingCost = 0.00 
         WHERE UniqueID IN (?)",
        params = list(selected_items$UniqueID)
      )
      
      # 重新计算剩余挂靠物品的平摊运费
      dbExecute(
        con, "
        UPDATE unique_items ui
        JOIN (
          SELECT IntlTracking, TotalCost / COUNT(*) AS PerItemCost
          FROM unique_items
          JOIN intl_shipments ON unique_items.IntlTracking = intl_shipments.TrackingNumber
          WHERE IntlTracking IN (?)
          GROUP BY IntlTracking
        ) calc ON ui.IntlTracking = calc.IntlTracking
        SET ui.IntlShippingCost = calc.PerItemCost",
        
        params = list(selected_tracking_numbers)
      )
      
      # 提交事务
      dbCommit(con)
      
      showNotification("运单号已成功解除挂靠，相关物品的平摊运费已重新计算！", type = "message")
    }, error = function(e) {
      # 回滚事务
      dbRollback(con)
      showNotification(paste("解除挂靠失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 账务核对分页                                               ##
  ##                                                            ##
  ################################################################
  
  transactions_data <- reactive({
    # 从数据库读取 transactions 表
    dbReadTable(con, "transactions")
  })
  
  ### 公司债务
  
  # Reactive 计算公司债务总和
  company_liabilities_total <- reactive({
    initial_liabilities <- 45000  # 初始公司债务
    
    # 从 transactions_data 获取 TransactionType 为 "债务" 的总和
    debt_transactions <- transactions_data() %>%
      filter(TransactionType == "债务") %>%
      summarise(total_debt = sum(Amount, na.rm = TRUE)) %>%
      pull(total_debt)
    
    # 返回公司债务总和
    initial_liabilities + debt_transactions
  })
    # 显示公司债务
  output$company_liabilities <- renderText({
    sprintf("¥%.2f", company_liabilities_total())
  })
  
  
  ### 社保
  
  # Reactive 计算公司社保总和
  social_security_total <- reactive({
    initial_social_security <- 4618  # 初始社保金额
    
    # 从 transactions_data 获取 TransactionType 为 "社保" 的总和
    social_transactions <- transactions_data() %>%
      filter(TransactionType == "社保") %>%
      summarise(total_social = sum(Amount, na.rm = TRUE)) %>%
      pull(total_social)
    
    # 返回公司社保总和
    initial_social_security + social_transactions
  })
  
  # 显示公司社保
  output$social_security <- renderText({
    sprintf("¥%.2f", social_security_total())
  })
  
  
  ### 工资
  
  # Reactive 计算工资总支出（只计算 Amount < 0 的部分，并取绝对值）
  salary_total <- reactive({
    transactions_data() %>%
      filter(TransactionType == "工资", Amount < 0) %>%
      summarise(total_salary = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_salary)
  })
  
  # 显示工资总支出
  output$salary <- renderText({
    sprintf("¥%.2f", salary_total())
  })
  
  
  ### 现金流
  
  # 计算现金流
  cash_flow_total <- reactive({
    # 获取 transactions_data 中所有 Amount 的总和
    total_amount <- transactions_data() %>%
      summarise(total = sum(Amount, na.rm = TRUE)) %>%
      pull(total)
    
    # 获取公司债务和社保总额
    total_liabilities <- company_liabilities_total()
    total_social_security <- social_security_total()
    
    # 计算现金流
    cash_flow <- total_amount - total_liabilities - total_social_security
    
    return(cash_flow)
  })
  
  # 显示现金流
  output$cash_flow <- renderText({
    sprintf("¥%.2f", cash_flow_total())
  })
  
  
  ### 公司税费
  
  # 计算公司税费总支出
  company_tax_total <- reactive({
    transactions_data() %>%
      filter(TransactionType == "税费", Amount < 0) %>%
      summarise(total_tax = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_tax)
  })
  
  # 显示公司税费
  output$company_tax <- renderText({
    sprintf("¥%.2f", company_tax_total())
  })
  

  ### 公司杂费
  
  # 计算公司杂费总支出
  company_expenses_total <- reactive({
    transactions_data() %>%
      filter(TransactionType %in% c("杂费", "图解"), Amount < 0) %>%
      summarise(total_expenses = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_expenses)
  })
  
  # 显示公司杂费
  output$company_expenses <- renderText({
    sprintf("¥%.2f", company_expenses_total())
  })
  
  
  ### 投入总金额
  
  # 计算投入总金额
  total_investment_value <- reactive({
    initial_investment <- 82445.9  # 初始投入金额
    
    # 获取 transactions 表中 AccountType 为 "美元卡" 且 Amount > 0 的总和
    usd_card_transactions <- transactions_data() %>%
      filter(AccountType == "美元卡", Amount > 0) %>%
      summarise(total_investment = sum(Amount, na.rm = TRUE)) %>%
      pull(total_investment)
    
    # 计算最终投入总金额
    total_investment <- initial_investment + usd_card_transactions
    
    return(total_investment)
  })
  
  # 显示投入总金额
  output$total_investment <- renderText({
    sprintf("¥%.2f", total_investment_value())
  })
  
  
  ###
  
  # 计算实际总金额
  actual_total_value <- reactive({
    total_salary <- salary_total()  # 总工资
    total_cash_flow <- cash_flow_total()  # 现金流
    total_after_20241223 <- inventory_value_cost_data()$after$total_value + inventory_value_cost_data()$after$total_shipping  # 12月23日后货值（含运费）
    total_tax <- company_tax_total()  # 公司税费
    total_expenses <- company_expenses_total()  # 公司杂费
    
    actual_total <- total_salary + total_cash_flow + total_after_20241223 + total_tax + total_expenses
    
    return(actual_total)
  })
  
  # 显示实际总金额
  output$actual_total <- renderText({
    sprintf("¥%.2f", actual_total_value())
  })
  
  
  # 显示对账差额
  output$reconciliation_difference <- renderText({
    sprintf("¥%.2f", total_investment_value() - actual_total_value() )
  })
  
  
  ### 货值与运费
  
  # 计算货值与运费
  inventory_value_cost_data <- reactive({
    data <- unique_items_data()
    date_cutoff <- as.Date("2024-12-23")
    
    # 按时间分割数据
    before_20241223 <- data %>% filter(PurchaseTime <= date_cutoff)
    after_20241223 <- data %>% filter(PurchaseTime > date_cutoff)
    
    # 调用 process_data 处理数据
    before <- process_data(before_20241223)
    after <- process_data(after_20241223)
    
    # 汇总数据
    list(
      before = c(before, calculate_totals(before)),
      after = c(after, calculate_totals(after))
    )
  })
  
  # 显示12月23日前货值与运费统计数据
  output$before_20241223_total_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$total_value)
  })
  output$before_20241223_total_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$total_shipping)
  })
  output$before_20241223_domestic_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$domestic$value)
  })
  output$before_20241223_domestic_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$domestic$shipping)
  })
  output$before_20241223_logistics_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$logistics$value)
  })
  output$before_20241223_logistics_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$logistics$shipping)
  })
  output$before_20241223_us_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$us$value)
  })
  output$before_20241223_us_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$us$shipping)
  })
  output$before_20241223_sold_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$sold$value)
  })
  output$before_20241223_sold_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$sold$shipping)
  })
  
  # 显示12月23日后货值与运费统计数据
  output$after_20241223_total_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$total_value)
  })
  output$after_20241223_total_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$total_shipping)
  })
  output$after_20241223_domestic_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$domestic$value)
  })
  output$after_20241223_domestic_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$domestic$shipping)
  })
  output$after_20241223_logistics_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$logistics$value)
  })
  output$after_20241223_logistics_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$logistics$shipping)
  })
  output$after_20241223_us_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$us$value)
  })
  output$after_20241223_us_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$us$shipping)
  })
  output$after_20241223_sold_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$sold$value)
  })
  output$after_20241223_sold_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$sold$shipping)
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
      showNotification("库存表已刷新！", type = "message")
    }
  }, ignoreInit = TRUE)  # 忽略初始值
  
  # 物品表过滤模块
  itemFilterServer(
    id = "query_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听点击事件，弹出大图
  observeEvent(input$show_large_image, {
    req(input$show_large_image)  # 确保图片路径有效
    
    showModal(modalDialog(
      title = "物品图片预览",
      tags$div(
        style = "overflow: auto; max-height: 700px; text-align: center;",
        tags$img(
          src = input$show_large_image,  # 直接使用传入的图片路径
          style = "max-width: 100%; height: auto; display: inline-block; border: 1px solid #ddd; border-radius: 8px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ###
  
  # 右键点击选择商品
  query_soldout_selected_item_details <- reactiveVal()
  
  # 监听鼠标右键 selected_inventory_row，并获取用户点击的 SKU。
  observeEvent(input$selected_inventory_row, {
    req(input$selected_inventory_row)
    
    row_index <- as.numeric(input$selected_inventory_row)  # 获取用户点击的行索引
    selected_item <- filtered_inventory()[row_index, ]  # 获取选中的数据
    
    if (nrow(selected_item) > 0) {
      # 存储物品详情
      query_soldout_selected_item_details(list(
        sku = selected_item$SKU,
        name = selected_item$ItemName,
        image = ifelse(
          is.na(selected_item$ItemImagePath) || selected_item$ItemImagePath == "",
          placeholder_150px_path,
          paste0(host_url, "/images/", basename(selected_item$ItemImagePath))
        ),
        maker = selected_item$Maker,
        domestic_stock = selected_item$DomesticQuantity
      ))
    }
  })
  
  # 点击采购请求
  observeEvent(input$query_purchase_request, {
    req(query_soldout_selected_item_details())
    
    details <- query_soldout_selected_item_details()
    
    showModal(modalDialog(
      title = "创建采购请求",
      
      div(
        style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
        
        # 左侧：商品图片 + 详情
        div(
          style = "flex: 0 0 40%; text-align: center;",
          tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
          div(
            tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
            tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
            tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;")
          )
        ),
        
        # 右侧：采购数量 + 备注
        div(
          style = "flex: 0 0 50%;",
          numericInput("query_purchase_qty", "采购数量", value = 1, min = 1, width = "80%"),
          textAreaInput("query_purchase_remark", "备注", "", width = "80%", height = "80px")
        )
      ),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("query_confirm_purchase", "确认采购", class = "btn-primary")
      )
    ))
  })
  
  # 确认采购
  observeEvent(input$query_confirm_purchase, {
    req(query_soldout_selected_item_details(), input$query_purchase_qty)
    
    details <- query_soldout_selected_item_details()
    request_id <- uuid::UUIDgenerate()
    
    # 数据库操作：插入采购请求
    dbExecute(con, "
    INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
    VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
              params = list(
                request_id,
                details$sku,
                details$maker,
                details$image,
                details$name,
                input$query_purchase_qty,
                format_remark(input$query_purchase_remark, system_type) 
              )
    )
    
    bind_buttons(request_id, requests_data(), input, output, session, con)
    
    showNotification("采购请求已创建", type = "message")
    removeModal()  # 关闭模态框
  })
  
  # # 点击出库请求
  # observeEvent(input$query_outbound_request, {
  #   req(query_soldout_selected_item_details())
  #   
  #   details <- query_soldout_selected_item_details()
  #   
  #   showModal(modalDialog(
  #     title = "创建出库请求",
  #     
  #     div(
  #       style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
  #       
  #       # 左侧：商品图片 + 详情
  #       div(
  #         style = "flex: 0 0 40%; text-align: center;",
  #         tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
  #         div(
  #           tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
  #           tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
  #           tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;"),
  #           tags$p(
  #             paste("国内库存:", details$domestic_stock),
  #             style = paste("margin: 0;", ifelse(details$domestic_stock == 0, "color: #DC3545; font-weight: bold;", "color: #28A745;"))
  #           )
  #         )
  #       ),
  #       
  #       # 右侧：出库数量 + 备注
  #       div(
  #         style = "flex: 0 0 50%; display: flex; flex-direction: column; gap: 10px;",
  #         numericInput("query_outbound_qty", "出库数量", value = 1, min = 1, max = details$domestic_stock, width = "80%"),
  #         textAreaInput("query_outbound_remark", "备注", "", width = "80%", height = "80px")
  #       )
  #     ),
  #     
  #     footer = tagList(
  #       modalButton("取消"),
  #       actionButton("query_confirm_outbound", "确认出库", class = "btn-success")
  #     )
  #   ))
  # })
  # 
  # # 确认出库
  # observeEvent(input$query_confirm_outbound, {
  #   req(query_soldout_selected_item_details(), input$query_outbound_qty)
  #   
  #   details <- query_soldout_selected_item_details()
  #   request_id <- uuid::UUIDgenerate()
  #   
  #   # 如果用户输入的出库数量大于国内库存，禁止提交
  #   if (input$query_outbound_qty > details$domestic_stock) {
  #     showNotification("出库数量不能大于国内库存数！", type = "error")
  #     return()
  #   }
  #   
  #   # 数据库操作：插入出库请求
  #   dbExecute(con, "
  #   INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
  #   VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '出库')",
  #             params = list(
  #               request_id,
  #               details$sku,
  #               details$maker,
  #               details$image,
  #               details$name,
  #               input$query_outbound_qty,
  #               format_remark(input$query_outbound_remark, system_type)
  #             )
  #   )
  #   
  #   bind_buttons(request_id, requests_data(), input, output, session, con)
  #   
  #   showNotification("出库请求已创建", type = "message")
  #   removeModal()  # 关闭模态框
  # })
  
  ###
  
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
          placeholder_200px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )
        
        div(
          style = "display: flex; flex-direction: column; padding: 10px;",
          
          # 上部分：图片和基本信息
          div(
            style = "display: flex; align-items: flex-start; width: 100%;",
            
            # 图片区域（带点击事件）
            div(
              style = "flex: 1; text-align: center; padding-right: 10px;",
              tags$img(
                src = img_path, height = "200px",
                style = "border: 1px solid #ddd; border-radius: 8px; cursor: pointer;",
                onclick = sprintf("Shiny.setInputValue('show_large_image', '%s', {priority: 'event'})", img_path)
              )
            ),
            
            # 右侧：商品信息
            div(
              style = "flex: 2;",
              tags$table(
                style = "width: 100%; border-collapse: collapse; line-height: 2;",
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "商品名称："), 
                  tags$td(style = "word-break: break-word;", sku_data$ItemName[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "供应商："), 
                  tags$td(style = "word-break: break-word;", sku_data$Maker[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "分类："), 
                  tags$td(style = "word-break: break-word;", paste(sku_data$MajorType[1], "/", sku_data$MinorType[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均成本："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ProductCost[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均运费："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ShippingCost[1]))
                )
              )
            )
          ),
          
          # 底部：库存信息
          div(
            style = "width: 100%; margin-top: 10px; text-align: center; padding-top: 5px; border-top: 1px solid #ddd;",
            tags$span(
              style = "font-weight: bold;",
              "库存数："
            ),
            tags$span(
              HTML(sprintf(
                "国内：%d &emsp;|&emsp; 在途：%d &emsp;|&emsp; 美国：%d &emsp;|&emsp; 总计：%d",
                sku_data$DomesticQuantity[1], 
                sku_data$TransitQuantity[1], 
                sku_data$UsQuantity[1], 
                sku_data$Quantity[1]
              ))
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
  
  #################################################################
  
  # 开销统计
  expense_summary_data <- reactive({
    req(input$time_range)
    data <- unique_items_data()
    
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    
    time_sequence <- switch(input$precision,
                            "天" = seq.Date(from = start_date, to = end_date, by = "day"),
                            "周" = seq.Date(from = floor_date(start_date, "week"),
                                           to = floor_date(end_date, "week"), by = "week"),
                            "月" = seq.Date(from = floor_date(start_date, "month"),
                                           to = floor_date(end_date, "month"), by = "month"),
                            "年" = seq.Date(from = floor_date(start_date, "year"),
                                           to = floor_date(end_date, "year"), by = "year"))
    
    time_df <- data.frame(GroupDate = time_sequence)
    
    summarized_data <- data %>%
      filter(!is.na(PurchaseTime) & PurchaseTime >= start_date & PurchaseTime <= end_date) %>%
      mutate(
        GroupDate = case_when(
          input$precision == "天" ~ as.Date(PurchaseTime),
          input$precision == "周" ~ floor_date(as.Date(PurchaseTime), "week"),
          input$precision == "月" ~ floor_date(as.Date(PurchaseTime), "month"),
          input$precision == "年" ~ floor_date(as.Date(PurchaseTime), "year")
        )
      ) %>%
      group_by(GroupDate) %>%
      summarise(
        Cost_Domestic = round(sum(ProductCost + DomesticShippingCost, na.rm = TRUE), 2),
        ProductCost = round(sum(ProductCost, na.rm = TRUE), 2),
        DomesticShippingCost = round(sum(DomesticShippingCost, na.rm = TRUE), 2),
        IntlShippingCost = round(sum(IntlShippingCost, na.rm = TRUE), 2),
        TotalExpense = round(sum(ProductCost + DomesticShippingCost + IntlShippingCost, na.rm = TRUE), 2),
        AllPurchaseCheck = all(PurchaseCheck == 1, na.rm = TRUE), # 是否全部为1
        .groups = "drop"
      )
    
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(
        Cost_Domestic = 0,
        ProductCost = 0,
        DomesticShippingCost = 0,
        IntlShippingCost = 0,
        TotalExpense = 0,
        AllPurchaseCheck = FALSE # 默认设置为 FALSE
      ))
    
    complete_data
  })
  
  # 定义 reactiveVal 用于存储观察器状态
  is_observer_click_suspended <- reactiveVal(TRUE)
  
  # 存储选定的时间范围
  selected_range <- reactiveVal(NULL) # 存储时间范围
  
  # 开销柱状图
  output$expense_chart <- renderPlotly({
    req(expense_summary_data())
    data <- expense_summary_data()
    
    # 获取用户选择的 Y 轴变量及颜色
    y_var <- switch(input$expense_type,
                    "total" = "TotalExpense",
                    "cost" = "ProductCost",
                    "domestic_shipping" = "DomesticShippingCost",
                    "intl_shipping" = "IntlShippingCost",
                    "cost_domestic" = "Cost_Domestic")
    color <- switch(input$expense_type,
                    "total" = "#007BFF",
                    "cost" = "#4CAF50",
                    "domestic_shipping" = "#FF5733",
                    "intl_shipping" = "#FFC107",
                    "cost_domestic" = "#17A2B8")
    
    # 根据精度生成时间范围标签
    data <- data %>%
      mutate(
        GroupLabel = case_when(
          input$precision == "天" ~ format(GroupDate, "%Y-%m-%d"),
          input$precision == "周" ~ paste(
            format(floor_date(GroupDate, "week"), "%Y-%m-%d"),
            "\n至\n",
            format(ceiling_date(GroupDate, "week") - 1, "%Y-%m-%d")
          ),
          input$precision == "月" ~ format(GroupDate, "%Y-%m"),
          input$precision == "年" ~ format(GroupDate, "%Y")
        )
      )
    
    # 创建柱状图
    p <- plot_ly(
      data,
      x = ~GroupLabel,
      y = ~get(y_var),
      type = "bar",
      name = NULL,
      marker = list(color = color),
      text = ~round(get(y_var), 2),
      textposition = "outside",
      source = "expense_chart" # 确保 source 唯一
    ) %>%
      # 注册事件
      event_register("plotly_click") %>%
      event_register("plotly_selected") %>%
      # 显示圆底对勾
      add_trace(
        type = "scatter",
        mode = "markers+text", # 同时使用 markers 和 text 模式
        x = ~GroupLabel,
        y = ~get(y_var) + (max(data[[y_var]], na.rm = TRUE) * 0.15), # 在柱子顶部留出空间
        marker = list(
          size = 20, # 圆点的大小
          color = ~ifelse(AllPurchaseCheck, "#039e2a", "#D3D3D3"), # 根据状态设置深绿色或浅灰色
          line = list(width = 0) # 移除外边框
        ),
        text = ~ifelse(AllPurchaseCheck, "\u2714", "\u2714"), # 使用 Unicode 的白色勾
        textfont = list(
          size = 14, # 增大字体，增强可见度
          color = "white", # 勾的颜色为白色
          weight = "bold" # 加粗字体
        ),
        textposition = "middle center", # 勾的位置在圆点正中央
        showlegend = FALSE # 不显示图例
      ) %>%
      # 添加布局和其他设置
      layout(
        xaxis = list(
          title = "",
          tickvals = data$GroupLabel,
          tickangle = -45,
          tickfont = list(size = 12),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "开销（元）",
          tickfont = list(size = 12),
          showgrid = TRUE  # 保留网格线
        ),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        showlegend = FALSE,
        plot_bgcolor = "#F9F9F9",
        paper_bgcolor = "#FFFFFF"
      )
    
    # 激活观察器
    if (is_observer_click_suspended()) {
      observer_click$resume()
      is_observer_click_suspended(FALSE)
    }
    
    p
  })
  
  # 定义点击观察器，初始状态为 suspended = TRUE
  observer_click <- observeEvent(event_data("plotly_click", source = "expense_chart"), suspended = TRUE, {
    clicked_point <- event_data("plotly_click", source = "expense_chart")
    if (!is.null(clicked_point)) {
      precision <- input$precision # 当前精度（天、周、月、年）
      
      # 根据精度解析点击的时间点
      clicked_date <- switch(
        precision,
        "年" = as.Date(paste0(clicked_point$x, "-01-01")), # 对"年"进行特殊处理
        as.Date(clicked_point$x) # 其他情况直接转为日期
      )
      
      # 根据精度计算时间范围
      range <- switch(precision,
                      "天" = c(clicked_date, clicked_date),
                      "周" = c(floor_date(clicked_date, "week"), ceiling_date(clicked_date, "week") - 1),
                      "月" = c(floor_date(clicked_date, "month"), ceiling_date(clicked_date, "month") - 1),
                      "年" = c(floor_date(clicked_date, "year"), ceiling_date(clicked_date, "year") - 1)
      )
      
      # 调用 updateDateRangeInput 更新用户界面的时间范围选择
      updateDateRangeInput(
        session,
        inputId = "time_range",
        start = range[1],
        end = range[2]
      )
      
      selected_range(range)
    }
  })
  
  # 筛选物品详情数据
  filtered_items <- reactive({
    req(selected_range()) # 确保时间范围存在
    range <- selected_range()
    
    # 从物品数据中筛选出时间范围内的数据
    unique_items_data() %>%
      filter(PurchaseTime >= range[1] & PurchaseTime <= range[2]) %>%
      arrange(PurchaseTime) # 按采购时间升序排列
  })
  
  # 渲染筛选
  callModule(uniqueItemsTableServer, "expense_details_table",
             column_mapping = c(common_columns, list(
               DomesticShippingCost = "国内运费",
               IntlShippingCost = "国际运费",
               PurchaseTime = "采购时间",
               PurchaseCheck = "核对"
             )),
             data = filtered_items
  )
  
  # 总开销分布饼图
  output$pie_chart <- renderPlotly({
    data <- expense_summary_data()
    
    # 饼图数据：计算总开销分布
    total_product_cost <- sum(data$ProductCost, na.rm = TRUE)
    total_domestic_shipping_cost <- sum(data$DomesticShippingCost, na.rm = TRUE)
    total_intl_shipping_cost <- sum(data$IntlShippingCost, na.rm = TRUE)
    
    pie_data <- data.frame(
      Category = c("商品成本", "国内运费", "国际运费"),
      Value = c(total_product_cost, total_domestic_shipping_cost, total_intl_shipping_cost)
    )
    
    # 获取时间范围
    time_range <- paste(as.Date(input$time_range[1]), "至", as.Date(input$time_range[2]))
    
    # 绘制饼图
    plot_ly(
      pie_data,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      textinfo = "label+value",  # 显示标签和数值
      hoverinfo = "label+percent",  # 悬停显示类别和百分比
      insidetextorientation = "radial",
      marker = list(colors = c("#4CAF50", "#FF5733", "#FFC107"))
    ) %>%
      layout(
        annotations = list(
          x = 0.5, y = -0.2,  # 调整注释的位置
          text = paste("统计时间范围：", time_range),
          showarrow = FALSE,
          font = list(size = 12, color = "#666")
        ),
        showlegend = FALSE,  # 隐藏图例
        paper_bgcolor = "#F9F9F9",  # 背景颜色
        margin = list(l = 50, r = 30, t = 80, b = 50)  # 增加左右和底部边距
      )
  })
  
  # 重置时间范围
  observeEvent(input$reset_time_range, {
    # 重置时间范围到默认值（最近30天）
    default_start <- Sys.Date() - 30
    default_end <- Sys.Date()
    
    updateDateRangeInput(
      session,
      inputId = "time_range",
      start = default_start,
      end = default_end
    )
  })
  
  #################################################################
  
  # 库存总览数据统计
  overview_data <- reactive({
    process_data(unique_items_data())
  })

  # 输出卡片数据
  output$domestic_total_count <- renderText({ overview_data()$domestic$count })
  output$domestic_total_value <- renderText({ sprintf("¥%.2f", overview_data()$domestic$value) })
  output$domestic_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$domestic$shipping) })

  output$logistics_total_count <- renderText({ overview_data()$logistics$count })
  output$logistics_total_value <- renderText({ sprintf("¥%.2f", overview_data()$logistics$value) })
  output$logistics_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$logistics$shipping) })

  output$us_total_count <- renderText({ overview_data()$us$count })
  output$us_total_value <- renderText({ sprintf("¥%.2f", overview_data()$us$value) })
  output$us_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$us$shipping) })

  output$sold_total_count <- renderText({ overview_data()$sold$count })
  output$sold_total_count_with_shipping <- renderText({
    count <- overview_data()$sold$count
    us_shipping_count <- overview_data()$sold$us_shipping_count
    paste0(count, " (", us_shipping_count, ")")
  })
  output$sold_total_value <- renderText({ sprintf("¥%.2f", overview_data()$sold$value) })
  output$sold_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$sold$shipping) })

  # 状态流转桑基图
  output$status_sankey <- renderSankeyNetwork({
    # 获取物品状态历史数据
    history_data <- dbGetQuery(con, "SELECT * FROM item_status_history")
    
    filtered_data <- history_data %>%
      arrange(UniqueID, change_time) %>%
      # 应用过滤规则
      group_by(UniqueID) %>%
      mutate(
        to_remove = FALSE,
        to_remove = ifelse(previous_status == "采购" & !is.na(lead(previous_status)) & lead(previous_status) != "国内入库", TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内入库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("国内出库", "国内售出"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内售出" & !is.na(lead(previous_status)) & lead(previous_status) != "美国发货", TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内出库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("美国入库", "美国调货"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "美国入库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("美国调货", "美国发货"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "美国调货" & !is.na(lead(previous_status)) & lead(previous_status) != "美国发货", TRUE, to_remove)
      ) %>%
      filter(!to_remove) %>%
      select(-to_remove) %>%
      ungroup() %>%
      group_by(UniqueID, previous_status) %>%
      slice_min(previous_status_timestamp, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # 确保状态流转顺序正确
    links <- filtered_data %>%
      group_by(UniqueID) %>%
      arrange(change_time, .by_group = TRUE) %>%
      mutate(next_status = lead(previous_status)) %>%
      filter(!is.na(next_status)) %>%
      ungroup() %>%
      group_by(source = previous_status, target = next_status) %>%
      summarise(value = n(), .groups = "drop")
    
    links <- as.data.frame(links)
    
    # 定义状态颜色映射
    status_colors <- c(
      "采购" = "lightgray",
      "国内入库" = "#c7e89b",
      "国内售出" = "#9ca695",
      "国内出库" = "#46a80d",
      "美国入库" = "#6f52ff",
      "美国调货" = "#529aff",
      "美国发货" = "#faf0d4",
      "交易完毕" = "#f4c7fc"
    )
    
    # 定义节点
    nodes <- data.frame(name = unique(c(links$source, links$target)))
    
    # 映射 source 和 target 到节点索引
    links <- links %>%
      mutate(
        source = match(source, nodes$name) - 1,
        target = match(target, nodes$name) - 1
      )
    
    # 校验 links 和 nodes 是否有效
    if (nrow(links) == 0 || nrow(nodes) == 0) {
      showNotification("没有可用的状态流转数据，请检查数据源。", type = "error")
      return(NULL)
    }
    
    # 生成颜色映射 JS 代码
    color_js <- sprintf(
      "d3.scaleOrdinal().domain(%s).range(%s)",
      jsonlite::toJSON(names(status_colors), auto_unbox = TRUE),
      jsonlite::toJSON(status_colors, auto_unbox = TRUE)
    )
    
    # 渲染桑基图
    sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 14,
      nodeWidth = 40,
      nodePadding = 20,  # 增加节点间距
      iterations = 5,
      colourScale = color_js
    )
  })
  
  #################################################################
  
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
  
  # 监听用户点击图片列
  observeEvent(input$filtered_inventory_table_query_cell_clicked, {
    info <- input$filtered_inventory_table_query_cell_clicked
    
    # 检查是否点击了图片列（第三列）
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == 2) {  # 第三列在 R 中的索引是 2
        
        img_path <- as.character(filtered_inventory()[info$row, "ItemImagePath"])
        
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
      label = NULL,
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
    updateSelectizeInput(session, "download_item_name", choices = item_names, selected = "", server = TRUE)
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
    updateSelectizeInput(session, "download_item_name", choices = "", selected = "", server = TRUE)
    updateDateRangeInput(session, "download_date_range", start = Sys.Date() - 365, end = Sys.Date() + 1)
  })
  
  # 下载物品汇总表为 Excel
  output$download_summary_xlsx <- downloadHandler(
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
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning")
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品汇总表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品汇总表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message")
    }
  )
  
  
  # 下载物品明细表为 Excel
  output$download_details_xlsx <- downloadHandler(
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
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning")
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品明细表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品明细表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message")
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 管理员分页                                                 ##
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
                    choices = c('采购','国内入库','国内出库','国内售出','美国入库','美国发货','美国调货','交易完毕'), 
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
        
        div(
          class = "card shadow-sm",
          style = "padding: 15px; border: 1px solid #007BFF; border-radius: 8px; margin-top: 20px;",
          tags$h4("历史库存状态流转记录", style = "color: #007BFF; font-weight: bold; margin-bottom: 10px;"),
          textOutput("selected_item_unique_id"),  # 显示 UniqueID
          DTOutput("item_status_history_table")   # 渲染状态流转表
        )
      )
    } else {
      div(tags$p("请输入密码以访问管理员功能", style = "color: red; font-weight: bold; text-align: center;"))
    }
  })
  
  # 使用 uniqueItemsTableServer 渲染表格
  unique_items_table_admin_selected_row <- callModule(uniqueItemsTableServer, "admin_items_table", 
                                                      column_mapping = c(common_columns, list(
                                                        Defect = "瑕疵态",
                                                        urchaseTime = "采购日",
                                                        DomesticEntryTime = "入库日",
                                                        DomesticExitTime = "出库日",
                                                        DomesticSoldTime = "售出日",
                                                        UsEntryTime = "美入库日",
                                                        UsRelocationTime = "美调货日",
                                                        UsShippingTime = "美发货日",
                                                        OrderID = "订单号"
                                                      )), 
                                                      selection = "multiple",
                                                      option = modifyList(table_default_options, list(searching = TRUE)),
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
          refresh_trigger = NULL,
          update_timestamp = record_timestamp  # 使用用户选择的值
        )
      })
      
      # 通知成功并刷新数据
      showNotification("库存状态更新成功！", type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("库存状态更新失败：", e$message), type = "error")
    })
  })
  
  # 更新瑕疵状态按钮
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
          refresh_trigger = NULL
        )
      })
      
      # 通知成功并刷新数据
      showNotification("瑕疵品状态更新成功！", type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("瑕疵品状态更新失败：", e$message), type = "error")
    })
  })
  
  # 监听表格选中行，获取 UniqueID
  observeEvent(unique_items_table_admin_selected_row(), {
    selected_rows <- unique_items_table_admin_selected_row()
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      updateTextOutput(session, "selected_item_unique_id", value = "未选择")
      output$item_status_history_table <- renderDT({ data.frame() })  # 清空表格
      return()
    }
    
    selected_item <- unique_items_data()[selected_rows, ]
    unique_id <- selected_item$UniqueID[length(selected_rows)]
    
    # 显示 UniqueID
    output$selected_item_unique_id <- renderText({ unique_id })
    
    # 查询该物品的状态历史
    status_history <- dbGetQuery(con, 
                                 "SELECT previous_status AS 'Status', previous_status_timestamp AS 'Time' 
                                  FROM item_status_history 
                                  WHERE UniqueID = ? 
                                  ORDER BY previous_status_timestamp",
                                 params = list(unique_id))
    
    # 格式化时间列
    if (nrow(status_history) > 0) {
      status_history$Time <- format(as.POSIXct(status_history$Time, format = "%Y-%m-%dT%H:%M:%SZ"))
    }
    
    # 渲染状态历史表格
    output$item_status_history_table <- renderDT({
      datatable(status_history, rownames = FALSE, options = list(
        searching = FALSE, paging = FALSE, info = FALSE
      ))
    })
  })
  
  #########################################################################################################################
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}
