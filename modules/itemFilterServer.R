itemFilterServer <- function(id, makers_items_map) {
  moduleServer(id, function(input, output, session) {
    # 缓存哈希值
    makers_hash <- reactiveVal(NULL)
    filtered_item_names_hash <- reactiveVal(NULL)
    
    # 动态更新 makers 控件
    observe({
      req(makers_items_map())  # 确保数据已加载
      
      current_makers <- makers_items_map() %>% pull(Maker) %>% unique()
      new_hash <- digest::digest(current_makers)
      
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()  # 无需更新
      makers_hash(new_hash)
      
      updateSelectizeInput(
        session,
        inputId = "maker",
        choices = c("", current_makers),  # 更新 Maker 列表
        selected = "",
        server = TRUE
      )
    })
    
    # 动态过滤并更新 item names 控件
    observe({
      req(makers_items_map())  # 确保数据已加载
      
      selected_maker <- input$maker
      filtered_item_names <- if (!is.null(selected_maker) && selected_maker != "") {
        makers_items_map() %>%
          filter(Maker == selected_maker) %>%
          pull(ItemName) %>%
          unique() %>%
          sort()
      } else {
        makers_items_map() %>% pull(ItemName) %>% unique() %>% sort()  # 所有 ItemName
      }
      
      new_hash <- digest::digest(filtered_item_names)
      if (!is.null(filtered_item_names_hash()) && filtered_item_names_hash() == new_hash) return()
      filtered_item_names_hash(new_hash)
      
      updateSelectizeInput(
        session,
        inputId = "name",
        choices = c("", filtered_item_names),  # 更新筛选后的 ItemName
        selected = "",
        server = TRUE
      )
    })
    
    # 清空输入（按钮绑定逻辑）
    observeEvent(input$reset_btn, {
      resetFilters()  # 调用封装的 resetFilters 方法
    })
    
    # 清空输入的逻辑封装为 resetFilters 方法
    resetFilters <- function() {
      tryCatch({
        # 重置 makers 控件
        makers_choices <- makers_items_map() %>% pull(Maker) %>% unique()  # 获取 makers 列表
        updateSelectizeInput(
          session, 
          inputId = "maker", 
          choices = c("", makers_choices),  # 重新定义 makers choices
          selected = NULL, 
          server = TRUE
        )
        
        # 重置商品名称控件
        item_name_choices <- makers_items_map() %>% pull(ItemName) %>% unique() %>% sort()  # 获取所有 ItemName
        updateSelectizeInput(
          session, 
          inputId = "name", 
          choices = c("", item_name_choices),  # 重新定义 item name choices
          selected = NULL, 
          server = TRUE
        )
        
        # 重置日期选择器
        updateDateRangeInput(session, "purchase_date_range", start = Sys.Date() - 365, end = Sys.Date())
        
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    }
    
    # 返回 resetFilters 方法供外部调用
    return(list(resetFilters = resetFilters))
  })
}
