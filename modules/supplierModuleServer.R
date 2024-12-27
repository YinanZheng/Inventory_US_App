# 定义供应商模块
supplierModuleServer <- function(input, output, session, con, maker_data) {

  # 初始化供应商选择器
  observe({
    update_maker_choices(session, "new_maker", maker_data())
  })
  
  # 添加供应商弹窗
  observeEvent(input$add_supplier_btn, {
    showModal(modalDialog(
      title = "添加新供应商",
      textInput("new_supplier_name", "请输入新供应商名称"),
      uiOutput("matched_suppliers"),
      footer = tagList(
        modalButton("取消"),
        actionButton("submit_supplier", "提交", class = "btn-primary")
      )
    ))
  })
  
  # 动态匹配供应商名称
  observe({
    req(maker_data())
    output$matched_suppliers <- renderUI({
      current_name <- input$new_supplier_name
      
      if (is.null(current_name) || current_name == "") return(NULL)
      
      all_suppliers <- maker_data()$Maker
      matched <- all_suppliers[grepl(current_name, all_suppliers, ignore.case = TRUE)]
      
      if (length(matched) > 0) {
        tagList(
          h5("已存在的供应商如下，请勿重复加入:"),
          tags$ul(lapply(matched, tags$li))
        )
      } else {
        h5("该供应商可以加入！")
      }
    })
  })
  
  # 提交新供应商
  observeEvent(input$submit_supplier, {
    new_supplier <- input$new_supplier_name
    existing_suppliers <- maker_data()$Maker
    
    if (new_supplier %in% existing_suppliers) {
      showModal(modalDialog(
        title = "错误",
        paste0("供应商 '", new_supplier, "' 已经存在，请勿重复添加。"),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # 自动生成拼音
    pinyin_name <- remove_tone(stri_trans_general(new_supplier, "Latin"))
    
    if (new_supplier != "") {
      # 添加新供应商到 MySQL 数据库
      tryCatch({
        dbExecute(con, "INSERT INTO maker_data (Name, Pinyin) VALUES (?, ?)", 
                  params = list(new_supplier, pinyin_name))
        
        # 更新数据并刷新 UI
        new_data <- dbGetQuery(con, "SELECT Name AS Maker, Pinyin FROM maker_data ORDER BY Pinyin ASC")
        update_maker_choices(session, "new_maker", new_data)
        
        showNotification("新供应商添加成功！", type = "message")
        removeModal()
      }, error = function(e) {
        showModal(modalDialog(
          title = "错误",
          paste0("添加供应商失败：", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else {
      showModal(modalDialog(
        title = "错误",
        "供应商名称不能为空。",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
}