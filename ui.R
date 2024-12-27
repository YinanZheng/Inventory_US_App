# Define UI
ui <- navbarPage(
  title = "库存管理系统（国内端）",
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    shinyjs::useShinyjs(),  # 启用 shinyjs
    
    tags$head(
      tags$style(HTML("
      
      /* 默认显示导航栏标题 */
      .navbar-brand {
        display: inline-block !important;
      }
  
      /* 强制导航栏水平滚动，禁止换行 */
      .navbar-nav {
        display: flex !important; /* 使用 Flex 布局 */
        flex-wrap: nowrap !important; /* 禁止换行 */
        overflow-x: auto !important; /* 启用水平滚动 */
        white-space: nowrap !important; /* 确保内容不换行 */
      }
    
      /* 美化滚动条 */
      .navbar-nav::-webkit-scrollbar {
        height: 6px; /* 滚动条高度 */
      }
      .navbar-nav::-webkit-scrollbar-thumb {
        background: #007BFF; /* 滚动条颜色 */
        border-radius: 10px;
      }
    
      /* 禁止导航栏高度扩展 */
      .navbar {
        white-space: nowrap !important; /* 确保所有子元素在单行内 */
      }
    
     /* 当屏幕宽度小于 1380px 时，隐藏标题 */
      @media (max-width: 1380px) {
        .navbar-brand {
          display: none !important;
        }
      }
    
      /* 当屏幕宽度小于 768px 时，调整导航项的字体和间距 */
      @media (max-width: 768px) {
        .navbar-nav > li > a {
          font-size: 12px !important; /* 调整字体大小适配小屏幕 */
          padding: 6px 8px !important; /* 减少间距 */
        }
      }
    
    
      body {
        padding-top: 70px; /* 为导航栏腾出空间 */
      }
      
      /* Flexbox 容器 */
      .layout-container {
        display: flex; /* Flex 布局 */
        flex-wrap: nowrap; /* 禁止换行 */
        height: 100%; /* 满高布局 */
      }

      /* Sticky Sidebar */
      .sticky-sidebar {
        position: sticky; /* 保持固定 */
        top: 70px; /* 与导航栏对齐 */
        z-index: 900;
        width: 450px; /* 固定宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        flex-shrink: 0; /* 防止压缩 */
      }
    
      /* 主面板 */
      .main-panel {
        flex-grow: 1; /* 占据剩余空间 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
      }
      
      /* 自定义 selectize 样式 */
      .custom-selectize .selectize-input {
        font-size: 12px !important; /* 设置输入框字体大小 */
      }
      .custom-selectize .selectize-dropdown-content {
        font-size: 12px !important; /* 设置下拉菜单字体大小 */
      }
    ")),
      
      tags$script(HTML("
        $(document).on('paste', '[id$=\"paste_area\"]', function(event) {
          const items = (event.originalEvent.clipboardData || event.clipboardData).items;
          for (let i = 0; i < items.length; i++) {
            if (items[i].type.indexOf('image') !== -1) {
              const file = items[i].getAsFile();
              const reader = new FileReader();
    
              reader.onload = function(evt) {
                // 使用 currentTarget 确保获取的是父级元素的 id
                const inputId = event.currentTarget.id + '_pasted_image';
                Shiny.setInputValue(inputId, evt.target.result, {priority: 'event'});
              };
    
              reader.readAsDataURL(file);
              break;
            }
          }
        });")),
      
      tags$script('
        Shiny.addCustomMessageHandler("navigate", function(url) {
          window.location.href = url;
        });
      ')
      
    )
  ),
  
  tabPanel(
    "采购登记", icon = icon("shopping-cart"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        fluidRow(
          column(10, 
                 selectizeInput("new_maker", "供应商:", choices = NULL, width = "100%",
                                options = list(placeholder = '输入供应商名称（或拼音）进行搜索', maxOptions = 500))
          ),
          column(2, 
                 div(style = "display: flex; justify-content: flex-start; align-items: center; height: 100%;", 
                     actionButton("add_supplier_btn", label = NULL, icon = icon("plus"), 
                                  style = "font-size: 14px; width: 100%; height: 34px; padding: 0px; margin-top: 26px;")
                 )
          )
        ),
        
        typeModuleUI("type_module"),
        
        fluidRow(
          column(7, uiOutput("new_name_combo_box_ui")),  # 动态生成ComboBox
          
          column(5, dateInput(
            inputId = "purchase_date",
            label = "采购日期:",
            value = Sys.Date(),  # 默认日期为今天
            width = "100%"
          ))
        ),
        
        fluidRow(
          column(4, numericInput("new_quantity", "数量:", value = 0, min = 0, step = 1)),
          column(4, numericInput("new_product_cost", "单价:", value = 0, min = 0)),
          column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
        ),
        fluidRow(
          column(9,textInput("new_sku", "SKU(自动生成):", value = "", width = "100%")),
          column(3,actionButton("reset_btn", "清空", icon = icon("snowplow"), class = "btn-danger", 
                                style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;"))
        ),
        
        imageModuleUI("image_purchase"),
        
        fluidRow(
          column(12, style = "text-align: left;", actionButton("add_btn", "添加/更新采购货品信息", width = "100%", icon = icon("pen"), style = "background-color: #006400; color: white;")),
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;")
      ),
      
      div(
        class = "main-panel",
        div(
          div(
            tags$span(icon("shopping-cart"), style = "margin-right: 5px;"),  # 使用 span 包裹图标
            "采购箱", style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; padding: 10px; text-align: center; border-radius: 4px;"
          ),
          column(12, DTOutput("added_items_table")),
          
          div(
            style = "padding: 20px 0;",  # 添加上下20px的padding
            fluidRow(
              column(3, actionButton("delete_btn", "删除选中记录", icon = icon("trash"), class = "btn-danger")),
              column(6, div(
                textOutput("total_cost"),
                style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"
              )),
              column(3, div(
                style = "text-align: right;",
                actionButton("confirm_btn", "确认登记采购货品", icon = icon("check"), class = "btn-primary")
              ))
            )
          )
        ),
        
        tags$hr(style = "margin: 20px 0; border: 1px solid #ddd;"),  # 添加分隔线
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_purchase",
              uniqueItemsTableUI("unique_items_table_purchase")
            )
          )
        )
      )
    )
  ), # end of 采购登记 tab
  
  tabPanel(
    "入库", icon = icon("arrow-circle-down"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        style = "width: 400px;", # override 宽度
        
        itemFilterUI(id = "inbound_filter", border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(), # 分隔线
        
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #007BFF; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              ),
              
              # SKU 输入框
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "inbound_sku", 
                  label = NULL, 
                  placeholder = "请扫描或输入SKU",
                  width = "100%"
                )
              ),
              
              # 瑕疵品复选框
              div(
                style = "margin-bottom: 20px; display: flex; align-items: center;",
                tags$input(
                  type = "checkbox", 
                  id = "defective_item", 
                  style = "width: 20px; height: 20px; margin-right: 10px;"
                ),
                tags$label("瑕疵品", `for` = "defective_item", style = "font-size: 18px; font-weight: bold; color: #444;")
              ),
              
              div(
                id = "defective_notes_container",
                style = "display: none; margin-top: 10px;",
                textAreaInput(
                  inputId = "defective_notes",
                  label = "瑕疵品备注：",
                  placeholder = "请输入备注内容...",
                  width = "100%"
                )
              ),
              
              # 确认入库按钮
              actionButton(
                "confirm_inbound_btn", 
                "确认入库", 
                icon = icon("check"), 
                class = "btn-primary", 
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        ),
        
        tags$hr(), # 分隔线
        
        fluidRow( 
          # 条形码生成下载按钮
          column(12,              
                 tags$div(
                   class = "card",
                   style = "padding: 15px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                   
                   # 卡片标题
                   div(
                     style = "margin-bottom: 10px; padding-bottom: 8px;",
                     tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
                   ),
                   
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     actionButton("export_select_btn", "生成选中商品条形码", icon = icon("barcode"), class = "btn-info"),
                     downloadButton("download_select_pdf", "下载条形码", class = "btn-info")
                   )
                 )
          )
        )
      ),
      
      div(
        class = "main-panel",
        
        div(
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("inbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_inbound",
              uniqueItemsTableUI("unique_items_table_inbound")
            )
          )
        )
      )
    )
  ), # end of 入库 tab
  
  tabPanel(
    "出库", icon = icon("arrow-circle-up"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        style = "width: 400px;", # override 宽度
        
        itemFilterUI(id = "outbound_filter", border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(), # 分隔线
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("出库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("outbound_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          
          tags$div(
            class = "card",
            style = "padding: 15px; border: 2px solid #007BFF; border-radius: 8px; background-color: #f9f9f9; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
            tags$h4("选择国际运输方式:", style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;"),
            radioButtons(
              inputId = "outbound_shipping_method",
              label = NULL, # 将标签移到卡片标题
              choices = list("空运" = "空运", "海运" = "海运"),
              selected = "空运",  # 默认选择空运
              inline = TRUE       # 设置为横向排布
            )
          ),
          
          actionButton(
            "confirm_outbound_btn", 
            "确认出库", 
            icon = icon("check"), 
            class = "btn-primary", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      
      div(
        class = "main-panel",
        div(
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("outbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_outbound",
              uniqueItemsTableUI("unique_items_table_outbound")
            )
          )
        )
      )
    )
  ), # end of 出库 tab
  
  
  tabPanel(
    "售出", icon = icon("dollar-sign"),
    div(
      class = "layout-container",  # Flexbox 容器
      
      # 左侧订单信息录入
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        style = "width: 400px;", # override 宽度
        
        itemFilterUI(id = "sold_filter", border_color = "#28A745", text_color = "#28A745"),
        
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          # 订单录入表单标题
          tags$h4("订单登记", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          fluidRow(
            column(
              7,  # 占页面宽度的 6/12，即 50%
              textInput("order_id", "订单号", placeholder = "请输入订单号", width = "100%")
            ),
            column(
              5,  # 占页面宽度的 6/12，即 50%
              selectInput(
                inputId = "platform",
                label = "电商平台",
                choices = c(
                  "请选择" = "", 
                  "Etsy" = "Etsy", 
                  "Shopify" = "Shopify", 
                  "TikTok" = "TikTok", 
                  "其他" = "其他"
                ),
                selected = "",
                width = "100%"
              )
            )
          ),
          
          # 顾客姓名
          textInput("customer_name", "顾客姓名", placeholder = "请输入顾客姓名", width = "100%"),
          
          # 运单号1
          textInput("tracking_number1", "运单号", placeholder = "请输入运单号", width = "100%"),
          
          # 运单号动态添加部分
          uiOutput("additional_tracking_numbers"),
          actionButton(
            "add_tracking_btn",
            label = "点击增加一行运单号输入栏",
            icon = icon("plus"),
            style = "background-color: #28A745; color: white; border: none; margin-top: 10px; width: 100%; font-size: 14px;"
          ),
          
          tags$div(style = "margin-top: 20px;"),  # 增加20px垂直间距
          
          # 订单图片上传
          imageModuleUI("image_sold", label = "订单图片上传", label_color = "#007BFF"),
          
          # 订单备注
          textAreaInput("order_notes", "订单备注", placeholder = "请输入备注内容", width = "100%"),
          
          # 按钮区
          div(
            style = "margin-top: 10px; display: flex; justify-content: space-between;",
            actionButton(
              "register_order_btn",
              "登记/更新订单",
              icon = icon("save"),
              class = "btn-primary",
              style = "font-size: 16px; width: 48%; height: 42px;"
            ),
            actionButton(
              "clear_order_btn",
              "清空订单输入",
              icon = icon("eraser"),
              class = "btn-warning",
              style = "font-size: 16px; width: 48%; height: 42px;"
            )
          )
        )
      ),
      
      # 主面板：右侧物品选择和已选物品列表
      div(
        class = "main-panel",
        
        fluidRow(
          # 货架部分
          column(6,
                 div(
                   class = "card",
                   style = "padding: 20px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                   tags$h4(
                     HTML(paste0(as.character(icon("warehouse")), "  货架")), 
                     style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"
                   ),
                   DTOutput("shelf_table")  # 显示货架上的物品
                 )
          ),
          
          # 箱子部分
          column(6,
                 div(
                   class = "card",
                   style = "padding: 20px; margin-bottom: 20px; border: 1px solid #28A745; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                   tags$h4(
                     HTML(paste0(as.character(icon("box")), "  发货箱")), 
                     style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
                   ),
                   DTOutput("box_table"),  # 显示已放入箱子的物品
                   
                   fluidRow(
                     column(
                       width = 7, # 左侧按钮宽度
                       actionButton(
                         "confirm_order_btn",
                         "确认售出",
                         icon = icon("check"),
                         class = "btn-primary", 
                         style = "font-size: 16px; width: 100%; height: 50px; margin-top: 10px;"
                       )
                     ),
                     column(
                       width = 5, # 右侧选择框宽度
                       tags$div(
                         style = "
    display: flex; 
    align-items: center; 
    justify-content: flex-start; 
    border: 1px solid #007BFF; 
    border-radius: 8px; 
    height: 50px; 
    padding: 0 10px; 
    margin-top: 10px;
  ",
                         tags$span(
                           "国际运输:", 
                           style = "font-size: 16px; font-weight: bold; margin-right: 15px; line-height: 1;"
                         ),
                         tags$div(
                           style = "
      display: flex; 
      align-items: center; 
      height: 100%; 
      margin-bottom: 0; /* 移除底部间距 */
    ",
                           tags$style(HTML("
      #sold_shipping_method .radio {
        margin-bottom: 0 !important; /* 移除默认的 margin */
      }
      #sold_shipping_method {
        margin-bottom: 0 !important; /* 避免容器本身多余间距 */
      }
    ")),
                           radioButtons(
                             inputId = "sold_shipping_method",
                             label = NULL, # 去掉默认 label
                             choices = list("空运" = "空运", "海运" = "海运"),
                             selected = "空运",  # 默认选择空运
                             inline = TRUE       # 设置为横向排布
                           )
                         )
                       )
                       
                       
                       
                     )
                   )
                 )
          )
        ),
        
        tags$hr(style = "margin: 20px 0; border: 1px solid #ddd;"),  # 添加分隔线
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_sold",
              uniqueItemsTableUI("unique_items_table_sold")
            )
          )
        )
      )
    )
  ), # end of 售出 tab
  
  
  tabPanel(
    "物品管理", icon = icon("list-check"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("删除选中物品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              ),
              
              # 确认删除按钮
              actionButton(
                "confirm_delete_btn", 
                "确认删除", 
                icon = icon("check"), 
                class = "btn-primary", 
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        ),
        
        tags$hr(), # 分隔线
        
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          # 卡片标题
          div(
            style = "margin-bottom: 10px; padding-bottom: 8px;",
            tags$h4("更新商品图片", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
            
            imageModuleUI("image_manage", label = ""),
            
            actionButton("update_image_btn", "更新商品图片", icon = icon("pen"), style = "background-color: #006400; color: white;")
          ),
        )
      ),
      
      div(
        class = "main-panel",
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_manage",
              uniqueItemsTableUI("unique_items_table_manage")
            )
          )
        )
      )
    )
  ), # end of 物品管理 tab
  
  tabPanel(
    "瑕疵品管理", icon = icon("exclamation-circle"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        style = "width: 300px;", # override 宽度
        
        # 登记瑕疵品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_defective", 
            "登记瑕疵品", 
            icon = icon("circle-exclamation"),
            class = "btn-warning", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示无瑕品", class = "control-label"),  
          switchInput(
            inputId = "show_perfects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
        # 登记修复品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_repair", 
            "登记修复品", 
            icon = icon("hammer"),
            class = "btn-success", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示瑕疵品", class = "control-label"),  
          switchInput(
            inputId = "show_defects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
        # 备注输入框
        textAreaInput(
          inputId = "manage_defective_notes",
          label = "备注：",
          placeholder = "请输入备注内容...",
          width = "100%"
        )
      ),
      
      # 主面板：物品状态表
      div(
        class = "main-panel",
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_defect",
              uniqueItemsTableUI("unique_items_table_defect")
            )
          )
        )
      )
    )
  ), # end of 瑕疵品管理 tab
  
  
  tabPanel(
    "国际物流管理", icon = icon("globe"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        fluidRow(
          column(
            12,
            textInput(
              "intl_tracking_number",
              "国际运单号:",
              placeholder = "请输入空运或海运运单号",
              width = "100%"
            )
          ),
          column(
            12,
            selectInput(
              "intl_shipping_method",
              "国际运输方式:",
              choices = c("空运" = "空运", "海运" = "海运"),
              selected = "空运",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "link_tracking_btn",
              "挂靠运单",
              icon = icon("link"),
              class = "btn-primary",
              style = "margin-top: 20px; width: 100%;"
            )
          ),
          column(
            6,
            actionButton(
              "delete_tracking_btn",
              "删除运单",
              icon = icon("trash"),
              class = "btn-danger",
              style = "margin-top: 20px; width: 100%;"
            )
          )
        )
      ),
      div(
        class = "main-panel",
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_logistics",
              uniqueItemsTableUI("unique_items_table_logistics")
            )
          )
        )
      )
    )
  ), # end of 国际物流管理 tab
  
  
  # 订单管理分页
  tabPanel(
    title = "订单管理",
    icon = icon("clipboard-list"),
    div(
      class = "layout-container",
      
      # 左侧：筛选条件和订单信息
      div(
        class = "sticky-sidebar",
        style = "width: 280px;",  # 缩窄侧边栏宽度
        
        # 筛选条件 Card
        div(
          class = "card",
          style = "padding: 15px; border: 1px solid #28A745; border-radius: 8px; margin-bottom: 15px;",
          tags$h4("订单筛选", style = "color: #28A745; font-weight: bold;"),
          textInput("filter_order_id", "订单号", placeholder = "输入订单号", width = "100%"),
          textInput("filter_customer_name", "顾客姓名", placeholder = "输入顾客姓名", width = "100%"),
          selectInput(
            inputId = "filter_platform", 
            label = "电商平台",
            choices = c("所有平台" = "", "Etsy" = "Etsy", "Shopify" = "Shopify", "TikTok" = "TikTok", "其他" = "其他"),
            selected = "", 
            width = "100%"
          )
        ),
        
        # 订单信息 Card
        div(
          class = "card",
          style = "padding: 15px; border: 1px solid #007BFF; border-radius: 8px;",
          tags$h4("订单信息修改", style = "color: #007BFF; font-weight: bold;"),
          textInput("update_customer_name", "顾客姓名", placeholder = "更新顾客姓名", width = "100%"),
          selectInput(
            inputId = "update_platform", 
            label = "电商平台",
            choices = c("Etsy", "Shopify", "TikTok", "其他"),
            selected = NULL, 
            width = "100%"
          ),
          textInput("update_tracking_number1", "运单号", placeholder = "更新运单号", width = "100%"),
          textInput("update_tracking_number2", "运单号2", placeholder = "更新运单号2", width = "100%"),
          textInput("update_tracking_number3", "运单号3", placeholder = "更新运单号3", width = "100%"),
          textAreaInput("update_order_notes", "订单备注", placeholder = "更新备注内容", width = "100%"),
          
          # 图片模块
          imageModuleUI("image_order_manage", label = "订单图片上传", label_color = "#007BFF"),
          
          # 更新和删除按钮
          div(
            style = "margin-top: 20px; display: flex; justify-content: space-between;",
            actionButton("update_order_btn", "更新订单", class = "btn-success", style = "width: 48%;"),
            actionButton("delete_order_btn", "删除订单", class = "btn-danger", style = "width: 48%;")
          )
        )
      ),
      
      # 右侧：主面板
      div(
        class = "main-panel",
        style = "display: flex; gap: 20px;",
        
        # 订单表
        div(
          class = "card",
          style = "flex: 1 0 40%; padding: 15px; border: 1px solid #ccc; border-radius: 8px;",  # 窄一些
          tags$h4("订单表", style = "color: #007BFF; font-weight: bold;"),
          orderTableUI("orders_table_module")  # 订单表模块
        ),
        
        # 关联物品表
        div(
          class = "card",
          style = "flex: 1 0 60%; padding: 15px; border: 1px solid #ccc; border-radius: 8px;",  # 宽一些
          uiOutput("associated_items_title"),  # 动态标题
          uniqueItemsTableUI("associated_items_table_module")  # 关联物品表模块
        )
      )
    )
  ), # end of 订单管理
  
  tabPanel(
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%")
        )
      ),
      div(
        class = "main-panel",
        # 使用 tabsetPanel 来组织分页
        tabsetPanel(
          type = "tabs", # 使用 tabs 样式
          tabPanel(
            "商品状态",
            fluidRow(
              column(
                4,
                div(
                  class = "card",
                  style = "height: 453.89px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品信息", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                  uiOutput("query_item_info") # 动态渲染物品信息
                )
              ),
              
              column(
                4,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态图表", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                  plotlyOutput("inventory_status_chart", height = "400px") # 使用 plotlyOutput
                )
              ),
              
              column(
                4,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("瑕疵情况图表", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                  plotlyOutput("defect_status_chart", height = "400px") # 使用 plotlyOutput
                )
              )
            ),
            
            div(
              style = "display: flex; flex-direction: column;",
              div(
                style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
                div(
                  id = "inventory_table_container_query",
                  DTOutput("filtered_inventory_table_query")
                )
              )
            )
          ), # end of 商品状态
          
          tabPanel(
            "采购开销",
            fluidRow(
              column(
                12,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  
                  # 选择器行
                  fluidRow(
                    column(4,                   
                           dateRangeInput(
                             "time_range",
                             label = "选择采购时间范围",
                             start = Sys.Date() - 30, # 默认最近30天
                             end = Sys.Date()
                           )),
                    column(4,
                           radioButtons(
                             "precision",
                             label = "选择统计精度",
                             choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"),
                             selected = "天",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(4,
                           radioButtons(
                             "expense_type",
                             label = "选择显示内容",
                             choices = c("总开销" = "total", "物品成本" = "cost", "运费开销" = "shipping"),
                             selected = "total",
                             inline = TRUE # 使选项横向排列
                           ))
                  ),
                  
                  # 图表行：柱状图 + 饼图
                  fluidRow(
                    column(9, plotlyOutput("bar_chart", height = "350px")), # 80% 宽度柱状图
                    column(3, plotlyOutput("pie_chart", height = "350px"))  # 20% 宽度饼图
                  )
                )
              )
            )
          ) # end of 开销汇总tab
          
          
          # 你可以在这里添加更多的 tabPanel 来扩展图表
          
        ) #end of tabpanel
      )
    )
  ), # end of 查询 tab
  
  tabPanel(
    "数据下载", icon = icon("download"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          
          tags$h4("表格筛选", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          # 供应商筛选
          uiOutput("download_maker_ui"),  # 动态生成供应商筛选器,
          
          # 商品名称筛选
          selectizeInput(
            inputId = "download_item_name",
            label = "商品名称:",
            choices = NULL,          # 动态加载商品名称
            selected = NULL,         # 默认全选
            multiple = FALSE,        # 单选，适合精确匹配
            options = list(          # 提供更好的交互体验
              placeholder = "请输入商品名称...",
              create = FALSE         # 不允许用户输入新值
            ),
            width = "100%"
          ),
          
          # 采购日期筛选
          dateRangeInput(
            inputId = "download_date_range",
            label = "选择采购日期范围:",
            start = Sys.Date() - 30, # 默认最近30天
            end = Sys.Date(),        # 默认结束日期为今天
            format = "yyyy-mm-dd",   # 日期格式
            separator = " 至 ",
            width = "100%"
          ),
          
          actionButton("download_reset_filters", "重置筛选", class = "btn-secondary")
        ),
        
        tags$hr(),
        
        downloadButton("download_unique_items_xlsx", "下载物品明细表 (Excel)", 
                       class = "btn-primary", style = "width: 100%;")
        
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("unique_items_table_download")
      )
    )
  ), # End of 数据下载 tab
  
  tabPanel(
    "管理员", icon = icon("user-shield"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        style = "width: 300px;",
        passwordInput("admin_password", "请输入管理员密码：", width = "100%"),
        actionButton("admin_login_btn", "登录", icon = icon("unlock"), class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
        tags$hr(),
        uiOutput("admin_controls")
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("admin_items_table")  # 使用你的模组渲染物品明细表
      )
    )
  ) # End of 管理员 tab
)