# Define UI
ui <- navbarPage(
  title = "库存管理系统（美国端）",
  id = "inventory_us",  # 设置 ID，用于监听当前选中的主页面
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
        background-color: #4b0363 !important; /* 设置背景颜色 */
        white-space: nowrap !important; /* 确保所有子元素在单行内 */
      }
      
      /* 鼠标悬停时修改标题颜色 */
      .navbar-brand:hover {
        color: #FFD700 !important; /* 悬停时标题文字颜色 */
      } 
      /* 鼠标悬停导航项时的颜色 */
      .navbar-nav > li > a:hover {
        color: #FFD700 !important;           /* 悬停文字颜色 */
        background-color: #4b0363 !important;/* 悬停背景颜色 */
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
        width: 380px; /* 固定宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        flex-shrink: 0; /* 防止压缩 */
      }
    
      .order-info-container {
        position: relative; /* 相对定位 */
        overflow: hidden;   /* 避免外层滚动 */
      }
      
      .order-info-scroll {
        height: 100%;
        overflow-x: auto; /* 水平滚动 */
        overflow-y: hidden; /* 禁用垂直滚动 */
        white-space: nowrap; /* 确保内容不换行 */
        display: inline-flex; /* 子项水平排列 */
        gap: 15px; /* 卡片间距 */
        padding: 15px;
      }
      
      .main-panel {
        position: relative;
        flex-grow: 1; /* 允许主面板扩展 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
        overflow: hidden; /* 避免主面板影响滚动条 */
      }
      
      /* 自定义 selectize 样式 */
      .custom-selectize .selectize-input {
        font-size: 12px !important; /* 设置输入框字体大小 */
      }
      .custom-selectize .selectize-dropdown-content {
        font-size: 12px !important; /* 设置下拉菜单字体大小 */
      }
      
      table.dataTable thead th {
        white-space: nowrap; /* 表头内容强制不换行 */
      }
      
      .order-card {
        border: 1px solid #ddd;
        transition: border-color 0.2s, box-shadow 0.2s;
      }
      
      .order-card:hover {
        border-color: #007BFF;
        box-shadow: 0px 4px 8px rgba(0, 123, 255, 0.2);
      }
      
      .order-card.selected {
        border-color: #007BFF !important;
        box-shadow: 0px 4px 8px rgba(0, 123, 255, 0.5) !important;
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
        });
      "))
    )
  ),
  
  tabPanel(
    "入库", icon = icon("arrow-circle-down"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏

        itemFilterUI(id = "inbound_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = FALSE),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #007BFF; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
              ),
              
              # SKU 输入框
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "inbound_sku", 
                  label = NULL, 
                  placeholder = "请扫描或输入SKU",
                  width = "100%"
                ),
                checkboxInput(
                  "auto_inbound",  # 勾选框的 inputId
                  label = "自动入库（瑕疵信息不会采用）", 
                  value = FALSE  # 默认不勾选
                ),
              ),
              
              div(
                style = "width: 100%;",
                numericInput(
                  inputId = "inbound_quantity",
                  label = "入库数量",
                  value = 1,        # 默认值
                  min = 1,          # 最小值
                  max = 1,
                  step = 1          # 步长
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
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow( 
          # 条形码生成下载按钮
          column(12,              
                 tags$div(
                   class = "card",
                   style = "padding: 15px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                   
                   # 卡片标题
                   div(
                     style = "margin-bottom: 10px; padding-bottom: 8px;",
                     tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
                   ),
                   
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     actionButton("export_select_btn", "生成条形码", icon = icon("barcode"), class = "btn-info"),
                     downloadButton("download_select_pdf", "下载条形码", class = "btn-primary")
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
    "发货", icon = icon("truck"),
    div(
      class = "layout-container",  # Flexbox 容器
      
      # 左侧：发货条形码输入区域
      div(
        class = "sticky-sidebar",

        # 国内售出订单发货
        div(class = "card", style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; margin-bottom: 20px;",
            tags$h4("国内售出订单发货", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
            textInput("shipping_bill_number", "运单号:", placeholder = "请扫描运单号", width = "100%"),
            textInput("sku_input", "SKU:", placeholder = "请扫描SKU条形码", width = "100%"),
            uiOutput("dynamic_ship_button"),  # 动态按钮位置
            actionButton("clear_shipping_bill_btn", "清空", icon = icon("trash-alt"), class = "btn-danger", style = "margin-top: 10px;"),
        ),
        
        downloadButton("download_pdf", ""),
        
        # 美国售出订单发货
        div(class = "card", style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; margin-bottom: 20px;",
            tags$h4("美国售出订单发货", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
            textInput("us_shipping_bill_number", "运单号:", placeholder = "请扫描运单号", width = "100%"),
            textInput("us_shipping_sku_input", "SKU:", placeholder = "请扫描SKU条形码", width = "100%"),
            selectInput("us_shipping_platform", "平台:", choices = c("请选择" = "", "Etsy" = "Etsy", "Shopify" = "Shopify", "TikTok" = "TikTok"), selected = "TikTok", width = "100%"),
            textAreaInput("us_shipping_order_notes", "订单备注:", placeholder = "请输入订单备注", width = "100%", height = "80px"),
            actionButton("us_ship_order_btn", "发货", icon = icon("paper-plane"), class = "btn-success", style = "margin-top: 10px;", width = "50%"),
            actionButton("clear_us_shipping_bill_btn", "重置", icon = icon("rotate-right"), class = "btn-danger", style = "margin-top: 10px;")
        ),
      ),
        
      # 右侧：主面板内容
      div(
        class = "main-panel",
        
        # 订单信息区域
        div(
          class = "order-info-container",  # 单独的容器样式
          style = "height: 360px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 8px; 
             padding: 0px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); overflow: hidden;",
          
          # 内部可滚动区域
          div(
            class = "order-info-scroll",  # 滚动条样式
            style = "height: 100%; width: 100%; overflow-x: auto; overflow-y: hidden; padding: 15px;",
            div(
              style = "white-space: nowrap; display: inline-flex; gap: 15px;",  # 水平布局
              uiOutput("order_info_card")  # 动态显示订单信息卡片
            )
          )
        ),
        
        # 订单内物品区域
        div(
          style = "flex-grow: 1; overflow-y: auto; padding: 15px; border: 1px solid #28A745; 
             border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
          class = "card",
          uiOutput("order_items_title"),  # 动态标题
          uiOutput("shipping_order_items_cards")  # 动态显示订单内物品卡片
        )
      )
    )
  ), # End of "发货"
  
  
  tabPanel(
    "订单管理", icon = icon("clipboard-list"),
    div(
      class = "layout-container",
      
      # 左侧：动态变化的筛选区和订单登记
      div(
        class = "sticky-sidebar",
        style = "width: 400px;",  # 使用内联样式覆盖宽度
        
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #28A745; border-radius: 8px;",
          tags$h4("订单筛选", style = "color: #28A745; font-weight: bold;"),
          
          textInput("filter_order_id", "订单号", placeholder = "输入订单号", width = "100%"),
          textInput("filter_tracking_id", "运单号", placeholder = "输入运单号", width = "100%"),
          
          fluidRow(
            column(6, 
                   textInput("filter_customer_name", "顾客姓名", placeholder = "输入顾客姓名", width = "100%")),
            column(6, 
                   textInput("filter_customer_netname", "顾客网名", placeholder = "输入顾客网名", width = "100%"))
          ),
          
          fluidRow(
            column(6, 
                   selectInput(
                     inputId = "filter_platform",
                     label = "电商平台",
                     choices = c("所有平台" = "", "Etsy", "Shopify", "TikTok", "其他"),
                     selected = "",
                     width = "100%"
                   )),
            column(6, 
                   selectInput(
                     inputId = "filter_order_status",
                     label = "订单状态",
                     choices = c("所有状态" = "", "备货", "预定", "调货", "装箱", "发出", "在途", "送达"),
                     selected = "",
                     width = "100%"
                   ))
          ),
          
          fluidRow(
            column(6, 
                   textInput("filter_sku", "SKU反查", placeholder = "输入SKU", width = "100%")),
            column(6, 
                   autocompleteInputUI("sold", label = "商品名反查", placeholder = "输入商品名"))
          ),
          
          fluidRow(
            column(6, 
                   actionButton("delete_order_btn", "删除订单", class = "btn-danger", style = "width: 100%;")),
            column(6, 
                   actionButton("reset_filter_btn", "清空筛选条件", class = "btn-secondary", style = "width: 100%;"))
          )
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        # 订单登记区（共用）
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          tags$h4("订单登记与更新", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          fluidRow(
            column(
              7,
              textInput("order_id", "订单号", placeholder = "请输入订单号", width = "100%")
            ),
            column(
              5,
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
          
          fluidRow(
            column(6, textInput("customer_name", "顾客姓名", placeholder = "请输入", width = "100%")),
            column(6, textInput("customer_netname", "顾客网名", placeholder = "请输入", width = "100%"))
          ),
          
          # 运单号
          textInput("tracking_number", "运单号", placeholder = "请输入运单号", width = "100%"),
          
          tags$div(style = "margin-top: 20px;"),  # 增加20px垂直间距
          
          # 订单图片上传
          imageModuleUI("image_sold", label = "订单图片上传", label_color = "#007BFF"),
          
          # 订单备注
          textAreaInput("order_notes", "订单备注", placeholder = "请输入备注内容", width = "100%"),
          
          # 按钮区
          div(
            style = "margin-top: 10px; display: flex; flex-direction: column; gap: 5px;",  # 增加垂直间距
            
            div(
              style = "display: flex; justify-content: space-between;",
              uiOutput("register_order_button_ui"),
              actionButton(
                "clear_order_btn",
                "清空订单",
                icon = icon("eraser"),
                class = "btn-warning",
                style = "font-size: 16px; width: 48%; height: 42px;"
              )
            ),
            
            div(
              style = "margin-top: 5px; display: flex; justify-content: center;",  # 设置行间距
              actionButton(
                "merge_order_btn",
                "合并订单",
                icon = icon("object-group"),
                class = "btn-primary",
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      
      # 主面板：订单管理
      div(
        class = "main-panel",
        div(
          class = "card",
          style = "height: 460px; padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
          orderTableUI("orders_table_module")
        ),
        div(
          class = "card",
          style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
          uiOutput("associated_items_title"),  # 动态标题
          uiOutput("order_items_cards")  # 动态显示订单内物品卡片
        )
      )
    )
  ), # End of 订单管理
  
  
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
        
        itemFilterUI(id = "logistic_filter", 
                     use_purchase_date = FALSE,
                     use_sold_date = TRUE, use_exit_date = TRUE,
                     border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        div(
          class = "card shadow-sm",
          style = "padding: 10px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          # Card 标题
          tags$h4("登记国际运单", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          # 输入内容
          fluidRow(
            column(12, textInput("intl_tracking_number", "国际运单号:", placeholder = "请输入空运或海运运单号", width = "100%")),
            column(12, selectInput("intl_shipping_method", "国际运输方式:", choices = c("空运" = "空运", "海运" = "海运"), selected = "空运", width = "100%")),
            column(12, numericInput("intl_total_shipping_cost", "国际物流总运费 (元):", value = 0, min = 0, width = "100%"))
          ),
          
          fluidRow(
            column(4, actionButton("register_shipment_btn", "登记", icon = icon("save"), class = "btn-info", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
            column(4, actionButton("batch_value_btn", "货值", icon = icon("dollar-sign"), class = "btn-success", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
            column(4, actionButton("delete_shipment_btn", "删除", icon = icon("trash"), class = "btn-danger", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
          )
        ),
        
        fluidRow(
          column(6, actionButton("link_tracking_btn", "挂靠运单", icon = icon("link"), class = "btn-primary", style = "margin-top: 20px; width: 100%;", disabled = TRUE)),
          column(6, actionButton("delete_tracking_btn", "解除挂靠", icon = icon("link-slash"), class = "btn-danger", style = "margin-top: 20px; width: 100%;"))
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

  
  tabPanel(
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = FALSE),
        
        tags$hr(),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
        )
      ),
      div(
        class = "main-panel",
        # 使用 tabsetPanel 来组织分页
        tabsetPanel(
          id = "query_tabs",
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
            start = Sys.Date() - 365, # 默认最近365天
            end = Sys.Date(),        # 默认结束日期为今天
            format = "yyyy-mm-dd",   # 日期格式
            separator = " 至 ",
            width = "100%"
          ),
          
          actionButton("download_reset_filters", "重置筛选", class = "btn-secondary")
        ),
        
        tags$hr(),
        
        downloadButton(
          outputId = "download_summary_xlsx",
          label = "下载物品汇总表（按采购日期）",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        ),
        
        downloadButton(
          outputId = "download_details_xlsx",
          label = "下载物品明细表",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        )
        
        
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