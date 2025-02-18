# Define UI
ui <- navbarPage(
  title = "ERP系统（美国端）",
  id = "inventory_us",  # 设置 ID，用于监听当前选中的主页面
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    shinyjs::useShinyjs(),  # 启用 shinyjs
    
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "https://www.goldenbeanllc.com/icons/favicon-96x96.png"),
      
      tags$style(HTML("

      /* 强制导航栏支持水平滚动 */
      .navbar-nav {
        display: flex !important;
        flex-wrap: nowrap !important;
        overflow-x: auto !important;
        white-space: nowrap !important;
        max-width: 100% !important; /* 防止宽度限制 */
      }
      
      /* 导航栏滚动条样式 */
      .navbar-nav::-webkit-scrollbar {
        height: 6px;
      }
      .navbar-nav::-webkit-scrollbar-thumb {
        background: #007BFF;
        border-radius: 10px;
      }
      
      /* 强制显示滚动条，小于1380px时 */
      @media (max-width: 1380px) {
        .navbar-nav {
          overflow-x: scroll !important;
        }
        .navbar-brand {
          display: none !important; /* 隐藏标题 */
        }
      }
      
      /* 限制 .navbar 的宽度扩展 */
      .navbar {
        display: block !important;
        overflow: hidden !important;
        width: 100% !important;
        background-color: #4b0363 !important; /* 设置背景颜色为紫色 */
      }
      
      /* 小屏幕调整字体和间距 */
      @media (max-width: 900px) {
        .navbar-nav > li > a {
          font-size: 12px !important;
          padding: 6px 8px !important;
        }
      }
      
      /* 为导航栏顶部留出空间 */
      body {
        padding-top: 70px !important;
      }
      
      /* --------------------------------------------------------- */
      
      /* 鼠标悬停时修改标题颜色 */
      .navbar-brand:hover {
        color: #FFD700 !important; /* 悬停时标题文字颜色 */
      } 
      /* 鼠标悬停导航项时的颜色 */
      .navbar-nav > li > a:hover {
        color: #FFD700 !important;           /* 悬停文字颜色 */
        background-color: #4b0363 !important;/* 悬停背景颜色 */
      }
      
      .nav-pills > li.active > a {
        background-color: #4b0363 !important; /* 设置激活选项卡的背景颜色 */
      }
      
      /* --------------------------------------------------------- */
      
      /* Flexbox 容器 */
      .layout-container {
        display: flex;
        flex-direction: row;
        height: 100%;
        width: 100%;
        overflow: hidden; /* 禁止滚动条 */
      }

      .sticky-sidebar {
        position: sticky; /* 保持固定 */
        z-index: 900;
        flex: 0 0 auto; /* 固定宽度并防止被压缩 */
        width: 380px; /* 默认宽度 */
        min-width: 280px; /* 最小宽度 */
        max-width: 580px; /* 最大宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border-right: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .main-panel {
        flex-grow: 1;
        overflow: hidden; /* 禁止滚动条 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .resizable-divider {
        background-color: #aaa;
        width: 5px;
        cursor: ew-resize;
        flex-shrink: 0;
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
      
       /* DT 搜索框左对齐 */
      div.dataTables_wrapper div.dataTables_filter {
          text-align: left !important; /* 搜索框文字左对齐 */
          float: left !important;      /* 搜索框容器浮动到左侧 */
        }
      div.dataTables_wrapper div.dataTables_filter label {
        display: inline-flex;       /* 让标签和输入框同行 */
        align-items: center;       /* 垂直居中对齐 */
        gap: 5px;                  /* 间距调整 */
      }
      
      /* 采购流程链条箭头 */
      .arrow-icon {
        margin-right: 10px;
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
        
        // JavaScript 实现分隔条拖拽
        document.addEventListener('DOMContentLoaded', function() {
          function enableResizing(divider) {
            const sidebar = divider.previousElementSibling;  // 分隔条左侧的 sidebar
            let isResizing = false;
      
            divider.addEventListener('mousedown', function(e) {
              isResizing = true;
              document.body.style.cursor = 'ew-resize';
              document.body.style.userSelect = 'none';
            });
      
            document.addEventListener('mousemove', function(e) {
              if (!isResizing) return;
              const newSidebarWidth = Math.max(200, Math.min(600, e.clientX)); // 限制宽度范围
              sidebar.style.flex = `0 0 ${newSidebarWidth}px`;
              
               // 调整所有表格列宽
              $('.dataTable').DataTable().columns.adjust();
            });
      
            document.addEventListener('mouseup', function() {
              if (isResizing) {
                isResizing = false;
                document.body.style.cursor = '';
                document.body.style.userSelect = '';
                
                // 再次确保表格布局正确
                $('.dataTable').DataTable().columns.adjust();
              }
            });
          }
      
          function bindResizableDividers() {
            document.querySelectorAll('.resizable-divider').forEach(function(divider) {
              if (!divider.dataset.bound) { // 避免重复绑定
                enableResizing(divider);
                divider.dataset.bound = true; // 标记为已绑定
              }
            });
          }
      
          bindResizableDividers();
      
          // 分页切换后重新绑定
          $(document).on('shown.bs.tab', function() {
            bindResizableDividers();
            $('.dataTable').DataTable().columns.adjust();
          });
        });
        
         // 入库成功音效
        function playInboundSuccessSound() {
          var audio = new Audio('https://www.goldenbeanllc.com/sounds/success-josie.mp3');
          audio.play();
        }
        
        // 入库错误音效
        function playInboundErrorSound() {
          var audio = new Audio('https://www.goldenbeanllc.com/sounds/inbound_error.mp3');
          audio.play();
        }
        
        // 右键点击查询库存页面
        $(document).ready(function() {
          $('#filtered_inventory_table_query').on('contextmenu', 'tr', function(event) {
            event.preventDefault();
            var rowIdx = $(this).index();
            
            Shiny.setInputValue('selected_inventory_row', rowIdx + 1, {priority: 'event'});
      
            $('#context-menu').css({
              display: 'block',
              left: event.pageX + 'px',
              top: event.pageY + 'px'
            });
          });
      
          $(document).on('click', function(event) {
            if (!$(event.target).closest('#context-menu').length) {
              $('#context-menu').hide();
            }
          });
        });
      "))
    )
  ),
  
  tabPanel(
    "协作", icon = icon("users"),
    div(
      class = "layout-container",
      
      # 左侧侧边栏
      div(
        class = "sticky-sidebar",
        div(
          tags$h4("库存品请求", style = "font-weight: bold; color: #007BFF;"),
          fluidRow(
            column(6, textInput("search_sku", "按SKU搜索", placeholder = "输入SKU", width = "100%")),
            column(6, textInput("search_name", "按物品名搜索", placeholder = "输入物品名", width = "100%"))
          ),
          div(
            style = "margin-bottom: 10px;",
            div(
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
              tags$h5("物品预览", style = "font-weight: bold; color: #007BFF;"),
              uiOutput("item_preview")
            )
          ),
          numericInput("request_quantity", "请求数量", value = 0, min = 1, width = "100%"),
          textAreaInput("request_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"),
          actionButton("add_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;"),
          tags$hr(),
          tags$h4("新商品请求", style = "font-weight: bold; color: #007BFF;"),
          imageModuleUI("image_requests", label = "请求物品图片上传"),
          textInput("custom_description", "物品名", placeholder = "输入物品名", width = "100%"),
          numericInput("custom_quantity", "请求数量", value = 0, min = 1, width = "100%"),
          textAreaInput("custom_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"),
          actionButton("submit_custom_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;")
        )
      ),
      
      # 可调整的分割线
      div(class = "resizable-divider"),
      
      # 右侧主要面板
      div(
        class = "main-panel",
        
        # 采购流程 tabset
        tabsetPanel(
          id = "collaboration_tabs",
          type = "pills",
          
          # 采购流程链
          tabPanel(
            title = "采购请求",
            uiOutput("purchase_request_board")
          ),
          tabPanel(
            title = div(
              tags$span(class = "arrow-icon", icon("arrow-right")),
              "已安排",
            ), 
            uiOutput("provider_arranged_board")
          ),
          tabPanel(
            title = div(
              tags$span(class = "arrow-icon", icon("arrow-right")),
              "已完成",
            ), 
            uiOutput("done_paid_board")
          ),
          
          # 出库请求
          tabPanel(
            title = "出库请求",
            uiOutput("outbound_request_board")
          )
        )
      )
    )
  ), # End of 协作 tab
  
  tabPanel(
    "入库", icon = icon("arrow-circle-down"),
    div(class = "layout-container",
        div(class = "sticky-sidebar",
            itemFilterUI(id = "inbound_filter", border_color = "#28A745", text_color = "#28A745", 
                         status_choices = c("所有状态" = "", "国内出库", "美国入库"), use_purchase_date = FALSE),
            tags$hr(style = "margin: 5px 0; border: none;"),
            
            div(class = "card shadow-sm", style = "border: 1px solid #007BFF; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 10px;"),
                textInput("inbound_sku", label = NULL, placeholder = "请扫描或输入SKU", width = "100%"),
                checkboxInput("auto_inbound", "自动入库（瑕疵信息不会采用）", value = FALSE),
                conditionalPanel(
                  condition = "input.auto_inbound == true",  # 只有 auto_inbound 被选中时才显示
                  checkboxInput("speak_item_name", "念出商品名", value = FALSE)
                ),
                numericInput("inbound_quantity", "入库数量", value = 1, min = 1, max = 1, step = 1),
                
                div(style = "margin-bottom: 20px; display: flex; align-items: center;",
                    tags$input(type = "checkbox", id = "defective_item", style = "width: 20px; height: 20px; margin-right: 10px;"),
                    tags$label("瑕疵品", `for` = "defective_item", style = "font-size: 18px; font-weight: bold; color: #444;")
                ),
                
                div(id = "defective_notes_container", style = "display: none; margin-top: 10px;", 
                    textAreaInput("defective_notes", "瑕疵品备注：", placeholder = "请输入备注内容...", width = "100%")
                ),
                
                actionButton("confirm_inbound_btn", "确认入库", icon = icon("check"), class = "btn-primary", 
                             style = "font-size: 16px; width: 100%; height: 42px;"),
                
                actionButton("toggle_view", label = "切换至：大图模式", icon = icon("exchange-alt"), class = "btn-warning", style = "width: 100%; margin-top: 10px")
            ),
            
            tags$hr(style = "margin: 5px 0; border: none;"),
            
            div(class = "card", style = "padding: 15px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 5px;",
                tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold;"),
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                    actionButton("export_select_btn", "生成条形码", icon = icon("barcode"), class = "btn-info"),
                    downloadButton("download_select_pdf", "下载条形码", class = "btn-primary")
                )
            )
        ),
        
        div(class = "resizable-divider"),
        
        div(class = "main-panel", style = "display: flex; flex-direction: column;",
            div(id = "table_mode",
                div(style = "height: 300px; margin-bottom: 10px;", 
                    column(12, uiOutput("inbound_item_info"))
                ),
                div(style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;", 
                    div(id = "item_table_container_inbound", uniqueItemsTableUI("unique_items_table_inbound"))
                )
            ),
            
            div(id = "image_mode", style = "display: none;",
                div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; height: calc(100vh - 120px);",
                    uiOutput("inbound_item_info_image_mode")
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
        tabsetPanel(
          id = "shipping_tabs",
          type = "pills",
          tabPanel(
            "国内售出发货", icon = tags$img(src = paste0(host_url, "icons/icons8-china-48.png"), height = '20px', style = 'margin-right: 5px;'),
            div(
              class = "card",
              style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; margin-bottom: 20px;",
              tags$h4("国内售出订单发货", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              textInput("shipping_bill_number", "运单号:", placeholder = "请扫描运单号", width = "100%"),
              textInput("order_id_input", "订单号:", placeholder = "请输入订单号", width = "100%"),
              textInput("sku_input", "SKU:", placeholder = "请扫描SKU条形码", width = "100%"),
              actionButton("clear_shipping_bill_btn", "清空", icon = icon("trash-alt"), class = "btn-danger", style = "margin-top: 10px;", width = "100%"),
              uiOutput("dynamic_ship_button", style = "margin-top: 10px;"),  # 动态按钮位置
              uiOutput("dynamic_label_download_button", style = "margin-top: 10px;")  # 动态生成按钮
            )
          ),
          
          tabPanel(
            "美国售出发货", icon = tags$img(src = paste0(host_url, "icons/icons8-usa-48.png"), height = '20px', style = 'margin-right: 5px;'),
            div(
              class = "card",
              style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; margin-bottom: 20px;",
              tags$h4("美国售出订单发货", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
              textInput("us_shipping_bill_number", "运单号:", placeholder = "请扫描运单号", width = "100%"),
              hidden(textInput("us_shipping_sku_input", "SKU:", placeholder = "请扫描SKU条形码", width = "100%")),              
              selectInput(
                "us_shipping_platform",
                "平台:",
                choices = c("请选择" = "", "Etsy" = "Etsy", "Shopify" = "Shopify", "TikTok" = "TikTok"),
                selected = "TikTok",
                width = "100%"
              ),
              textAreaInput("us_shipping_order_notes", "订单备注:", placeholder = "请输入订单备注", width = "100%", height = "80px"),
              actionButton("clear_us_shipping_bill_btn", "清空", icon = icon("trash-alt"), class = "btn-danger", style = "margin-top: 10px;", width = "100%"),
              actionButton("us_ship_order_btn", "发货", icon = icon("paper-plane"), class = "btn-success", style = "margin-top: 10px;", width = "100%")
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",
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
    "退货", icon = icon("undo"),
    div(
      class = "layout-container",
      
      # 左侧：搜索栏
      div(
        class = "sticky-sidebar",
        tags$h4("退货管理", style = "color: #007BFF; font-weight: bold; margin-bottom: 10px;"),
        textInput("return_sku_itemname", "SKU / 物品名", placeholder = "输入 SKU 或物品名", width = "100%"),
        actionButton("confirm_return_btn", "确认退货", icon = icon("undo"), class = "btn-danger", style = "width: 100%; margin-top: 10px;")
      ),
      
      div(class = "resizable-divider"),
      
      # 右侧：主面板
      div(
        class = "main-panel",
        
        # 物品信息卡片
        div(
          class = "card shadow-sm",
          style = "padding: 15px; margin-bottom: 20px; border: 1px solid #FF5733; border-radius: 8px;",
          tags$h4("退货物品信息", style = "color: #FF5733; font-weight: bold; text-align: left; margin-bottom: 10px;"),
          div(
            style = "display: flex; align-items: center;",
            div(style = "flex: 1; text-align: center; padding-right: 20px;", uiOutput("return_item_image")),
            div(style = "flex: 2;", uiOutput("return_item_info"))
          )
        ),
        
        # 订单信息卡片
        div(
          class = "card shadow-sm",
          style = "padding: 15px; border: 1px solid #007BFF; border-radius: 8px;",
          tags$h4("关联订单信息", style = "color: #007BFF; font-weight: bold; text-align: left; margin-bottom: 10px;"),
          div(
            style = "display: flex; align-items: center;",
            div(style = "flex: 1; text-align: center; padding-right: 20px;", uiOutput("return_order_image")),
            div(style = "flex: 2;", uiOutput("return_order_info"))
          )
        )
      )
    )
  ), # End of "退货"
  
  tabPanel(
    "订单管理", icon = icon("clipboard-list"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #28A745; border-radius: 8px;",
          tags$h4("订单筛选", style = "color: #28A745; font-weight: bold;"),
          textInput("filter_order_id", "订单号", placeholder = "输入订单号", width = "100%"),
          textInput("filter_tracking_id", "运单号", placeholder = "输入运单号", width = "100%"),
          fluidRow(
            column(6, textInput("filter_customer_name", "顾客姓名", placeholder = "输入顾客姓名", width = "100%")),
            column(6, textInput("filter_customer_netname", "顾客网名", placeholder = "输入顾客网名", width = "100%"))
          ),
          fluidRow(
            column(6, selectInput("filter_platform", "电商平台", c("所有平台" = "", "Etsy", "Shopify", "TikTok", "其他"), selected = "", width = "100%")),
            column(6, selectInput("filter_order_status", "订单状态", c("所有状态" = "", "备货", "预定", "调货", "装箱", "发出", "在途", "送达"), selected = "", width = "100%"))
          ),
          fluidRow(
            column(6, textInput("filter_sku", "SKU反查", placeholder = "输入SKU", width = "100%")),
            column(6, autocompleteInputUI("sold", label = "商品名反查", placeholder = "输入商品名"))
          ),
          fluidRow(
            column(4, actionButton("delete_order_btn", "删除", class = "btn-danger", style = "width: 100%;")),
            column(4, actionButton("reset_filter_btn", "清空", class = "btn-info", style = "width: 100%;")),
            column(4, actionButton("refresh_orders", "刷新", class = "btn-secondary", style = "width: 100%;"))
          )
        ),
        tags$hr(style = "margin: 5px 0; border: none;"),
        div(
          style = "margin-top: 5px; display: flex; justify-content: center;", 
          actionButton("merge_order_btn", "合并订单", icon = icon("object-group"), class = "btn-primary", style = "font-size: 16px; width: 100%; height: 42px;")
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        tabsetPanel(
          id = "order_management_tabs",
          type = "pills",
          tabPanel(
            "已经到齐",
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;",
              orderTableUI("orders_table_arrived")
            )
          ),
          tabPanel(
            "没有到齐",
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;",
              orderTableUI("orders_table_waiting")
            )
          ),
          tabPanel(
            "需要调货",
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;",
              orderTableUI("orders_relocation")
            )
          ),
          tabPanel(
            "订单查询",
            div(
              class = "card",
              style = "height: 460px; padding: 5px; border: 1px solid #ccc; border-radius: 8px;",
              orderTableUI("orders_table_module")
            ),
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;",
              uiOutput("associated_items_title"),
              uiOutput("order_items_cards")
            )
          )
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
        itemFilterUI(id = "manage_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = TRUE),
        
        tags$hr(), # 分隔线
        
        # 添加 TabsetPanel 组织不同功能
        tabsetPanel(
          id = "manage_tabs",
          type = "pills",
          tabPanel(
            "更新图片", icon = icon("image"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              # 添加说明
              tags$p("请点选一行（一种商品）进行图片更新。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              imageModuleUI("image_manage", label = "更新商品图片"),
              actionButton("update_image_btn", "更新图片", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")
            )
          ),
          tabPanel(
            "更新信息", icon = icon("edit"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 添加说明
              tags$p("请点选一行或多行进行信息更新。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              fluidRow(
                column(12, numericInput("update_product_cost", "修改单价", value = NULL, min = 0, width = "100%")),
                column(12, numericInput("update_shipping_cost", "修改国内运费", value = NULL, min = 0, width = "100%")),
                column(12, dateInput("update_purchase_date", "修改采购日期", value = Sys.Date(), width = "100%"))
              ),
              fluidRow(
                column(6, actionButton("update_info_btn", "更新信息", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")),
                column(6, actionButton("clear_info_btn", "清空", icon = icon("eraser"), style = "background-color: #8B0000; color: white; width: 100%;"))
              )
            )
          ),
          tabPanel(
            "删除", icon = icon("trash"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 添加说明
              tags$p("请点选一行或多行物品，支持批量删除。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              actionButton(
                "confirm_delete_btn",
                "确认删除",
                icon = icon("check"),
                class = "btn-primary",
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",
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
        itemFilterUI(id = "defect_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
        
        tags$hr(), # 分隔线
        
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
      
      div(
        class = "resizable-divider",
      ),
      
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
                     use_sold_date = TRUE, use_exit_date = TRUE, use_status = FALSE,
                     border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        tabsetPanel(
          id = "intl_shipment_tabs",
          type = "pills",
          
          # 第一个 Tab：登记国际运单
          tabPanel(
            title = tagList(icon("file-alt"), "登记国际运单"),
            value = "register_shipment",
            div(
              class = "card shadow-sm",
              style = "padding: 10px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              
              # Card 标题
              tags$h4("登记国际运单", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              
              # 输入内容
              fluidRow(
                column(12, textInput("intl_tracking_number", "国际运单号:", placeholder = "请输入空运或海运运单号", width = "100%")),
                column(12, textOutput("intl_status_display"), style = "color: blue; font-weight: bold; margin-bottom: 20px;"),
                column(12, selectInput("intl_shipping_method", "国际运输方式:", choices = c("空运" = "空运", "海运" = "海运"), selected = "空运", width = "100%")),
                column(12, numericInput("intl_total_shipping_cost", "国际物流总运费 (元):", value = 0, min = 0, width = "100%"))
              ),
              
              # 按钮
              fluidRow(
                column(6, actionButton("register_shipment_btn", "登记运单", icon = icon("save"), class = "btn-primary", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
                column(6, actionButton("batch_value_btn", "包裹货值", icon = icon("dollar-sign"), class = "btn-success", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
              ),
              fluidRow(
                column(6, actionButton("delete_shipment_btn", "删除运单", icon = icon("trash"), class = "btn-danger", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
                column(6, actionButton("clean_shipment_btn", "清空填写", icon = icon("trash"), class = "btn-info", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
              )
            )
          ),
          
          # 挂靠管理
          tabPanel(
            title = tagList(icon("link"), "挂靠管理"),
            value = "link_management",  # 添加唯一标识值
            div(
              class = "card shadow-sm",
              style = "padding: 10px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              
              # Card 标题
              tags$h4("挂靠管理", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
              
              fluidRow(
                column(12, textInput("intl_link_tracking_number", "", placeholder = "请输入要挂靠的运单号", width = "100%")),
                column(12, htmlOutput("intl_link_display"), style = "color: blue; font-weight: bold; margin-bottom: 20px;")
              ),
              
              # 挂靠和解除挂靠按钮
              fluidRow(
                column(6, actionButton("link_tracking_btn", "挂靠运单", icon = icon("link"), class = "btn-primary", style = "margin-top: 20px; width: 100%;", disabled = TRUE)),
                column(6, actionButton("unlink_tracking_btn", "解除挂靠", icon = icon("link-slash"), class = "btn-danger", style = "margin-top: 20px; width: 100%;", disabled = TRUE))
              )
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",  # 用于调整宽度的分隔条
        style = "cursor: ew-resize; background-color: #ccc; width: 5px; flex-shrink: 0;"
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        
        div(
          id = "item_table_container_logistics",
          uniqueItemsTableUI("unique_items_table_logistics")
        )
      )
    )
  ), # end of 国际物流管理 tab

  tabPanel(
    "账务核查", icon = icon("wallet"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        style = "width: 480px;",
        h4("算法备忘", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
        p("投入总金额 = 初始资金(82445.90) + 美元转账（换算人民币）总计"),
        p("实际总金额 = 现金流 + 工资 + 公司税费 + 公司杂费 + 新货值与运费"),
        p("现金流 = 四卡总余额 - 公司债务 - 社保")
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        fluidRow(
          column(12, div(
            class = "card shadow-lg",
            style = "background: orange; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("对账差额 (投入总金额 - 实际总金额)", style = "font-weight: bold; font-size: 20px; margin-bottom: 10px;"),
            tags$h3(textOutput("reconciliation_difference"), style = "font-size: 24px; font-weight: bold; color: #FF0000; margin-bottom: 0;")
          ))
        ),
        fluidRow(
          column(6, div(
            class = "card shadow-lg",
            style = "background: yellow; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("投入总金额", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("total_investment"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(6, div(
            class = "card shadow-lg",
            style = "background: yellow; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("实际总金额", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("actual_total"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          ))
        ),
        fluidRow(
          column(3, div(
            class = "card shadow-lg",
            style = "background: #9be0a4; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("现金流", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("cash_flow"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(3, div(
            class = "card shadow-lg",
            style = "background: #9be0a4; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("工资", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("salary"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(3, div(
            class = "card shadow-lg",
            style = "background: #9be0a4; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("公司税费", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("company_tax"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(3, div(
            class = "card shadow-lg",
            style = "background: #9be0a4; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("公司杂费", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("company_expenses"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          ))
        ),
        fluidRow(
          column(6, div(
            class = "card shadow-lg",
            style = "background: #e0e0e0; color: black; padding: 15px; border-radius: 16px; margin-top: 20px;",
            tags$h4("旧货值与运费 （2024年12月23日前）", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
            tags$div(
              style = "display: flex; justify-content: space-around; align-items: center; margin-bottom: 20px;",
              tags$div(
                style = "text-align: center;",
                tags$p("总货值:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
                tags$h3(textOutput("before_20241223_total_value"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
              ),
              tags$div(
                style = "text-align: center;",
                tags$p("总运费:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
                tags$h3(textOutput("before_20241223_total_shipping"), style = "font-size: 24px; font-weight: bold; color: #28A745; margin-bottom: 0;")
              )
            ),
            tags$div(
              style = "border-top: 1px solid #CCC; padding-top: 20px; display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; text-align: center;",
              tags$div(
                tags$h5("国内", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("before_20241223_domestic_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("before_20241223_domestic_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("在途", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("before_20241223_logistics_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("before_20241223_logistics_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("美国", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("before_20241223_us_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("before_20241223_us_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("售出", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("before_20241223_sold_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("before_20241223_sold_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              )
            )
          )),
          column(6, div(
            class = "card shadow-lg",
            style = "background: #d8eaf5; color: black; padding: 15px; border-radius: 16px; margin-top: 20px;",
            tags$h4("新货值与运费 （2024年12月23日后）", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
            tags$div(
              style = "display: flex; justify-content: space-around; align-items: center; margin-bottom: 20px;",
              tags$div(
                style = "text-align: center;",
                tags$p("总货值:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
                tags$h3(textOutput("after_20241223_total_value"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
              ),
              tags$div(
                style = "text-align: center;",
                tags$p("总运费:", style = "font-size: 18px; font-weight: bold; margin-bottom: 5px;"),
                tags$h3(textOutput("after_20241223_total_shipping"), style = "font-size: 24px; font-weight: bold; color: #28A745; margin-bottom: 0;")
              )
            ),
            tags$div(
              style = "border-top: 1px solid #CCC; padding-top: 20px; display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; text-align: center;",
              tags$div(
                tags$h5("国内", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_domestic_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_domestic_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("在途", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_logistics_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_logistics_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("美国", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_us_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_us_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("售出", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_sold_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_sold_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              )
            )
          ))
        ),
        fluidRow(
          column(6, div(
            class = "card shadow-lg",
            style = "background: #e0e0e0; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("公司债务", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("company_liabilities"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(6, div(
            class = "card shadow-lg",
            style = "background: #e0e0e0; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("社保(初始资金4618)", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("social_security"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          ))
        )
      )
    )
  ), # End of 账务管理
  
  tabPanel(
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
        
        tags$hr(),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
        ),
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #DC3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("售罄物品", style = "color: #DC3545; font-weight: bold; margin-bottom: 15px;"),
          radioButtons(
            inputId = "query_stock_status",
            label = NULL,  # 不显示默认标题，使用 h4 作为标题
            choices = c("不过滤" = "none", "美国售罄, 国内有货" = "us", "国内售罄, 美国有货" = "domestic", "全库存售罄" = "all"),
            selected = "none",  # 默认选择 “不过滤”
            inline = FALSE
          )
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        # 使用 tabsetPanel 来组织分页
        tabsetPanel(
          id = "query_tabs",
          type = "pills",
          tabPanel(
            "商品状态",
            fluidRow(
              column(
                5,
                div(
                  class = "card",
                  style = "height: 373px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品信息", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                  uiOutput("query_item_info") # 动态渲染物品信息
                )
              ),
              
              column(
                4,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                  plotlyOutput("inventory_status_chart", height = "320px") # 使用 plotlyOutput
                )
              ),
              
              column(
                3,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("瑕疵情况", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                  plotlyOutput("defect_status_chart", height = "320px") # 使用 plotlyOutput
                )
              )
            ),
            
            div(
              style = "display: flex; flex-direction: column;",
              div(
                style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
                
                div(
                  id = "context-menu",
                  style = "display: none; position: absolute; background: white; border: 1px solid #ccc; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); padding: 5px; border-radius: 5px; z-index: 1000;",
                  actionButton("query_purchase_request", "采购请求", class = "btn btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
                  uiOutput("query_outbound_request_btn")  # 动态生成出库请求按钮                
                ),
                
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
                    column(3,                   
                           dateRangeInput(
                             "time_range",
                             label = "选择采购时间范围",
                             start = Sys.Date() - 30, # 默认最近30天
                             end = Sys.Date() + 1
                           )),
                    column(3,
                           radioButtons(
                             "precision",
                             label = "选择统计精度",
                             choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"),
                             selected = "天",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(5,
                           radioButtons(
                             "expense_type",
                             label = "选择显示内容",
                             choices = c("成本+国内运费" = "cost_domestic", "成本" = "cost", "国内运费" = "domestic_shipping", "国际运费" = "intl_shipping", "总开销" = "total"),
                             selected = "cost_domestic",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(1,
                           actionButton(
                             "reset_time_range",
                             label = "",
                             icon = icon("redo"), # 添加一个重置图标
                             class = "btn-warning", # 设置按钮样式
                             style = "height: 50px; font-size: 14px;" # 设置样式
                           ))
                  ),
                  
                  # 图表行：柱状图 + 饼图
                  fluidRow(
                    column(9, plotlyOutput("expense_chart", height = "350px")), # 80% 宽度柱状图
                    column(3, plotlyOutput("pie_chart", height = "350px"))  # 20% 宽度饼图
                  ),
                  uniqueItemsTableUI("expense_details_table") # 物品详情表
                )
              )
            )
          ), # end of 开销汇总tab
          
          tabPanel(
            "库存总览",
            fluidRow(
              # 国内库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国内库存", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("domestic_total_count"), style = "color: #007BFF; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_total_value"), style = "color: #007BFF;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_shipping_cost"), style = "color: #007BFF;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 国际物流卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国际物流", style = "color: #28A745; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("logistics_total_count"), style = "color: #28A745; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_total_value"), style = "color: #28A745;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_shipping_cost"), style = "color: #28A745;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 美国库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #6F42C1; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("美国库存", style = "color: #6F42C1; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("us_total_count"), style = "color: #6F42C1; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_total_value"), style = "color: #6F42C1;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_shipping_cost"), style = "color: #6F42C1;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 商品售出卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #FF5733; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品售出", style = "color: #FF5733; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(
                      textOutput("sold_total_count_with_shipping"),
                      style = "color: #FF5733; font-weight: bold;"
                    ),
                    tags$p("物品总数（已投递）")
                  )
                  ,
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_total_value"), style = "color: #FF5733;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_shipping_cost"), style = "color: #FF5733;"),
                    tags$p("运输成本")
                  )
                )
              )
            ),
            
            tags$hr(style = "margin: 10px 0; border: 1px solid #ddd;"),
            
            fluidRow(
              column(
                12,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态流转桑基图", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  sankeyNetworkOutput("status_sankey", height = "345px")
                )
              )
            )
          ) # end of 库存汇总tab
        ) #end of tabsetPanel
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
            end = Sys.Date() + 1,        # 默认结束日期为今天
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
        class = "resizable-divider",
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
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        uniqueItemsTableUI("admin_items_table")  # 使用你的模组渲染物品明细表
      )
    )
  ) # End of 管理员 tab
)
