# Connect to backend database
db_connection <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "inventory_system",
    host = "localhost",
    user = "root",
    password = "goldenbeanllc",
    encoding = "utf8mb4"
  )
}

# 更新供应商下拉选项函数
update_maker_choices <- function(session, input_id, maker_data) {
  if (is.null(maker_data) || nrow(maker_data) == 0) {
    updateSelectizeInput(session, input_id, choices = NULL, server = TRUE)
  } else {
    choices <- c("", setNames(maker_data$Maker, paste0(maker_data$Maker, "(", maker_data$Pinyin, ")")))
    updateSelectizeInput(session, input_id, choices = choices, selected = "", server = TRUE)
  }
}


# Generate Code 128 barcode PDF
export_barcode_pdf <- function(sku, page_width, page_height, unit = "in") {
  # Create a temporary file path for the PDF
  temp_dir <- tempdir() 
  if (unit == "cm") {
    # 1 cm = 0.393701 in 
    page_width <- page_width / 2.54  
    page_height <- page_height / 2.54
  }
  
  if(length(unique(sku)) > 1)
    pdf_path <- paste0(temp_dir, "/multiple_barcode")  # 组合文件夹路径和文件名
  
  if(length(unique(sku)) == 1)
    pdf_path <- paste0(temp_dir, "/", unique(sku), "_", length(sku), "_barcode")
  
  custom_create_PDF(Labels = sku, 
                    name = pdf_path, 
                    type = "linear", 
                    page_width = page_width, 
                    page_height = page_height,
                    numrow = 1, 
                    numcol = 1, 
                    width_margin = 0, 
                    height_margin = 0.05)
  
  return(paste0(pdf_path, ".pdf"))
}

# Get image height width ratio
get_image_dimensions <- function(image_path) {
  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  list(width = info$width, height = info$height)
}

# Save compressed image to the server
save_compressed_image <- function(file_path, output_dir, image_name, quality = 75, max_width = 500) {
  # 验证输入
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("Invalid input file path.")
  }
  
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }
  
  tryCatch({
    # 加载图片
    img <- magick::image_read(file_path)
    
    # 获取原始宽度
    img_info <- magick::image_info(img)
    original_width <- img_info$width
    
    # 判断是否需要缩放
    if (original_width > max_width) {
      img <- magick::image_scale(img, paste0(max_width, "x"))  # 缩放图片
    }
    
    # 转为 JPEG 格式
    img <- magick::image_convert(img, format = "jpeg")
    
    # 保存图片
    output_path <- file.path(output_dir, image_name)
    magick::image_write(img, path = output_path, quality = quality)
    
    return(output_path)
  }, error = function(e) {
    showNotification(paste("图片压缩失败:", e$message), type = "error")
    return(NULL)
  })
}

# 将 Base64 编码的图片数据解码并保存为实际图片文件
base64_decode_image <- function(base64_string, output_path) {
  # 提取 Base64 数据部分（去掉头部信息，如 "data:image/png;base64,"）
  base64_data <- gsub("^data:image/[^;]+;base64,", "", base64_string)
  
  # 解码 Base64 数据为二进制文件
  decoded_image <- base64enc::base64decode(base64_data)
  
  # 写入文件
  writeBin(decoded_image, output_path)
}

render_image_preview <- function(img_src, img_info, ns) {
  renderUI({
    div(
      tags$img(src = img_src, height = "200px",
               style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
      tags$p(
        style = "color: #007BFF; font-size: 14px;",
        paste0("分辨率: ", img_info$width, "x", img_info$height,
               ", 文件大小: ", round(img_info$filesize / 1024, 2), " KB")
      ),
      actionButton(ns("clear_image_preview"), "清除图片", icon = icon("trash"), class = "btn-danger", style = "margin-top: 10px;")
    )
  })
}

render_paste_prompt <- function() {
  renderUI({
    div("将图片粘贴到这里（Ctrl+V 或 Cmd+V）",
        style = "color: #888; font-size: 16px; font-style: italic;")
  })
}

# 保存图片（文件上传或粘贴）
process_image_upload <- function(sku, file_data = NULL, pasted_data = NULL, inventory_path = NULL, output_dir = "/var/www/images") {
  if (is.null(file_data) && is.null(pasted_data)) {
    # 没有上传图片，返回库存路径或 NULL
    if (!is.null(inventory_path)) {
      showNotification("使用库存中现有图片路径。", type = "message")
      return(inventory_path)
    } else {
      showNotification("未上传图片，且库存中没有对应图片路径。", type = "warning")
      return(NA)
    }
  }
  
  # 生成唯一文件名
  unique_image_name <- paste0(sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
  final_image_path <- file.path(output_dir, unique_image_name)
  
  tryCatch({
    if (!is.null(file_data)) {
      compressed_path <- save_compressed_image(
        file_path = file_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    } else if (!is.null(pasted_data)) {
      compressed_path <- save_compressed_image(
        file_path = pasted_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    }
    
    if (!is.null(compressed_path)) {
      showNotification("图片已成功压缩并存入数据库！", type = "message")
      return(compressed_path)
    } else {
      showNotification("图片压缩存储处理失败！", type = "error")
      return(NA)
    }
  }, error = function(e) {
    showNotification("图片上传时发生错误！", type = "error")
    return(NA)
  })
}

# Generate unique code
generate_unique_code <- function(input, length = 4) {
  # Validate input
  if (is.null(input) || input == "") {
    return(NULL)  
  }
  
  # Validate length parameter
  if (!is.numeric(length) || length <= 0) {
    stop("Length must be a positive integer.")
  }
  
  # Generate a hash value
  hash_value <- digest::digest(enc2utf8(input), algo = "sha512")
  
  # Extract numeric seed from the hash
  hash_numeric <- abs(sum(utf8ToInt(hash_value))) %% .Machine$integer.max
  
  set.seed(hash_numeric)
  
  # Generate a random alphanumeric code
  random_output <- paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = "")
  return(random_output)
}

# Generate SKU
generate_sku <- function(item_type_data, major_type, minor_type, item_name, maker) {
  if (is.null(major_type) || major_type == "" || 
      is.null(minor_type) || minor_type == "" || 
      is.null(item_name) || item_name == "" || 
      is.null(maker) || maker == "") {
    return("")  # Return empty if any input is invalid
  }
  
  # Get MajorTypeSKU and MinorTypeSKU
  major_type_sku <- item_type_data %>%
    filter(MajorType == major_type) %>%
    pull(MajorTypeSKU) %>%
    unique()
  
  minor_type_sku <- item_type_data %>%
    filter(MinorType == minor_type) %>%
    pull(MinorTypeSKU) %>%
    unique()
  
  if (length(major_type_sku) == 0 || length(minor_type_sku) == 0) {
    return("")  # Return empty if no matching SKUs are found
  }
  
  # Generate unique code
  unique_code <- generate_unique_code(paste(item_name, maker, sep = "_"), length = 4)
  
  # Create the SKU in the format: MajorTypeSKU-MinorTypeSKU-UniqueCode
  paste0(major_type_sku, "-", minor_type_sku, "-", unique_code)
}

# Remove tone of letters
remove_tone <- function(text) {
  # 替换规则：音调字母 -> 无音调字母
  text <- stri_replace_all_regex(text, "ā|á|ǎ|à|a", "a")
  text <- stri_replace_all_regex(text, "ē|é|ě|è|e", "e")
  text <- stri_replace_all_regex(text, "ī|í|ǐ|ì|i", "i")
  text <- stri_replace_all_regex(text, "ō|ó|ǒ|ò|o", "o")
  text <- stri_replace_all_regex(text, "ū|ú|ǔ|ù|u", "u")
  text <- stri_replace_all_regex(text, "ǖ|ǘ|ǚ|ǜ|ü", "u")
  text <- stri_replace_all_regex(text, "Ā|Á|Ǎ|À|A", "A")
  text <- stri_replace_all_regex(text, "Ē|É|Ě|È|E", "E")
  text <- stri_replace_all_regex(text, "Ī|Í|Ǐ|Ì|I", "I")
  text <- stri_replace_all_regex(text, "Ō|Ó|Ǒ|Ò|O", "O")
  text <- stri_replace_all_regex(text, "Ū|Ú|Ǔ|Ù|U", "U")
  text <- stri_replace_all_regex(text, "Ǖ|Ǘ|Ǚ|Ǜ|Ü", "U")
  return(text)
}

# Define an empty inventory template
create_empty_inventory <- function() {
  data.frame(
    SKU = character(),            # Product SKU
    Maker = character(),          # Supplier
    MajorType = character(),      # Major category
    MinorType = character(),      # Minor category
    ItemName = character(),       # Item name
    Quantity = numeric(),         # Quantity in stock
    ProductCost = numeric(),      # Product Cost
    ShippingCost = numeric(),     # Shipping Cost
    ItemImagePath = character(),  # Path to item image
    stringsAsFactors = FALSE      # Avoid factor columns
  )
}

# 空的箱子与货架
create_empty_shelf_box <- function() {
  data.frame(
    SKU = character(),
    UniqueID = character(),
    ItemName = character(),
    ProductCost = numeric(),
    ItemImagePath = character(),
    stringsAsFactors = FALSE
  )
}

# Map column names and filter only mapped columns
map_column_names <- function(data, column_mapping) {
  # Get the mapped columns in the order of column_mapping
  mapped_columns <- names(column_mapping)[names(column_mapping) %in% names(data)]
  
  # If no matching columns, return an empty data frame
  if (length(mapped_columns) == 0) {
    return(data.frame())
  }
  
  # Select and reorder columns in the order of column_mapping
  data <- data[, mapped_columns, drop = FALSE]
  
  # Rename columns according to column_mapping
  data <- setNames(data, column_mapping[mapped_columns])
  
  return(data)
}

# Function to render the image column (local images with public URL prefix)
render_image_column <- function(image_column_data, 
                                host_url = host_url, 
                                placeholder = placeholder_50px_path) {
  
  sapply(image_column_data, function(img) {
    if (is.na(img) || img == "") {
      # 返回占位符图片
      paste0('<img src="', placeholder, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    } else {
      # 拼接完整的图片 URL
      img_path <- paste0(host_url, "/images/", basename(img))
      paste0('<img src="', img_path, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    }
  }, USE.NAMES = FALSE)
}

# Function to render datatable with images
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     selection = "single",
                                     image_column = NULL,
                                     options = list(scrollY = "770px",  # 根据内容动态调整滚动高度
                                                    scrollX = TRUE,  # 支持水平滚动
                                                    fixedHeader = TRUE,  # 启用表头固定
                                                    dom = 't',  # 隐藏搜索框和分页等控件
                                                    paging = FALSE,  # 禁止分页
                                                    searching = FALSE  # 禁止搜索
                                     )) {
  if (!is.null(image_column) && nrow(data) > 0) {
    # Render the image column
    data[[image_column]] <- render_image_column(data[[image_column]], host_url)
  }
  
  # Map column names for user-friendly display
  if (!is.null(column_mapping)) {
    data <- map_column_names(data, column_mapping)
  }
  
  # 获取更新后的列名
  updated_column_names <- colnames(data)
  
  # 返回列表，包括 datatable 对象和列名
  list(
    datatable = datatable(
      data,
      escape = FALSE,  # Disable HTML escaping to allow rendering of images
      selection = selection,
      rownames = FALSE,
      options = options
    ),
    column_names = updated_column_names
  )
}



update_status <- function(con, unique_id, new_status = NULL, defect_status = NULL, 
                          shipping_method = NULL, clear_shipping_method = FALSE, 
                          refresh_trigger = NULL, update_timestamp = TRUE, 
                          clear_status_timestamp = NULL) {
  # 检查 UniqueID 是否存在
  current_status <- dbGetQuery(con, paste0(
    "SELECT Status, updated_at FROM unique_items WHERE UniqueID = '", unique_id, "'"
  ))
  
  if (nrow(current_status) == 0) {
    showNotification("UniqueID not found", type = "error")
    return()
  }
  
  # 如果 new_status 发生变化，记录状态历史
  if (!is.null(new_status) && current_status$Status != new_status) {
    dbExecute(con, paste0(
      "INSERT INTO item_status_history (UniqueID, previous_status, previous_status_timestamp) VALUES ('",
      unique_id, "', '", current_status$Status, "', '", current_status$updated_at, "')"
    ))
  }
  
  # 构建动态 SQL 子句
  set_clauses <- c(
    if (!is.null(new_status)) {
      # 检查 new_status 的合法性
      if (!new_status %in% names(status_columns)) {
        showNotification("Invalid status provided", type = "error")
        return()
      }
      # 获取时间戳列
      timestamp_column <- status_columns[[new_status]]
      timestamp_update <- if (update_timestamp) paste0(timestamp_column, " = NOW()") else NULL
      c("Status = ?", timestamp_update)
    } else {
      NULL
    },
    if (!is.null(defect_status)) "Defect = ?" else NULL,
    if (!is.null(shipping_method)) "IntlShippingMethod = ?" else NULL,
    if (clear_shipping_method) "IntlShippingMethod = NULL" else NULL, # 显式清空运输方式
    if (!is.null(clear_status_timestamp)) {
      # 检查 clear_status_timestamp 的合法性
      if (!clear_status_timestamp %in% names(status_columns)) {
        showNotification("Invalid clear_status_timestamp provided", type = "error")
        return()
      }
      paste0(status_columns[[clear_status_timestamp]], " = NULL") # 清空指定列
    } else {
      NULL
    }
  )
  
  # 拼接 SET 子句
  set_clause <- paste(set_clauses[!is.null(set_clauses)], collapse = ", ")
  
  # 如果没有任何更新内容，提示错误并返回
  if (set_clause == "") {
    showNotification("No updates provided", type = "error")
    return()
  }
  
  # 构建 SQL 查询
  query <- paste0(
    "UPDATE unique_items SET ", set_clause, " WHERE UniqueID = ?"
  )
  
  # 构建参数列表
  params <- c(
    if (!is.null(new_status)) list(new_status) else NULL,
    if (!is.null(defect_status)) list(defect_status) else NULL,
    if (!is.null(shipping_method)) list(shipping_method) else NULL,
    list(unique_id)  # 唯一 ID 是必须的
  )
  
  # 展平参数列表
  params <- unlist(params)
  
  # 执行 SQL 更新
  dbExecute(con, query, params = params)
  
  # 触发刷新
  if (!is.null(refresh_trigger)) {
    refresh_trigger(!refresh_trigger())
  }
}


update_order_id <- function(con, unique_id, order_id) {
  tryCatch({
    if (is.null(order_id)) {
      # 构造 SQL 更新语句，清空 OrderID
      query <- "UPDATE unique_items SET OrderID = NULL WHERE UniqueID = ?"
      params <- list(unique_id)
      
      # 执行 SQL 更新
      dbExecute(con, query, params = params)
      
      # 成功提示
      showNotification("订单号已成功清空！", type = "message")
    } else if (order_id == "") {
      showNotification("订单号不能为空字符串！", type = "error")
      return()
    } else {
      # 构造 SQL 更新语句
      query <- "UPDATE unique_items SET OrderID = ? WHERE UniqueID = ?"
      params <- list(order_id, unique_id)
      
      # 执行 SQL 更新
      dbExecute(con, query, params = params)
      
      # 成功提示
      showNotification("订单号已成功更新！", type = "message")
    }
  }, error = function(e) {
    # 错误提示
    showNotification(paste("更新订单号时发生错误：", e$message), type = "error")
  })
}

# 定义确认框
deleteConfirmationModal <- function(item_count) {
  modalDialog(
    title = "确认删除",
    paste0("您已选择 ", item_count, " 件物品。这些物品删除后将无法恢复。是否继续？"),
    footer = tagList(
      modalButton("取消"),
      actionButton("confirm_delete_final", "确认删除", class = "btn-danger")
    )
  )
}

fetchSkuData <- function(sku, con) {
  query <- "
    SELECT 
      ItemName,
      Maker,
      MajorType,
      MinorType,
      Quantity AS TotalQuantity,
      ProductCost AS AverageCost,
      ShippingCost AS AverageShippingCost,
      ItemImagePath
    FROM inventory
    WHERE SKU = ?"
  dbGetQuery(con, query, params = list(sku))
}


fetchInventoryStatusData <- function(sku, con) {
  query <- "
    SELECT 
      Status, 
      COUNT(*) AS Count
    FROM unique_items
    WHERE SKU = ?
    GROUP BY Status"
  dbGetQuery(con, query, params = list(sku))
}


fetchDefectStatusData <- function(sku, con) {
  query <- "
    SELECT 
      Defect, 
      COUNT(*) AS Count
    FROM unique_items
    WHERE SKU = ?
    GROUP BY Defect"
  dbGetQuery(con, query, params = list(sku))
}


fetchSkuOperationData <- function(sku, con) {
  # 查询 SKU 的基本信息和相关状态数据
  query <- "
    SELECT 
      inv.ItemImagePath,
      inv.ItemName,
      inv.Maker,
      inv.MajorType,
      inv.MinorType,
      inv.Quantity AS TotalQuantity, -- 总库存数量
      SUM(CASE WHEN u.Status = '国内出库' THEN 1 ELSE 0 END) AS PendingQuantity -- 待入库数
    FROM inventory AS inv
    LEFT JOIN unique_items AS u
      ON inv.SKU = u.SKU
    WHERE inv.SKU = ?
    GROUP BY inv.ItemImagePath, inv.ItemName, inv.Maker, inv.MajorType, inv.MinorType, inv.Quantity
  "
  
  # 执行查询并返回结果
  dbGetQuery(con, query, params = list(sku))
}


plotBarChart <- function(data, x, y, x_label, y_label, colors) {
  # 检查数据是否为空
  if (nrow(data) == 0 || is.null(data[[y]]) || length(data[[y]]) == 0) {
    return(plotly::plot_ly(type = "scatter", mode = "text") %>%
             plotly::add_text(x = 0.5, y = 0.5, text = "无库存状态数据", textfont = list(size = 20, color = "red")))
  }
  
  # 使用 plotly 绘制柱状图
  plotly::plot_ly(
    data = data,
    x = ~get(x),
    y = ~get(y),
    type = "bar",
    marker = list(color = colors[seq_along(data[[y]])]) # 设置颜色
  ) %>%
    plotly::layout(
      xaxis = list(title = x_label),
      yaxis = list(title = y_label),
      title = "状态分布",
      showlegend = FALSE
    )
}


plotPieChart <- function(data, labels, values, colors) {
  # 检查数据是否为空
  if (nrow(data) == 0 || is.null(data[[values]]) || length(data[[values]]) == 0) {
    return(plotly::plot_ly(type = "scatter", mode = "text") %>%
             plotly::add_text(x = 0.5, y = 0.5, text = "无瑕疵情况数据", textfont = list(size = 20, color = "red")))
  }
  
  # 使用 plotly 绘制饼图
  plotly::plot_ly(
    data = data,
    labels = ~get(labels),
    values = ~get(values),
    type = "pie",
    textinfo = "label+percent",
    marker = list(colors = colors[seq_along(data[[values]])]) # 设置颜色
  ) %>%
    plotly::layout(
      title = "瑕疵情况分布",
      showlegend = TRUE
    )
}


renderItemInfo <- function(output, output_name, item_info, img_path, count_label = "待入库数", count_field = "PendingQuantity") {
  # 如果 item_info 为空或没有数据，构造一个默认空数据框
  if (is.null(item_info) || nrow(item_info) == 0) {
    item_info <- data.frame(
      ItemName = "",
      Maker = "",
      MajorType = "",
      MinorType = "",
      PendingQuantity = 0,
      AvailableForOutbound = 0,
      stringsAsFactors = FALSE
    )
  }
  
  # 动态获取数量值
  count_value <- item_info[[count_field]][1]
  
  # 动态渲染 UI
  output[[output_name]] <- renderUI({
    fluidRow(
      column(
        4,
        div(
          style = "text-align: center;",
          img(
            src = img_path,
            height = "300px",
            style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"
          )
        )
      ),
      column(
        8,
        div(
          style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 8px;
                             box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); height: 300px;",
          tags$h4(
            "商品信息",
            style = "border-bottom: 3px solid #4CAF50; margin-bottom: 15px; padding-bottom: 8px; font-weight: bold; color: #333;"
          ),
          tags$table(
            style = "width: 100%; font-size: 16px; color: #444;",
            tags$tr(
              tags$td(tags$strong("商品名:"), style = "padding: 8px 10px; width: 120px; vertical-align: top;"),
              tags$td(tags$span(item_info$ItemName[1], style = "color: #4CAF50; font-weight: bold;"))
            ),
            tags$tr(
              tags$td(tags$strong("供应商:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$Maker[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong("大类:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$MajorType[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong("小类:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$MinorType[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong(count_label), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(
                ifelse(count_value == 0, paste0("无", count_label), count_value),
                style = "color: #FF4500; font-weight: bold;"
              ))
            )
          )
        )
      )
    )
  })
}

handleSkuInput <- function(
    sku_input,        # SKU 输入值
    output_name,      # 输出 UI 名称
    count_label,      # 显示的计数标签
    count_field,      # 数据字段名称
    con,              # 数据库连接
    output,           # 输出对象
    placeholder_path, # 默认占位图片路径
    host_url          # 图片主机 URL
) {
  sku <- trimws(sku_input) # 清理空格
  
  if (is.null(sku) || sku == "") {
    # 如果 SKU 为空，渲染默认空的商品信息
    shinyjs::delay(5000, {
      renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
    })
    return()
  }
  
  tryCatch({
    # 查询 SKU 数据
    item_info <- fetchSkuOperationData(sku, con)
    
    # 如果未找到记录
    if (nrow(item_info) == 0) {
      showNotification("未找到该条形码对应的物品！", type = "error")
      renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
      return()
    }
    
    # 渲染商品信息
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(
        is.na(item_info$ItemImagePath[1]),
        placeholder_path,
        paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
      ),
      count_label = count_label,
      count_field = count_field
    )
    
    # 返回 count_field 的值
    return(item_info[[count_field]][1])
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste("处理 SKU 输入时发生错误：", e$message), type = "error")
  })
}

handleOperation <- function(
    unique_items_data,       # 数据集
    operation_name,          # 操作名称（如 "入库", "出库"）
    sku_field,               # SKU 字段的 input 名称
    output_name,             # 输出的 UI 名称
    query_status,            # 查询的初始状态
    update_status_value,     # 更新后的状态
    count_label,             # 计数标签
    count_field,             # 计数字段名称
    refresh_trigger,         # 数据刷新触发器
    con,                     # 数据库连接
    input, output, session,  # Shiny 的输入、输出和会话对象
    clear_field = NULL,      # 需要清空的字段
    clear_shipping_method = FALSE
) {
  sku <- trimws(input[[sku_field]])
  
  if (is.null(sku) || sku == "") {
    showNotification("请先扫描 SKU！", type = "error")
    shinyjs::delay(5000, {
      renderItemInfo(output, output_name, NULL, placeholder_300px_path, count_label, count_field)
    })   
    return()
  }
  
  # 查询符合条件的物品
  sku_items <- unique_items_data %>%
    filter(SKU == sku, Status == query_status, Defect != "瑕疵") %>%
    arrange(ProductCost) %>%
    slice_head(n = 1)
  
  # 特殊情况：仅当操作为“入库”且状态为“国内出库”时检查
  if (operation_name == "入库" && query_status == "国内出库" && nrow(sku_items) == 0) {
    # 查询是否存在调货状态的物品
    transfer_items <- unique_items_data %>%
      filter(SKU == sku, Status == "美国调货", Defect != "瑕疵")
    
    if (nrow(transfer_items) > 0) {
      # 提取物品名称、订单号、运单号和图片路径
      item_name <- transfer_items$ItemName[1] %||% "未知商品"
      order_ids <- transfer_items$OrderID %>% unique()
      intl_tracking <- transfer_items$IntlTracking %>% unique()
      img_path <- transfer_items$ItemImagePath[1] %||% placeholder_300px_path
      
      # 弹窗提示用户
      showModal(modalDialog(
        title = paste0("调货提示 - SKU: ", sku, " - ", item_name),
        div(
          style = "display: flex; align-items: center; padding: 10px;",
          div(
            style = "flex: 1; text-align: center; margin-right: 20px;",
            tags$img(
              src = ifelse(is.na(img_path), placeholder_300px_path, paste0(host_url, "/images/", basename(img_path))),
              alt = "物品图片",
              style = "max-width: 150px; max-height: 150px; border: 1px solid #ddd; border-radius: 8px;"
            )
          ),
          div(
            style = "flex: 2;",
            tags$p("当前物品在入库前已被调货，请优先处理以下订单："),
            tags$b("物品名称："), item_name, tags$br(),
            tags$b("关联订单号："),
            lapply(order_ids, function(order_id) {
              div(
                style = "display: flex; align-items: center; margin-bottom: 5px;",
                tags$span(order_id, style = "flex: 1;"),
              )
            })
          )
        ),
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
      return()
    }
  }
  
  # 如果仍无符合条件的物品
  if (nrow(sku_items) == 0) {
    showNotification(paste0("无可", operation_name, "的物品，所有该商品已完成 ", operation_name, "！"), type = "message")
    return()
  }
    
  tryCatch({
    unique_id <- sku_items$UniqueID[1]
      
    # 动态设置瑕疵状态
    defect_status <- if (operation_name == "入库" && !is.null(input$defective_item)) {
      ifelse(isTRUE(input$defective_item), "瑕疵", "无瑕")
    } else NULL
    
    # 动态设置运输方式
    shipping_method <- switch(
      operation_name,
      "撤回" = NULL,
      "出库" = input$outbound_shipping_method,
      "售出" = input$sold_shipping_method,
      NULL
    )
    
    # 动态清空字段
    if (!is.null(clear_field)) {
      dbExecute(con, paste0("UPDATE unique_items SET ", clear_field, " = NULL WHERE UniqueID = ?"), params = list(unique_id))
    }
    
    # 调用更新状态函数
    update_status(
      con = con,
      unique_id = unique_id,
      new_status = update_status_value,
      defect_status = defect_status,
      shipping_method = shipping_method,
      clear_shipping_method = clear_shipping_method,
      refresh_trigger = refresh_trigger
    )
    
    # 成功提示
    showNotification(paste0("物品成功", operation_name, "！"), type = "message")
    
    # 查询 SKU 数据并刷新 UI
    item_info <- fetchSkuOperationData(sku, con)
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(is.na(item_info$ItemImagePath[1]), placeholder_300px_path, paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))),
      count_label = count_label,
      count_field = count_field
    )
    
    # 如果计数字段为 0，显示模态弹窗
    if (item_info[[count_field]][1] == 0) {
      showModal(modalDialog(
        title = paste0(operation_name, "完成"),
        paste0("此 SKU 的商品已全部完成 ", operation_name, "！"),
        easyClose = TRUE,
        footer = NULL
      ))
      shinyjs::delay(2000, removeModal())
    }
    
    # 重置输入框和控件
    updateTextInput(session, paste0(operation_name, "_sku"), value = "")
    if (operation_name == "入库") updateCheckboxInput(session, "defective_item", value = FALSE)
    
    return(unique_id)
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste0(operation_name, "失败：", e$message), type = "error")
  })
}

# 添加瑕疵备注
add_defective_note <- function(con, unique_id, note_content, status_label = "瑕疵", refresh_trigger = NULL) {
  # 获取当前日期并格式化
  current_date <- format(Sys.Date(), "%Y-%m-%d", tz = "Asia/Shanghai")
  
  # 为备注添加时间戳和状态标记
  new_note <- paste0("[", status_label, " ", current_date, "] ", note_content)
  
  # 查询现有备注
  current_notes <- dbGetQuery(
    con,
    "SELECT DefectNotes FROM unique_items WHERE UniqueID = ?",
    params = list(unique_id)
  )
  
  # 拼接备注
  if (nrow(current_notes) > 0 && !is.na(current_notes$DefectNotes[1])) {
    updated_notes <- paste(current_notes$DefectNotes[1], new_note, sep = "; ")
  } else {
    updated_notes <- new_note
  }
  
  # 更新数据库中的备注
  dbExecute(
    con,
    "UPDATE unique_items SET DefectNotes = ? WHERE UniqueID = ?",
    params = list(updated_notes, unique_id)
  )
  
  # 触发刷新机制
  if (!is.null(refresh_trigger)) {
    refresh_trigger(!refresh_trigger())
  }
}


apply_dynamic_styles <- function(table, column_names) {
  # 库存态样式
  if ("库存态" %in% column_names) {
    table <- table %>%
      formatStyle(
        "库存态",
        backgroundColor = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国售出", "美国发货", "退货"),
          c("lightgray", "#c7e89b", "#9ca695", "#46a80d", "#6f52ff", "#529aff", "#869bb8", "#faf0d4", "red")
        ),
        color = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国售出", "美国发货", "退货"),
          c("black", "black", "black", "white", "white", "black", "black", "black", "white")
        )
      )
  }
  
  # 瑕疵态样式
  if ("瑕疵态" %in% column_names) {
    table <- table %>%
      formatStyle(
        "瑕疵态",
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
  
  # 国际运输样式
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
  
  return(table)
}

# 订单注册与更新
register_order <- function(order_id, customer_name, customer_netname, platform, order_notes, tracking_number, 
                           image_data, con, orders, box_items, unique_items_data, 
                           is_transfer_order = NULL, is_preorder = NULL, preorder_supplier = NULL) {
  tryCatch({
    # 将 NULL 的 is_transfer_order 和 is_preorder 设置为默认值 FALSE
    is_transfer_order <- is_transfer_order %||% FALSE
    is_preorder <- is_preorder %||% FALSE
    
    # 查询是否已有相同订单号的记录
    existing_order <- orders() %>% filter(OrderID == order_id)
    
    # 初始化订单图片路径
    order_image_path <- NULL
    
    # 确定订单状态
    order_status <- "备货"  # 默认状态
    if (is_transfer_order && !is_preorder) {
      order_status <- "调货"
    } else if (is_preorder && !is_transfer_order) {
      order_status <- "预定"
    }
    
    # 检查发货箱中的物品是否含有“美国入库”状态
    if (nrow(box_items()) > 0) {
      # 如果 box_items() 中包含 "国内出库" 或 "美国入库"，设置状态为 "调货"
      if (any(box_items()$Status %in% c("国内出库", "美国入库"))) {
        order_status <- "调货"
      }
    }
    
    # 确认运单 PDF 文件的状态
    label_status <- if (!is.null(tracking_number)) {
      if (file.exists(file.path("/var/uploads/shiplabels", paste0(tracking_number, ".pdf")))) {
        "上传"
      } else {
        "无"
      }
    } else {
      "无"
    }
    
    # 如果状态为 "调货" 且 LabelStatus 为 "无"，显示错误通知
    if (order_status == "调货" && label_status == "无") {
      showNotification("调货订单必须上传运单 PDF！", type = "error")
      return(FALSE)
    }
    
    # 如果为预订单，生成或更新供应商备注
    if (is_preorder && !is.null(preorder_supplier)) {
      supplier_prefix <- "【供应商】"
      new_supplier_note <- paste0(supplier_prefix, preorder_supplier, "；")
      
      if (nrow(existing_order) > 0) {
        existing_notes <- existing_order$OrderNotes[1] %||% ""  # 确保为长度为 1 的字符
        if (grepl(supplier_prefix, existing_notes)) {
          order_notes <- gsub(
            paste0(supplier_prefix, ".*?；"),
            new_supplier_note,
            existing_notes
          )
        } else {
          order_notes <- paste0(new_supplier_note, existing_notes)
        }
      } else {
        order_notes <- paste0(new_supplier_note, order_notes %||% "")
      }
    }
    
    # 获取发货箱中的物品图片路径
    box_data <- box_items()
    box_image_paths <- if (nrow(box_data) > 0) {
      box_data$ItemImagePath[!is.na(box_data$ItemImagePath)]
    } else {
      character(0)  # 如果为空，返回空字符向量
    }
    
    # 获取订单内关联物品的图片路径
    order_items <- unique_items_data() %>% filter(OrderID == order_id)
    order_image_paths <- if (nrow(order_items) > 0) {
      order_items$ItemImagePath[!is.na(order_items$ItemImagePath)]
    } else {
      character(0)  # 如果为空，返回空字符向量
    }
    
    # 获取订单中的图片路径作为 inventory_path
    order_image_path <- if (nrow(existing_order) > 0) {
      existing_order$OrderImagePath[1]  # 提取 OrderImagePath
    } else {
      NULL
    }
    
    # 处理 image_data 数据
    image_path <- process_image_upload(
      sku = order_id,
      file_data = image_data$uploaded_file(),
      pasted_data = image_data$pasted_file(),
      inventory_path = order_image_path
    )
    
    # 合并订单关联物品和发货箱的图片路径
    combined_image_paths <- unique(c(order_image_paths, box_image_paths))
    
    # 决定订单图片路径
    if (!is.na(image_path)) {
      # 如果用户上传或粘贴了图片，直接使用用户的图片路径
      order_image_path <- image_path
    } else {
      # 如果没有用户上传或粘贴的图片，使用库存中的图片路径和发货箱的图片路径生成拼贴图
      combined_image_paths <- unique(c(order_image_paths, box_image_paths))
      if (length(combined_image_paths) > 0) {
        montage_path <- paste0("/var/www/images/", order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        order_image_path <- generate_montage(combined_image_paths, montage_path)
      } else {
        order_image_path <- NA  # 确保为长度为 1 的 NA
      }
    }
    
    # 确保所有参数为长度为 1 的值
    tracking_number <- tracking_number %||% NA
    order_notes <- order_notes %||% NA
    customer_name <- customer_name %||% NA
    customer_netname <- customer_netname %||% NA
    order_image_path <- as.character(order_image_path %||% NA)
    platform <- platform %||% ""
    
    # 插入或更新订单
    if (nrow(existing_order) > 0) {
      dbExecute(con, "
        UPDATE orders 
        SET OrderImagePath = COALESCE(?, OrderImagePath), 
            UsTrackingNumber = COALESCE(?, UsTrackingNumber), 
            OrderNotes = COALESCE(?, OrderNotes),
            CustomerName = COALESCE(?, CustomerName),
            CustomerNetName = COALESCE(?, CustomerNetName),
            Platform = COALESCE(?, Platform),
            OrderStatus = ?,
            LabelStatus = ?
        WHERE OrderID = ?",
                params = list(
                  order_image_path,
                  tracking_number,
                  order_notes,
                  customer_name,
                  customer_netname,
                  platform,
                  order_status,
                  label_status,
                  order_id
                )
      )
      showNotification("订单信息已更新！", type = "message")
    } else {
      dbExecute(con, "
        INSERT INTO orders (OrderID, UsTrackingNumber, OrderNotes, CustomerName, CustomerNetName, Platform, OrderImagePath, OrderStatus, LabelStatus)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                  order_id,
                  tracking_number,
                  order_notes,
                  customer_name,
                  customer_netname,
                  platform,
                  order_image_path,
                  order_status,
                  label_status
                )
      )
      showNotification("订单已成功登记！", type = "message")
    }
    return(TRUE)
  }, error = function(e) {
    showNotification(paste("登记订单时发生错误：", e$message), type = "error")
    return(FALSE)
  })
}

# 更新运单PDF状态列
update_label_status_column <- function(con, pdf_directory = "/var/uploads/shiplabels") {
  # 列出所有 PDF 文件
  existing_files <- list.files(pdf_directory, full.names = FALSE)
  existing_tracking_numbers <- gsub("\\.pdf$", "", existing_files)  # 提取运单号
  
  tryCatch({
    # 动态生成 CASE 语句
    case_statements <- paste0(
      "CASE
        WHEN UsTrackingNumber IS NULL OR UsTrackingNumber = '' THEN '无'
        WHEN UsTrackingNumber IS NOT NULL AND UsTrackingNumber IN (",
      if (length(existing_tracking_numbers) > 0) {
        paste(sprintf("'%s'", existing_tracking_numbers), collapse = ",")
      } else {
        "''"
      },
      ") THEN
          CASE
            WHEN LabelStatus = '印出' THEN '已传'
            ELSE '已传'
          END
        ELSE '无'
      END"
    )
    
    # 构建 SQL 更新语句
    update_query <- paste0(
      "UPDATE orders
       SET LabelStatus = ", case_statements
    )
    
    # 执行 SQL 更新
    dbExecute(con, update_query)
    message("LabelStatus 列已更新成功。")
  }, error = function(e) {
    stop(paste("更新 LabelStatus 列时发生错误：", e$message))
  })
}


# 动态生成 input 命名空间
get_input_id <- function(base_id, suffix) {
  # 提取 "XXX" 的前缀部分
  prefix <- strsplit(base_id, "-")[[1]][1]
  # 拼接完整 ID
  paste0(prefix, "-", suffix)
}


# 从输入数据中筛选数据
filter_unique_items_data_by_inputs <- function(
    data, 
    input, 
    maker_input_id, 
    status_input_id,
    item_name_input_id, 
    purchase_date_range_id = NULL, 
    sold_date_range_id = NULL,
    exit_date_range_id = NULL
) {
  req(data)  # 确保数据不为空
  
  # 按供应商筛选
  if (!is.null(input[[maker_input_id]]) && length(input[[maker_input_id]]) > 0 && any(input[[maker_input_id]] != "")) {
    data <- data %>% filter(Maker %in% input[[maker_input_id]])
  }
  
  # 按库存状态筛选
  if (!is.null(input[[status_input_id]]) && length(input[[status_input_id]]) > 0 && any(input[[status_input_id]] != "")) {
    data <- data %>% filter(Status %in% input[[status_input_id]])
  }
  
  # 按商品名称筛选
  if (!is.null(input[[item_name_input_id]]) && input[[item_name_input_id]] != "") {
    data <- data %>% filter(ItemName == input[[item_name_input_id]])
  }
  
  # 按采购日期筛选
  if (!is.null(purchase_date_range_id) && !is.null(input[[purchase_date_range_id]]) && length(input[[purchase_date_range_id]]) == 2) {
    purchase_date_range <- as.Date(input[[purchase_date_range_id]])
    data <- data %>% filter(as.Date(PurchaseTime) >= purchase_date_range[1], as.Date(PurchaseTime) <= purchase_date_range[2])
  }
  
  # 按售出日期筛选，仅对库存状态为‘国内售出’的物品有效
  if (!is.null(sold_date_range_id) && 
      !is.null(input[[sold_date_range_id]]) && 
      length(input[[sold_date_range_id]]) == 2) {
    
    only_show_sold_id <- get_input_id(sold_date_range_id, "only_show_sold")
    
    if (!is.null(input[[only_show_sold_id]]) && input[[only_show_sold_id]]) {
      sold_date_range <- as.Date(input[[sold_date_range_id]])
      data <- data %>%
        filter(Status == "国内售出", # 仅对状态为‘国内售出’的记录
               as.Date(DomesticSoldTime) >= sold_date_range[1], 
               as.Date(DomesticSoldTime) <= sold_date_range[2]) %>%
        select(-DomesticExitTime) # 去掉“国内出库”列
    }
  }
  
  # 按出库日期筛选，仅对库存状态为‘国内出库’的物品有效
  if (!is.null(exit_date_range_id) && 
      !is.null(input[[exit_date_range_id]]) && 
      length(input[[exit_date_range_id]]) == 2) {
    
    only_show_exit_id <- get_input_id(exit_date_range_id, "only_show_exit")
    
    if (!is.null(input[[only_show_exit_id]]) && input[[only_show_exit_id]]) {
      exit_date_range <- as.Date(input[[exit_date_range_id]])
      data <- data %>%
        filter(Status == "国内出库", # 仅对状态为‘国内出库’的记录
               as.Date(DomesticExitTime) >= exit_date_range[1], 
               as.Date(DomesticExitTime) <= exit_date_range[2]) %>%
        select(-DomesticSoldTime) # 去掉“国内售出”列
    }
  }
  
  data
}



adjust_inventory_quantity <- function(con, sku, adjustment) {
  tryCatch({
    sku <- trimws(sku)  # 清理空格
    # 查询现有库存
    existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
    
    if (nrow(existing_item) > 0) {
      # SKU 存在，计算新的库存
      new_quantity <- existing_item$Quantity + adjustment
      
      # 库存不足的校验（仅在减少库存时检查）
      if (adjustment < 0 && new_quantity < 0) {
        showNotification("库存不足，无法完成操作！", type = "error")
        return(FALSE)
      }
      
      # 更新库存数量到数据库
      dbExecute(con, "UPDATE inventory SET Quantity = ? WHERE SKU = ?", params = list(new_quantity, sku))
      
      showNotification(paste("库存已成功调整! SKU:", sku), type = "message")
      return(TRUE)
    } else {
      showNotification(paste("SKU 不存在，无法调整库存！SKU:", sku), type = "error")
      return(FALSE)
    }
  }, error = function(e) {
    showNotification(paste("调整库存时发生错误：", e$message), type = "error")
    return(FALSE)
  })
}

# 动态拼接图片函数（接近正方形布局）
generate_montage <- function(image_paths, output_path, geometry = "+5+5") {
  if (length(image_paths) == 0) {
    showNotification("无图片可供拼贴！", type = "error")
  }
  
  # 计算行列数，保持接近正方形
  n_images <- length(image_paths)
  n_cols <- ceiling(sqrt(n_images))
  n_rows <- ceiling(n_images / n_cols)
  tile <- paste0(n_rows, "x", n_cols)  # 拼接行列数，例如 "3x3"
  
  # 加载所有图片
  images <- lapply(image_paths, magick::image_read)
  
  # 拼接图片
  montage <- magick::image_montage(
    do.call(c, images),
    tile = tile,      # 动态行列布局
    geometry = geometry # 图片间距
  )
  
  montage <- magick::image_convert(montage, format = "jpeg")
  
  # 保存拼接图片
  magick::image_write(montage, path = output_path)
  
  return(output_path)
}

reset_order_form <- function(session, image_module) {
  updateTextInput(session, "order_id", value = "")
  updateSelectInput(session, "platform", selected = "")
  updateTextInput(session, "customer_name", value = "")
  updateTextInput(session, "customer_netname", value = "")
  updateCheckboxInput(session, "is_preorder", value = FALSE)
  updateCheckboxInput(session, "is_transfer_order", value = FALSE)
  updateTextInput(session, "tracking_number", value = "")
  image_module$reset()
  updateTextAreaInput(session, "order_notes", value = "")
}



createSearchableDropdown <- function(input_id, label, data, placeholder = "搜索...") {
  # 将数据转换为 Dropdown 所需格式
  options <- if (length(data) > 0) {
    lapply(data, function(item) list(key = item, text = item))
  } else {
    # 提供默认值避免空选项
    list(list(key = "no-data", text = "无数据"))
  }
  
  # 定义 DropdownMenuItemType
  DropdownMenuItemType <- function(type) {
    JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType.", type))
  }
  
  # 包含搜索框的选项列表
  options_with_search <- function(opt) {
    filter_header <- list(
      key = "__FilterHeader__", 
      text = "-", 
      itemType = DropdownMenuItemType("Header") # 添加一个 Header 类型选项用于搜索框
    )
    append(list(filter_header), opt)
  }
  
  # 定义模糊搜索渲染逻辑
  render_search_box <- JS(paste0("(option) => {
    if (option.key !== '__FilterHeader__') {
      return option.text;
    }
    const onChange = (event, newValue) => {
      const query = newValue.toLocaleLowerCase();
      const checkboxLabels = document.querySelectorAll(
        '#", input_id, "-list .ms-Checkbox-label'
      );
      checkboxLabels.forEach(label => {
        const text = label.innerText.replace('\\n', '').replace('', '').toLocaleLowerCase();
        // 使用 indexOf 实现模糊匹配
        if (query === '' || text.indexOf(query) !== -1) {
          label.parentElement.style.display = 'flex';
        } else {
          label.parentElement.style.display = 'none';
        }
      });
    };
    const props = { 
      placeholder: '", placeholder, "', 
      underlined: true, 
      onChange 
    };
    return React.createElement(jsmodule['@fluentui/react'].SearchBox, props);
  }"))
  
  # 定义样式，控制下拉菜单的高度
  dropdown_styles <- JS("{
    callout: {
      maxHeight: '200px', // 设置下拉菜单最大高度
      overflowY: 'auto'   // 启用垂直滚动条
    }
  }")
  
  # 返回创建的 UI
  div(
    style = "padding-bottom: 15px;", # 外层 div 设置内边距和字体大小
    Dropdown.shinyInput(
      inputId = input_id,
      label = label,
      options = options_with_search(options), # 添加搜索框到选项列表中
      multiSelect = TRUE,
      placeholder = placeholder,
      onRenderOption = render_search_box, # 自定义渲染逻辑
      styles = dropdown_styles            # 应用样式控制下拉菜单高度
    )
  )
}


renderOrderInfo <- function(output, output_name, matching_orders, clickable = TRUE) {
  # 如果没有物品，返回提示信息
  if (is.null(matching_orders) || nrow(matching_orders) == 0) {
    output[[output_name]] <- renderUI({
      div("没有找到匹配的订单")
    })
    return()
  }
  
  output[[output_name]] <- renderUI({
    # 动态渲染订单卡片
    order_cards <- lapply(1:nrow(matching_orders), function(i) {
      order_info <- matching_orders[i, ]
      
      # 图片路径
      img_path <- ifelse(
        is.na(order_info$OrderImagePath) || order_info$OrderImagePath == "",
        placeholder_300px_path,
        paste0(host_url, "/images/", basename(order_info$OrderImagePath))
      )
      
      # 动态添加蒙版和打勾图标
      mask_overlay <- if (order_info$OrderStatus == "装箱") {
        div(
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; 
                  background: rgba(128, 128, 128, 0.6); display: flex; justify-content: center; align-items: center;
                  border-radius: 8px; z-index: 2;",
          tags$div(
            style = "width: 50px; height: 50px; background: #28a745; border-radius: 50%; display: flex; 
                     justify-content: center; align-items: center;",
            tags$i(class = "fas fa-check", style = "color: white; font-size: 24px;")  # 绿色勾
          )
        )
      } else {
        NULL
      }
      
      # 如果卡片不可点击，不设置 onclick 事件
      onclick_script <- if (clickable) {
        sprintf("Shiny.setInputValue('selected_order_id', '%s', {priority: 'event'})", order_info$OrderID)
      } else {
        NULL
      }
      
      # 渲染订单卡片
      div(
        id = paste0("order_card_", order_info$OrderID),  # 设置唯一 ID
        class = "order-card",
        style = paste0(
          "position: relative; display: inline-block; width: 550px; height: 310px; ",
          "background-color: #ffffff; border: 1px solid #ddd; border-radius: 8px; ",
          "box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); margin-right: 15px; ",
          ifelse(clickable, "cursor: pointer;", "cursor: default;")  # 动态设置鼠标样式
        ),
        
        onclick = onclick_script,  # 动态设置点击事件
        
        mask_overlay,  # 动态显示蒙版
        
        # 卡片内容
        div(
          style = "display: flex; gap: 10px; height: 100%;",
          
          # 图片部分
          div(
            style = "flex: 1; text-align: center; display: flex; align-items: center; justify-content: center;",
            img(
              src = img_path,
              style = "height: 280px; max-width: 100%; object-fit: cover; border-radius: 8px;"
            )
          ),
          
          # 订单信息部分
          div(
            style = "flex: 1; overflow-y: auto; padding: 10px;",
            tags$table(
              style = "width: 100%; font-size: 14px; color: #444;",
              tags$tr(
                tags$td(tags$strong("订单号:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$OrderID, style = "color: #007BFF; font-weight: bold;"))
              ),
              tags$tr(
                tags$td(tags$strong("运单号:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$UsTrackingNumber, style = "color: #007BFF; font-weight: bold;"))
              ),
              tags$tr(
                tags$td(tags$strong("顾客姓名:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$CustomerName, style = "color: #007BFF;"))
              ),
              tags$tr(
                tags$td(tags$strong("平台:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$Platform, style = "color: #007BFF;"))
              ),
              tags$tr(
                tags$td(tags$strong("备注:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(
                  div(
                    style = "color: #FF0000; background-color: #FFFFCC; padding: 6px; white-space: normal; word-wrap: break-word; font-size: 14px; border-radius: 4px;",
                    order_info$OrderNotes
                  )
                )
              ),
              tags$tr(
                tags$td(tags$strong("状态:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$OrderStatus, style = "color: #007BFF;"))
              )
            )
          )
        )
      )
    })
    
    div(
      style = "display: flex; gap: 15px; flex-wrap: nowrap; overflow-x: auto; padding: 10px;",
      do.call(tagList, order_cards)  # 返回卡片列表
    )
  })
}

renderOrderItems <- function(output, output_name, order_items, deletable = FALSE) {
  # 如果没有物品，返回提示信息
  if (is.null(order_items) || nrow(order_items) == 0) {
    output[[output_name]] <- renderUI({
      div("没有找到订单内物品")
    })
    return()
  }
  
  # 动态渲染物品卡片
  output[[output_name]] <- renderUI({
    item_cards <- lapply(1:nrow(order_items), function(i) {
      item <- order_items[i, ]
      
      # 图片路径
      item_img_path <- ifelse(
        is.na(item$ItemImagePath) || item$ItemImagePath == "",
        placeholder_150px_path,
        paste0(host_url, "/images/", basename(item$ItemImagePath))
      )
      
      # 动态添加蒙版和打勾图标
      mask_overlay <- if (item$Status == "美国发货") {
        div(
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; 
                  background: rgba(128, 128, 128, 0.6); display: flex; justify-content: center; align-items: center;
                  border-radius: 8px; z-index: 2;",
          tags$div(
            style = "width: 50px; height: 50px; background: #28a745; border-radius: 50%; display: flex; 
                     justify-content: center; align-items: center;",
            tags$i(class = "fas fa-check", style = "color: white; font-size: 24px;")  # 绿色勾
          )
        )
      } else {
        NULL
      }
      
      # 渲染卡片
      div(
        id = paste0("card_", i),  # 设置唯一 ID
        class = "card",
        style = "position: relative; display: inline-block; padding: 10px; width: 230px; text-align: center; 
                 border: 1px solid #ddd; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
        
        mask_overlay,  # 动态显示蒙版
        
        div(
          style = "margin-bottom: 10px; position: relative;",
          tags$img(
            src = item_img_path,
            style = "height: 150px; object-fit: cover; border-radius: 8px;"  # 图片高度固定为150px
          ),
          if (deletable) {
            tags$button(
              class = "btn btn-danger btn-sm delete-btn",
              type = "button",
              style = "position: absolute; top: 5px; right: 5px;",
              onclick = sprintf("Shiny.setInputValue('delete_card', '%s', {priority: 'event'})", item$UniqueID),
              tags$i(class = "fas fa-trash-alt")
            )
          } else {
            NULL
          }
        ),
        
        tags$table(
          style = "width: 100%; font-size: 12px; color: #333;",
          tags$tr(
            tags$td(tags$strong("SKU:"), style = "padding: 0px; width: 60px;"),
            tags$td(item$SKU)
          ),
          tags$tr(
            tags$td(tags$strong("商品名:"), style = "padding: 0px;"),
            tags$td(item$ItemName)
          ),
          tags$tr(
            tags$td(tags$strong("状态:"), style = "padding: 0px;"),
            tags$td(
              paste0(
                item$Status,
                " (", ifelse(is.na(item$IntlTracking) || item$IntlTracking == "", "未邮寄", "已邮寄"), ")"
              )
            )
          ),
          tags$tr(
            tags$td(tags$strong("瑕疵态:"), style = "padding: 0px;"),
            tags$td(ifelse(is.na(item$Defect), "无", item$Defect))  # 显示瑕疵状态
          ),
          tags$tr(
            tags$td(tags$strong("瑕疵注:"), style = "padding: 0px;"),
            tags$td(ifelse(is.na(item$DefectNotes) || item$DefectNotes == "", "无备注", item$DefectNotes))  # 显示瑕疵备注
          )
        )
      )
    })
    
    do.call(tagList, item_cards)  # 返回卡片列表
  })
}

generate_order_id <- function(tracking_number, unique_ids) {
  # 验证 tracking_number 是否有效
  if (is.null(tracking_number) || tracking_number == "" || !is.character(tracking_number)) {
    stop("无效的运单号：tracking_number 必须是非空字符串。")
  }
  
  # 验证 unique_ids 是否为非空向量
  if (is.null(unique_ids) || length(unique_ids) == 0 || any(unique_ids == "")) {
    stop("无效的 unique_ids：必须是非空字符串向量。")
  }
  
  # 将运单号和所有 UniqueID 拼接成字符串
  input_string <- paste0(tracking_number, paste(unique_ids, collapse = ""))
  
  # 生成 SHA-256 哈希值
  hashed <- digest::digest(input_string, algo = "sha256", serialize = FALSE)
  
  # 截取前九位作为订单 ID
  order_id <- toupper(substr(hashed, 1, 9))
  
  return(order_id)
}


match_tracking_number <- function(data, tracking_number_column, input_tracking_id) {
  # 清理输入运单号
  cleaned_tracking_id <- gsub("[^0-9]", "", trimws(input_tracking_id))
  
  # 初次精准匹配
  matched_data <- data %>%
    filter(
      !is.na(.data[[tracking_number_column]]) &
        .data[[tracking_number_column]] != "" &
        .data[[tracking_number_column]] == cleaned_tracking_id
    )
  
  # 如果首次匹配为空且输入运单号长度超过 22，则截取后 22 位进行二次匹配
  if (nrow(matched_data) == 0 && nchar(cleaned_tracking_id) > 22) {
    trimmed_tracking_id <- substr(cleaned_tracking_id, 9, nchar(cleaned_tracking_id))
    matched_data <- data %>%
      filter(
        !is.na(.data[[tracking_number_column]]) &
          .data[[tracking_number_column]] != "" &
          .data[[tracking_number_column]] == trimmed_tracking_id
      )
  }
  
  return(matched_data)
}


# 清理未被记录的图片 (每天运行一次)
clean_untracked_images <- function() {
  # 数据库连接信息
  con <- db_connection()
  
  tryCatch({
    # 1. 获取数据库中记录的图片路径（包括 inventory 和 orders 表）
    inventory_query <- "SELECT ItemImagePath FROM inventory WHERE ItemImagePath IS NOT NULL"
    orders_query <- "SELECT OrderImagePath FROM orders WHERE OrderImagePath IS NOT NULL"
    
    inventory_paths <- normalizePath(dbGetQuery(con, inventory_query)$ItemImagePath, mustWork = FALSE)
    orders_paths <- normalizePath(dbGetQuery(con, orders_query)$OrderImagePath, mustWork = FALSE)
    
    # 合并所有记录路径
    recorded_paths <- unique(c(inventory_paths, orders_paths))
    
    # 2. 列出目录中所有图片文件，并规范化路径
    all_files <- normalizePath(list.files("/var/www/images/", full.names = TRUE), mustWork = FALSE)
    
    # 3. 检查哪些文件未被记录
    untracked_files <- setdiff(all_files, recorded_paths)
    
    # 4. 删除未被记录的文件
    if (length(untracked_files) > 0) {
      sapply(untracked_files, file.remove)
      message("以下文件已被删除：")
      print(untracked_files)
    } else {
      message("没有未被记录的文件需要清理。")
    }
  }, error = function(e) {
    message("清理过程中出现错误：", e$message)
  })
  
  # 断开数据库连接
  dbDisconnect(con)
}

