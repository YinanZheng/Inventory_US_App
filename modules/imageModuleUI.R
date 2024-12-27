imageModuleUI <- function(id, label = "商品图片上传", label_color = "#007BFF") {
  ns <- NS(id)
  
  tagList(
    # 模块的主要 UI
    tags$div(
      class = "card",
      style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px; margin-bottom: 5px;",
      tags$h5(label, style = sprintf("margin-bottom: 15px; font-weight: bold; color: %s;", label_color)),
      
      # 粘贴区域
      tags$div(
        id = ns("paste_area"), # 使用模块命名空间
        style = "border: 2px dashed #ccc; padding: 20px; text-align: center; margin-bottom: 15px; position: relative;",
        uiOutput(ns("paste_prompt")),  # 动态渲染提示文本
        uiOutput(ns("pasted_image_preview")) # 图片预览区域
      ),
      
      # 文件上传区域
      fileInput(ns("file_input"), "或拖拽/选择图片上传:", accept = c("image/png", "image/jpeg"), width = "100%")
    )
  )
}
