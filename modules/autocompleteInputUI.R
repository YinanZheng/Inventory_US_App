autocompleteInputUI <- function(id, label = "商品名：", placeholder = "请输入商品名...") {
  ns <- NS(id)
  tagList(
    div(
      style = "position: relative; width: 100%;",
      div(
        style = "position: absolute; top: 50%; left: 10px; transform: translateY(17%); color: grey; 
               font-size: 15px; pointer-events: none; white-space: nowrap; overflow: hidden;",
        id = ns("item_hint")  # 动态 ID，确保模块实例唯一
      ),
      textInput(
        inputId = ns("item_name"),  # 动态 ID，确保模块实例唯一
        label = label,
        placeholder = placeholder,
        width = "100%"
      )
    ),
    # JavaScript 部分：监听 Tab 键并触发补全
    tags$script(HTML(sprintf("
      $(document).on('keydown', function(e) {
        if (e.key === 'Tab' && $('#%s').is(':focus')) {
          const item_hint = $('#%s').text();
          if (item_hint.length > 0) {
            const currentValue = $('#%s').val();
            $('#%s').val(currentValue + item_hint);  // 补全输入框
            Shiny.setInputValue('%s', currentValue + item_hint, {priority: 'event'});  // 提交补全值
            $('#%s').text('');  // 清空提示
            e.preventDefault();  // 阻止默认 Tab 行为
          }
        }
      });", ns("item_name"), ns("item_hint"), ns("item_name"), ns("item_name"), ns("item_name"), ns("item_hint")))
    )
  )
}
