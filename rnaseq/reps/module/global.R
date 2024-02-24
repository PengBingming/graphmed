
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6("1、下载参考数据或点击运行可查看参考数据。"),
                 tags$h6("2、ID 列为基因名 ，不要改变列名“ID”。"),
                 tags$h6("3、去重复的方法有取平均、最大值行和最小值行，可自行选择。")
    
    )
    ) )
  
}
