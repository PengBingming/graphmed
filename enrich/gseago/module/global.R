
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、首先确定ID列、logFC列以及物种。")),
                 tags$h6(lang$t("2、数据至少包含 ID、logFC 两列。")),
                 tags$h6(lang$t("3、运行时间较长，点击运行后请耐心等待小会儿。"))
    )
    ) )
}
