
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t('Help'),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、点击“Show Data”可查看参考数据或输入的数据。
                             其中，ID 列必须有，对应基因名，其他列可无。")),
                 tags$h6(lang$t("2、选择转换的基因类型，点击“Analyze Data”得出结果。")),
                 tags$h6(lang$t("3、可以自行设置待转换“基因”的类型（推荐），也可选择 unknown 进行自行判断。")),
                 tags$h6(lang$t("4、设置为 unknown 时基因数不宜太少。"))
    )
    ) )
  
}
