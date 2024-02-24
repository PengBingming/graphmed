
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、输入数据支持“差异基因”和“差异矩阵”。")),
                 tags$h6(lang$t("2、首先确定物种和ID列。“差异矩阵”还需要需要确定 pvalue、padj、logFC 列以及对应参数。")),
                 tags$h6(lang$t("3、最后确定基因名类型，运行。若不知道基因名类型，可选择 unknown。运行时间较长，点击运行后请耐心等待小会儿。"))
    )
    ) )
  
}
