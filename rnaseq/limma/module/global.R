
download_plot_UI <- function(id){# download plot ui setting
  ns <- NS(id)
  tagList(
    hr(),
    dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                   br(),br() ,
                   numericInput(inputId  = ns('w0'),    label = lang$t("plot.weight"),value = 8),
                   numericInput(inputId  = ns('h0'),    label = lang$t("plot.high"),value = 8),
                   numericInput(inputId  = ns('ppi0'),  label = lang$t("plot.dpi"),value = 150),
                   downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("rds0"),  label = "RDS",  size='sm', block=TRUE )
    )
  )
}

helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title= lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、第一页，参考数据中，输入表达矩阵，确定基因ID列，
                                填写分组情况（condition列，如 untrt untrt untrt trt trt trt）")),
                 tags$h6(lang$t("2、第二页，选择分组的比较对象，运行得到差异结果。")),
                 tags$h6(lang$t("3、第三页，PCA、热图及火山图。"))
                 
    )
    ) )
}
