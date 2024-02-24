
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title= lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 h2("Help"),
                 hr(),
                 h6("1、点击“Show Data”可查看参考数据或输入的数据。"),
                 h6("2、对照参考数据，矩阵 “ID”列名不可改变，样本名与分组 sample 列对应。"),
                 h6("3、矩阵 ID 列“基因名”不要有重复，若存在重复，则取平均。")
    )
    ) )
  
}

download_plot_UI <- function(id){# download plot ui setting
  ns <- NS(id)
  tagList(
    hr(),
    dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                   br(),br() ,
                   numericInput(inputId  = ns('w0'),    label = lang$t("plot.weight"),value = 12),
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
