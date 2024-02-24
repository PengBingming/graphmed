
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"),rHandsontableOutput(ns("table") ) ) ) ,
      box(width=3,status="success",
          fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
          h6('Format: .csv .xlsx .xls'),
          actionBttn( inputId = ns("show"), label = "Show Data",
                      style = "fill", color = "primary", size = "sm" ),
          hr(),
          checkboxInput(inputId =ns("rep"), label = lang$t("Subbore (with/without)"),value = T ),
          downloadButton(ns("downloadSampleData"), lang$t("Sample data")) 
      )
    )
  )
}

resultUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Result data"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"),rHandsontableOutput(ns('results') ) ) ),
      box(width=3,status="success",
          actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                      style = "fill", color = "primary", size = "sm" ),hr(),
          
          selectInput(inputId = ns("type"), label = lang$t("Fitting mode"), selected = "binomial",
                      choices = c('Binomial' = "binomial",  '4p-logistic' = "logistic",'Linear'= "line") ),
          numericInput(ns("round"), lang$t("Significant digits"), min=0,max = 10,value = 3 ),
          downloadButton(ns("downloadData"), lang$t("Result data") )
      ) )
  )
} 

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Fitting plot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
          splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height=500) ) ),
      box(width=3,status="success",
          selectInput(ns('theme'),lang$t('Theme'),selected = 'linedraw',
                      choices = theme_select ),
          dropdownButton(circle=FALSE, label=lang$t("download plot"),status="success",icon = icon("download"),
                         br(),br(),
                         column(width = 12,numericInput(ns("w"), label = lang$t("plot.weight"), value = 8) ),
                         column(width = 12,numericInput(ns("h"), label = lang$t("plot.high"), value = 8) ),
                         column(width = 12,numericInput(ns("ppi"), label = lang$t("plot.dpi"), value = 72) ),
                         downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("rdata"), label = "Rdata", size='sm', block=TRUE ) ) )
    )
  )
}

helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 h2("使用说明"),
                 hr(),
                 h6("1、参考数据中，x 为小写 x ，y 为小写 y ，分别对应标曲
                         浓度 与 OD 值，其余列为检测样本 OD。"),
                 h6("2、如果标曲有复孔，需要自行求平均或者选择一条进行拟合。"),
                 h6("3、未提供去除背景的功能，需要去除背景的请自行减去。"),
                 hr(),
                 h6("1. In the reference data, x is lowercase x, y is lowercase y, 
                    corresponding to the concentration and OD value of the standard 
                    curve,and the other columns are the OD of the test sample."),
                 h6("2. If the standard curve has two rows of holes，you need to 
                    average it or choose one for fitting."),
                 h6("3. The function of removing the background is not provided, 
                    please subtract the background if necessary.")
                 
    )
    ) )
}
