
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("sample") ) ) 
      ),
      box(width = 3,status="success",
          fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
          h6('Format: .csv .xlsx .xls'),
          actionBttn( inputId = ns("show"), label = "Show Data",
                      style = "fill", color = "primary", size = "sm" ), hr(),
          dropdownButton(circle=FALSE,label=lang$t("Parameter settings"),  br(),br(),
                         numericInput(inputId = ns("Cq.Std.Dev"),label = "Cq.Std.Dev < ", value = 0.5),
                         selectInput(ns("gene") , lang$t("Internal reference"), c("") ),
                         selectInput(ns("group"), lang$t("Control sample"), c("") )
          ) ,br(),
          downloadButton(ns("downloadSampleData"), lang$t("Sample data"))
      ) )
  )
}

resultUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Result data"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), dataTableOutput(ns("results") ) ) ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit"), label = "Analyze Data",
                      style = "fill", color = "primary", size = "sm" ), hr(),
          radioButtons(ns("type"), lang$t("Result selection"), selected = "dat2",
                       choices = c('result1' = "dat2", 'result2' = "dat1") ),
          downloadButton(ns("downloadData"), lang$t("Result data")) 
      ) )
  )
}

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Express graphics"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) 
      ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit1"), label = lang$t("Start drawing"),
                      style = "fill", color = "primary", size = "sm" ),hr(),
          selectInput(ns("select"), lang$t("Target gene"), c("") ),
          dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                         br(),br() ,
                         numericInput(inputId = ns('w'),label = lang$t('plot.weight'),value = 15),
                         numericInput(inputId = ns('h'),label = lang$t('plot.high'),value = 15),
                         numericInput(inputId = ns('ppi'),label = lang$t('plot.dpi'),value = 72),
                         downloadBttn(ns("downloadplot"),  label = ".html",size='sm', block=TRUE),
                         downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
          )
      )
    ) # fluidRow
  )
}

helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，共5列，列名分别为Group、Target、Sample、Cq、Cq.Std.Dev，
                             不可改变列名，列数不可减少或增多。")),
                 tags$h6(lang$t("2、Group 为分组信息，自行添加；Target 为基因；Sample 为样本，
                              相同则为 同一样本的副孔；Cq 为 Ct 值；Cq.Std.Dev 为 Cq/Ct 的标准偏差。")),
                 tags$h6(lang$t("3、选择过滤指标、内参基因和对照组（参数设置）后运行。"))

    )
    ) )
  
}
