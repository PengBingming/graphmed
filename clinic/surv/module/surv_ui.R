
# library('rhandsontable')
source('module/global.R')

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

survUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("table") ) )
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     dropdownButton(circle=FALSE, label=lang$t("Parameter settings"), br(),br(),
                                    selectInput(ns("time"),   lang$t("time"), c("") ),
                                    selectInput(ns("status"), lang$t("status"), c("") ),
                                    selectInput(ns("group"),  lang$t("group"), c("") ) ),br(),
                     downloadButton(ns("downloadSampleData"),lang$t("Sample data")),br(),br()
                 )
               ) 
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Plot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     dropdownButton(circle=FALSE, label=lang$t("Parameter settings"), br(),br(),
                                    selectInput(ns("conf.int"),  lang$t("Confidence interval"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("pval"),  lang$t("pvalue"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("risk.table"),  lang$t("risk.table"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("add.all"),  lang$t("add.all"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("median.line"),  lang$t("median.line"),selected ="hv", 
                                                choices = c("none", "hv", "h", "v") )
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("Graphic labels"), br(),br(),
                                    selectInput(ns("palette"),  lang$t("palette"),selected ="hue", 
                                                c("hue","grey","npg","aaas","lancet","jco",
                                                  "ucscgb","uchicago","simpsons","rickandmorty") ),
                                    textInput(ns('xlab'),label = lang$t('xlab'),value = "Follow up time(d)" ),
                                    textInput(ns('legend'),label = lang$t('legend'),value = "group" ) 
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata"), label = "Rdata", size='sm', block=TRUE )
                     )
                 ) ) 
      ),
      tabPanel(title = 'Cumulative curve',
               fluidRow(
                 box(title=lang$t("Plot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot1"),height = 500 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit1"), label = lang$t("Start drawing"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     dropdownButton(circle=FALSE, label=lang$t("Parameter settings"), br(),br(),
                                    selectInput(ns("conf.int1"),  lang$t("Confidence interval"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("pval1"),  lang$t("pvalue"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("risk.table1"),  lang$t("risk.table"),selected ="T", 
                                                choices = c("T","F") ),
                                    selectInput(ns("add.all1"),  lang$t("add.all"),selected ="T", 
                                                choices = c("T","F") )
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("Graphic labels"),  br(),br(),
                                    selectInput(ns("palette1"),  lang$t("palette"),selected ="hue", 
                                                c("hue","grey","npg","aaas","lancet","jco",
                                                  "ucscgb","uchicago","simpsons","rickandmorty") ),
                                    textInput(ns('xlab1'),label = lang$t('xlab'),value = "Follow up time(d)" ),
                                    textInput(ns('legend1'),label = lang$t('legend'),value = "group" ) 
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w1'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h1'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi1'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf1") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png1") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg1"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff1"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata1"), label = "Rdata", size='sm', block=TRUE )
                     )
                     
                 ) ) 
      ),
      tabPanel(title = 'Help',helpUI("sur"))
    ) ) }
