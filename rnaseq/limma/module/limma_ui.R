
library('rhandsontable')
source('module/global.R')
# color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
#                 "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

limmaUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Exp',
               fluidRow(
                 box(title=lang$t("Expression matrix"),width=6,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("exp") ) ) ),
                 box(title=lang$t("Group information"),width=3,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("group") ) ) ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Expression matrix"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(inputId = ns("ID"),lang$t('GeneID'),c("")),
                     dropdownButton(circle=FALSE, label=lang$t("Sample data"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    downloadBttn(ns("downloadSampleExp"), lang$t("matrix"), size='sm', block=TRUE),
                                    numericInput(inputId = ns("sample_exp_row"), label = lang$t("Matrix rows"), value = 2000),
                                    downloadBttn(ns("downloadSampleGroup"), lang$t("group"), size='sm', block=TRUE) ) 
                 ) ) ),
      tabPanel(title = 'DEG',
               fluidRow(
                 box(title=lang$t("Differential genes"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("DEG") ) ) ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submitDEG"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("untrt"), lang$t("Control group"), c("") ),
                     selectInput(ns("trt")  , lang$t("Case group"), c("") ),
                     numericInput(inputId = ns("rowSum_filter"), value = 1,
                                  label = h6(lang$t("Filter low-expression genes(rowSum ≥)")) ) ,
                     selectInput(ns("method"),label = lang$t("Adjust method"),selected = "BH",
                                 choices = c("none"="none","BH"="BH","BY" ="BY","holm"="holm") ),
                     downloadButton(ns("downloadDEG"), "DEG .csv",icon = icon('download') )
                 ) ) ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Plot"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submitPlot"), label = "Show Plot",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("plot"),label = lang$t("Graphic selection"),selected = "pca",
                                 choices = c("pca"="pca",
                                             "heatmap"="heatmap",
                                             "volcano"="volcano") ),
                     conditionalPanel(
                       condition = "input.plot!=='pca'",ns = NS(id),
                       br(),dropdownButton(circle=FALSE, label=lang$t('Parameter settings'), br(),br(),
                                           fluidRow(
                                             column(width = 6,numericInput(ns("pvalue"), lang$t("pvalue"), value = 0.05) ),
                                             column(width = 6,numericInput(ns("padj")  , lang$t("padj"), value = 0.05) ),
                                             column(width = 6,numericInput(ns("logFC"),label = "logFC", value = 1) ), 
                                             column(width = 6,selectInput(ns("order"),label = lang$t("Gene sequencing"),selected = "pvalue",
                                                                          choices = c('pvalue'='pvalue',"logFC"="logFC") ) ) 
                                           ),
                                           conditionalPanel(
                                             condition = "input.plot=='heatmap'",ns = NS(id),
                                             fluidRow(
                                               column(width = 6,numericInput(ns("heatmap_num"),label = lang$t("Top genes of heatmap"), value = 50) )
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.plot=='volcano'",ns = NS(id),
                                             fluidRow(
                                               column(width = 6,numericInput(ns("volcano_num"),label = lang$t("Top labels of volcano"), value = 10) ),
                                               column(width = 6,selectInput(ns('theme'),lang$t('volcano theme'),selected = 'bw',
                                                                            choices =theme_select ) ),
                                               column(width = 6,selectInput(ns("color_down"), 'color.down', colors(),selected = 'blue' ) ),
                                               column(width = 6,selectInput(ns("color_not"), 'color.not', colors() ,selected = 'black') ),
                                               column(width = 6,selectInput(ns("color_up"), 'color.up', colors() ,'red') )
                                             )
                                           )
                       )
                     ),
                     download_plot_UI('limma')
                     )
               ) # fluidRow 
               ),
      tabPanel(title = "Help", helpUI("expr") )
      )
  ) # tagList
} # function(id)
