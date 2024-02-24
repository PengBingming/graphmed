
library('rhandsontable')
source('module/global.R')

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

volcanoUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("DEG") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(inputId = ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",style = "fill",
                                 color = "primary", size = "sm" ),hr(),
                     downloadButton(ns("downloadSampleData"), lang$t("Sample data"))

                     )
               ) # fluidRow
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Volcano plot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot") ,height = 500) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("order"),label = lang$t("gene sequencing"),selected = "pvalue",
                                 choices = c('pvalue'='pvalue',"logFC"="logFC") ),
                     dropdownButton(circle=FALSE,label=lang$t("plot parameters"),  br(),br(),
                                    numericInput(ns("pvalue"), lang$t("pvalue"), value = 0.05),
                                    numericInput(ns("padj")  , lang$t("padj"), value = 0.05),
                                    numericInput(ns("logFC"),label = "logFC", value = 1),
                                    numericInput(ns("volcano_num"),label = lang$t("top labels of volcano"), value = 10)
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("graphic labels"),icon = icon('image'),  br(),br(),                     selectInput(ns('theme'),lang$t('主题'),selected = 'bw',choices = theme_select ),
                                    selectInput(ns("color_down"), 'color.down', colors(),selected = 'blue' ) ,
                                    selectInput(ns("color_not"), 'color.not', colors() ,selected = 'black') ,
                                    selectInput(ns("color_up"), 'color.up', colors() ,'red')
                                     ),
                     download_plot_UI('volcano')
               )
      )
    ),
    tabPanel(title = 'Help',helpUI("volcano")) 
  ) # bs4Dash::tabsetPanel
  )# tagList
}

