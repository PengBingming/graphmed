
library('DT')
source('module/global.R')

lang <- Translator$new(translation_csvs_path = "./lang/info/")

repsUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("exp") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(inputId = ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     downloadButton(ns("downloadSampleData"), lang$t("Sample data"))
                 ) )
      ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("Deduplication"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("result") ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns('method'),label = lang$t("method"),selected = "aver",
                                 choices = c('max'="max","average"="aver","min"="min")),
                     downloadButton(ns("downloadResultData"), lang$t('Result') )
                 )
                 
               ) # fluidRow
      ),
      tabPanel(title = "Help", helpUI("reps") )
    )
  ) # tagList 
} # function(id)
