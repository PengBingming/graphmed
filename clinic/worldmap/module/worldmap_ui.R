
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")
source('module/global.R')

worldmapUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input Data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("table") ) )
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Input File"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     downloadButton(ns("downloadSampleData"),lang$t("Sample data"))
                 )
               )
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title = lang$t("WorldMap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     basic_plot_UI('worldmap')
                 ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     fluidRow(
                       column( width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('scale'),
                                                     choices = c("show","hide"), selected = "show") ),
                       column( width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('north.arrow'),
                                                     choices = c("show","hide") ,selected = "show" ) ),
                       column( width = 6,selectInput(inputId = ns("low") , lang$t("Low-value color"), colors() , selected = "gray60") ),
                       column( width = 6,selectInput(inputId = ns("high"), lang$t("High-value color"), colors() , selected = "red" ) ),
                       column( width = 6,numericInput(inputId = ns('size'),label = lang$t('Label font size'),value = 4     ) ),
                       column( width = 6,selectInput(inputId = ns('label_color'),label = lang$t('label.color'),choices = colors(),selected = "black") ),
                       column(width = 6,selectInput(inputId = ns("line"), lang$t("line.color"), colors() , selected = "white"   ) ),
                       column( width = 6,selectInput(ns('legend.position'),lang$t('legend.position'),selected = 'right',
                                                     choices = c( "left","top", "right" , "bottom")))
                     ),
                     download_plot_UI('worldmap')
                 )
               )
      ),
      tabPanel(title = 'Help',helpUI("worldmap") )
    ) )
}