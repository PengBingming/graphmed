
library('rhandsontable')
source('module/global.R')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

pcaUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Data',
             fluidRow(
               box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("table") ) ) ),
               box(width = 3,status="success",
                   fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                   h6('Format: .csv .xlsx .xls'),
                   actionBttn( inputId = ns("show"), label = "Show Data",
                               style = "fill", color = "primary", size = "sm" ),
                   hr(),
                   column(width = 12,selectInput(ns('factor'),  h6(lang$t("Categorical variables"),style="color:orange"), c(""), multiple = T )),
                   column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("Ordered variables"),style="color:orange"), c(""), multiple = T )),
                   column(width = 12,selectInput(ns('numeric'), h6(lang$t("Continuous variables"),style="color:orange"), c(""), multiple = T )),
                   hr(),
                   downloadButton(ns("downloadSampleData"),lang$t("Sample data"))
               )
             )
    ),
    tabPanel(title = 'Plot',
             fluidRow(
               box(title=lang$t("Plot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                   basic_plot_UI('pca') ),
               box(width=3,status="success",
                   actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   fluidRow(
                     column(width = 12, selectInput(ns("group"), label = h6('group',style="color:orange"), c("") ) ),
                     column(width = 6, selectInput(ns("scale"), label = 'scale', c("F","T"),selected = T ) ),
                     column(width = 6, selectInput(ns("addEllipses"), label = 'addEllipses', c("F","T"),selected = "T") ),
                     column(width = 6, selectInput(ns("mean_point"), label = 'mean.point', c("F","T"),selected = "T" ) ),
                     column(width = 6, selectInput(ns("label"), label = 'label', c("none","all"),selected = "none" ) ),
                     column(width = 6, selectInput(ns("repel"), label = 'repel', c("F","T"),selected = T ) ),
                     column(width = 6, selectInput(ns("geom"), label = 'geom', c("point","text") ,selected = "point") )

                   ),
                   download_plot_UI('pca')
               )
             ) )
  ) }
