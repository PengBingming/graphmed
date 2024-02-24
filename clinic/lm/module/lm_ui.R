
library('rhandsontable')
source('module/global.R')

# # File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

lm_dataUI <- function(id){
  ns <- NS(id)
  # tagList(
    fluidRow(
      box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white" ,
          splitLayout(cellWidths = c("60%"), rHandsontableOutput(ns("table") ) ) ),
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
          downloadButton(ns("downloadtable"),lang$t("Sample data") )  ) 
      )
  # )
}

lm_plotUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Fitting Plot"),width=9,solidHeader=TRUE,status='primary',background = "white",
          basic_plot_UI('lm')
          ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit"), label = "Analyze Data", 
                      style = "fill", color = "primary", size = "sm" ),hr(),
          fluidRow(
            column(width = 6, selectInput(ns("x"), label = 'x', c("") ) ),
            column(width = 6, selectInput(ns("y"), label = 'y', c("") ) ),
            column(width = 6, selectInput(ns("color"), label = 'color', c("") ) ),
            column(width = 6, selectInput(ns("fill"),  label = 'fill' , c("") ) )
          ),
          br(),
          dropdownButton( label = lang$t("Parameter settings"),icon = icon('image'),circle = FALSE,width = NULL,br(),br(),
                          fluidRow(
                            column( width = 6, selectInput(ns("method"), label = lang$t('Fitting mode'),
                                                           selected = 'lm', 
                                                           choices = c('loess','lm','glm','gam') ) ),
                            column( width = 6, selectInput(ns("plot"), label = lang$t('Graphic selection'),
                                                           selected = "sn",
                                                           choices = c("scatter"="s",
                                                                       "fitting"="n", 
                                                                       "merge"="sn") ) ),
                            column( width = 6, textInput(ns("formula"), label = lang$t('Fitting equation'),value = "y~x" ) ),
                            column( width = 6, selectInput(ns("se"),  label = lang$t('Confidence interval'),
                                                           choices = c('show'= 'T',
                                                                       'hide'= 'F'),
                                                           selected = 'T' ) ),
                            column( width = 6,numericInput(ns("se_level"),label = lang$t("CI level"), value = 0.95)),
                            column( width = 6, selectInput(ns("cor"), label = lang$t('Correlation'),
                                                           choices = c("pearson"="pearson", 
                                                                       "kendall"="kendall",
                                                                       "spearman"="spearman",
                                                                       "none"= F ),
                                                           selected = "pearson" ) ),
                            column( width = 6,numericInput(ns("size_plot"),label = lang$t("point.size"), value = 1) ),
                            column( width = 6,numericInput(ns("size_line"),label = lang$t("line.size"), value = 1) ),
                            column( width = 6, selectInput(ns('face'),label = lang$t('facet or not'),
                                                           choices = c('Yes'=T, "No"=F),
                                                           selected = T) )
                          )
          ),
          download_plot_UI('lm')    )
    ) # fluidRow
  )
}

lmUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', lm_dataUI("lm") ),
      tabPanel(title = 'Plot', lm_plotUI("lm") ),
      tabPanel(title = 'Help', lm_helpUI("lm") )
    )
  ) # NS(id)
} # function(id)
