
# library('rhandsontable')
source('module/global.R')
theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

bubbleUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Data',
             fluidRow(
               box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("table") ) ) ),
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
                   basic_plot_UI("bubble") ),
               box(width=3,status="success",
                   actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   fluidRow(
                     column(width = 6,selectInput(ns("y"), h6('y: pathway',style="color:orange"), c("") ) ),
                     column(width = 6,selectInput(ns("x"), h6('x: ratio',style="color:orange"), c("") ) ),
                     column(width = 6,selectInput(ns("color"), h6('color: FDR',style="color:orange"), c("") )),
                     column(width = 6,selectInput(ns("size"), h6('size: number',style="color:orange"), c("") ) ),
                     column(width = 6,selectInput(ns("fill"), 'fill', c("") ) )
                   ),
                   # point
                   dropdownButton(circle=FALSE, label = lang$t("Parameter settings"), br(),br(),
                                  fluidRow(
                                    column(width = 6,selectInput(ns("color_lower"), 'color_lower', colors() ,selected = "red") ),
                                    column(width = 6,selectInput(ns("color_high"), 'color_high', colors(),selected = "navy") ),
                                    column(width = 6,selectInput(ns("fill_lower"), 'fill_lower', colors() , selected = "") ),
                                    column(width = 6,selectInput(ns("fill_high"), 'fill_high', colors(), selected = "") ),
                                    column(width = 6,numericInput(ns("alpha_point"), 'alpha', value = 0.8 )),
                                    column(width = 6,numericInput(ns("size_point"), 'size', value = 1 ) ),
                                    column(width = 6,numericInput(ns("shape_point"), 'shape',value = 16)),
                                    column(width = 6,selectInput(ns("show.legend_point"), 'show.legend', 
                                                                 c("T","F"),selected = 'T'  ) )
                                  )
                   ), hr(),
                   dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                  br(),br() ,
                                  numericInput(inputId = ns('w0'),label = lang$t('plot.weight'),value = 15),
                                  numericInput(inputId = ns('h0'),label = lang$t('plot.high'),value = 15),
                                  numericInput(inputId = ns('ppi0'),label = lang$t('plot.dpi'),value = 72),
                                  downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("rds0"),  label = "RDS", size='sm', block=TRUE )
                   )
               )
             ) )
  ) }
