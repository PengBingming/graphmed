
# library('shinycssloaders')
# library('rhandsontable')
# library('plotly')
source('module/global.R')

lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

mapUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Map data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("60%"), rhandsontable::rHandsontableOutput(ns("table") ) ) 
                 ),
                 box(width = 3,status="success",
                     lang$t("Data sources: "),a("GeoAtlas,v3", href = "http://datav.aliyun.com/portal/school/atlas/area_selector"),
                     hr(),
                     fileInput(ns("file"),label = lang$t("Input file"), multiple = FALSE),
                     actionBttn(ns("show"), label = "Show Data",style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     selectInput(ns('labels'),lang$t('labels'),selected = 'province',
                                 choices = c("fullname","province","english","city","none")),
                     hr(),
                     downloadButton(ns("downloadSampleData"),lang$t("Sample data"))
                 )
                 
               ) ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title = lang$t("ChinaMap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     basic_plot_UI('map')
                 ),
                 box(width=3,status="success",
                     actionBttn(ns("submit"), label = lang$t("Start drawing"),style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     fluidRow(
                       column( width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('scale'),
                                                     choices = c("show","hide"), selected = "show") ),
                       column( width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('north.arrow'),
                                                     choices = c("show","hide") ,selected = "show" ) ),
                       column( width = 6,selectInput(inputId = ns("low") , lang$t("Low-value color"), colors() , selected = "white") ),
                       column( width = 6,selectInput(inputId = ns("high"), lang$t("High-value color"), colors() , selected = "red" ) ),
                       column( width = 6,numericInput(inputId = ns('size'),label = lang$t('Label font size'),value = 4     ) ),
                       column( width = 6,selectInput(inputId = ns('label_color'),label = lang$t('label.color'),choices = colors(),selected = "black") ),
                       column( width = 6,selectInput(ns('legend.position'),lang$t('legend.position'),selected = 'right',
                                                    choices = c( "left","top", "right" , "bottom")))
                     ),
                     download_plot_UI('map') )
               )
      ),
      tabPanel(title = 'Plotly',
               fluidRow(
                 box(title = lang$t("ChinaMap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"), plotly::plotlyOutput(ns("plot1"),height = 600 )
                     ) 
                 ),
                 box(width=3,status="success",
                     actionBttn(ns("submitPlotly"), label = lang$t("Start drawing"),style = "fill", 
                                color = "primary", size = "sm" ),hr(),
                     downloadButton(ns("downloadplot"),  label = ".html")
                 )
               )
      ),
      tabPanel(title = 'Help', map_helpUI("map") )
    ) # tabsetPanel
  ) # tagList
} # function(id)


