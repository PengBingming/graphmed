

library('rhandsontable')
source('./module/global.R')

color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

exprUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Expression matrix"),width=6,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("exp") ) ) 
                 ),
                 box(title=lang$t("Group information"),width=3,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("group") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Expression matrix"),multiple = FALSE ),
                     fileInput(ns("file2"), label = lang$t("Group information"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     dropdownButton(circle=FALSE, label=lang$t("Sample data"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    downloadBttn(ns("downloadExp"), lang$t("matrix"), size='sm', block=TRUE),
                                    downloadBttn(ns("downloadGroup"), lang$t("group"), size='sm', block=TRUE) )
                 ) )
               ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Boxplot"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput( inputId = ns("gene"),  label = lang$t('Select gene'), c(""),multiple = F),
                     selectInput( inputId = ns("method"),  label = lang$t('test methods'),selected = "anova",
                                  c('none'='wu',"anova",'t.test','wilcox.test','kruskal.test') ),
                     dropdownButton(circle=FALSE, label=lang$t("Graphic labels"), icon = icon("image"),
                                    br(),br() ,
                                    numericInput(inputId = ns('label.x'),label = lang$t('label.x'),value = 0),
                                    numericInput(inputId = ns('label.y'),label = lang$t('label.y'),value = 0),
                                    textInput(ns('title'),lang$t("title"),value = ''),
                                    textInput(ns('xlab'),lang$t("xlab"),value = ''),
                                    textInput(ns('ylab'),lang$t("ylab"),value = ''),
                                    selectInput(ns('theme'),lang$t('theme'),selected = 'bw',
                                                choices = theme_select ),
                                    selectInput(ns("color_type"), lang$t('color type'), color_type ) 
                     ),
                     download_plot_UI('expr')
                 ) ) # fluidRow    
      ),
      tabPanel(title = "Help", helpUI("expr") )
       )
  )  # tagList
} # function(id)


