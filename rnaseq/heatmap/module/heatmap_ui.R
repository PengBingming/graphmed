
library('rhandsontable')
source('module/global.R')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

heatmapUI <- function(id) {
    ns <- NS(id)
    shiny.i18n::usei18n(lang)
    tagList(
      bs4Dash::tabsetPanel(
        tabPanel(title = 'Data',
                 fluidRow(
                   box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("data") ) ) ),
                   box(width = 3,status="success",
                       fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                       h6('Format: .csv .xlsx .xls'),
                       actionBttn( inputId = ns("show"), label = "Show Data",
                                   style = "fill", color = "primary", size = "sm" ),
                       hr() ,
                       downloadButton(ns("downloadSampleData"), lang$t("Sample data"))
                   ) ) ),
        tabPanel(title = 'Annotation',
                 fluidRow(
                   box(title=lang$t("Annotation.row"),width=4,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("anno_row") ) ) ),
                   box(title=lang$t("Annotation.col"),width=5,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("anno_col") ) ) ),
                   box(width = 3,status="success",
                       dropdownButton(circle=FALSE, label=lang$t("Set row parameters"), br(),br() ,
                                      selectInput(ns('rowname'),lang$t("rowname"),choices = c("show"='T',"hide"='F'),selected = 'T'),
                                      selectInput(ns('row_cluster'),lang$t('row.cluster'),choices = c("Yes"='T',"No"='F'),selected = 'T'),
                                      selectInput(ns('row'),lang$t('add anno.row'), c("Yes"='T',"No"='F'),selected = 'T'),
                                      textInput(ns('row1'),label = lang$t('anno.row1'),value = 'cluster'),
                                      textInput(ns('row2'),label = lang$t('anno.row2'),value = '')
                       ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("Set col parameters"), br(),br() ,
                                      selectInput(ns('colname'),lang$t("colname"),choices = c("show"='T',"hide"='F'),selected = 'T'),
                                      selectInput(ns('col_cluster'),lang$t('col.cluster'),choices = c("Yes"='T',"No"='F'),selected = 'F'),
                                      selectInput(ns('col'),lang$t('add anno.col'), c("Yes"='T',"No"='F'),selected = 'T'),
                                      textInput(ns('col1'),label = lang$t('anno.col1'),value = 'group'),
                                      textInput(ns('col2'),label = lang$t('anno.col2'),value = 'rep')
                       )
                   ) )    ),
        tabPanel(title = 'Heatmap',
                 fluidRow(
                   box(title=lang$t("Heatmap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                   box(width=3,status="success",
                       actionBttn( inputId = ns("submit"), label = lang$t("Start drawing"),
                                   style = "fill", color = "primary", size = "sm" ),hr(),
                       
                       selectInput(ns("scale"),lang$t('Standardization'),choices = c("row"='row',"column"='column','none'='none'),selected = 'none'),
                       selectInput(ns('disp_num'),lang$t("display.numbers"),choices = c("show"='T',"hide"='F'),selected = 'F'),
                       selectInput(ns("angle_col"),lang$t('angle.col'),choices = c('270', '0', '45', '90', '315'),selected = '45' ),
                       dropdownButton(circle=FALSE, label=lang$t("graphic color"),icon = icon("image"),
                                      selectInput(ns('color1'),lang$t('Low.value color'),colors(),selected = "navy"),
                                      selectInput(ns('color2'),lang$t('Middle.value color'),colors(),selected = "white"),
                                      selectInput(ns('color3'),lang$t('High.value color'),colors(),selected = "firebrick3") ),br(),
                       download_plot_UI('heatmap')
                       )
                 ) # fluidRow
        ),
        tabPanel(title = 'Help',helpUI("heatmap"))
        )
    ) # tagList
  } # function(id)
