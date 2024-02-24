
library('rhandsontable')

# # File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

color_type <- c("npg","aaas","nejm","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

forest_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，列名大小写保持一致，不要改变。")),
                 tags$h6(lang$t("方法一：点击“Analyze Data”后将自己的数据粘贴到“输入数据”表格。
                             若需要增减行数，可在输入表格框内右键。")),
                 tags$h6(lang$t("方法二：下载数据后编辑上传，再点击“Analyze Data”。"))
                 
    )
    ) )
  
}

forestplotUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     rHandsontableOutput(ns('table') ) ),
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
                     downloadButton(ns("downloadtable"),lang$t("Sample data")) ) )
               ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Forestplot"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     fluidRow(
                       column(width =3,
                              dropdownButton(circle=FALSE, label= lang$t("basic"),size = "sm", br(),br(),
                                             fluidRow(
                                               column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                                               column(width = 6, textInput(ns('title'), label = "plot.title", value = "Forstplot" )),
                                               column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "OR" )),
                                               column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "Var" ) ),
                                               column(width = 6, textInput(ns("color_name"),label = "color.name",value = "Factor") ),
                                               column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) )
                                               # column(width = 6, textInput(ns("fill_name"),label = "fill.name",value = "") ),
                                               # column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                                             )
                              ) ),
                       column(width =9,
                              fluidRow(
                                column(width =2,
                                       dropdownButton(circle=FALSE, label= lang$t("title"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_title'),  'size', value = 30 )),
                                                        column(width = 12,selectInput(ns('color_title'),  "color",choices = colors(), selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_title'), "hjust",value = 0.5 )),
                                                        column(width = 12,numericInput(ns('vjust_title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_title'), 'angle', value = 0 ))
                                                      )
                                       )
                                ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("axis.title"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_axis.title'), 'size', value = 20 )),
                                                        column(width = 12,selectInput(ns('color_axis.title'), "color",choices = colors(),  selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_axis.title'),"hjust",value = 0.5 )),
                                                        column(width = 12,numericInput(ns('vjust_axis.title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_axis.title'), 'angle', value = 0 ))
                                                      )
                                       ) ) ,
                                
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("axis.text"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_axis.text'), 'size', value = 15 )),
                                                        column(width = 12,selectInput(ns('color_axis.text'), "color",choices = colors(), selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_axis.text'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_axis.text'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_axis.text'), 'angle', value = 0 ))
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("axis.ticks"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_axis.ticks'), 'size', value = 0.5 ) ),
                                                        column(width = 12,numericInput(ns('linetype_axis.ticks'), 'linetype', value = 1 ) ),
                                                        column(width = 12,selectInput(ns('color_axis.ticks'), "color",
                                                                                      choices = colors(), selected = "black" ) )
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("legend.title"),size = "sm", br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_legend.title'), 'size', value = 15 )),
                                                        column(width = 12,selectInput(ns('color_legend.title'), "color",choices = colors(),  selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_legend.title'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.title'), 'angle', value = 0 ))
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("legend.text"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_legend.text'), 'size', value = 10 )),
                                                        column(width = 12,selectInput(ns('color_legend.text'), "color",choices = colors(),  selected = "black")),
                                                        column(width = 12,numericInput(ns('hjust_legend.text'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.text'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.text'), 'angle', value = 0 ))
                                                      )
                                       )
                                ) ) ) ),
                     hr(),
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),width = 810,height = 500 ) )
                     ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     selectInput(ns("background"),  lang$t("background color"), colors() , selected = "skyblue" ),
                     selectInput(ns("zero"),        lang$t("vline color"), colors() , selected = "black"   ),
                     selectInput(ns("lines"),       lang$t("errorbarh color"), colors() , selected = "black"   ),
                     hr(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('plot.weight'),value = 18),
                                    numericInput(inputId = ns('h'),label = lang$t('plot.high'),value = 12),
                                    numericInput(inputId = ns('ppi'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rds"), label = "RDS", size='sm', block=TRUE ) 
                                    )
                 )
               ) # fluidRow
      ),
      tabPanel(title = 'Help',forest_helpUI("heatmap"))
      )
  ) # tagList
} # function(id)
