
# library('rhandsontable')
# library('DT')

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

color_type <- c("npg","aaas","nejm","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

logis_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6("1、参考数据中，因变量为二分类，在参数设置中选定（y），
                             默认第一列。其余列为自变量，可为连续变量、分类变量。"),
                 tags$h6("2、自变量为计数变量（有序及无序）时，先在变量类型中选择对应数据。",style="color:orange"),
                 tags$h6("3、自变量为连续变量（计量）时，可不选择，但不要有缺失值。"),
                 tags$h6("4、可在参数设置中对这两类变量进行选定，然后点击“Analyze Data”。")
                 
    )
    ) )
}

logisUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("df")) ) 
                 ), 
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data", 
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     column(width = 12,selectInput(ns('factor'),  h6(lang$t("Categorical variables"),style="color:orange"), c(""), multiple = T )),
                     column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("Ordered variables"),style="color:orange"), c(""), multiple = T )),
                     column(width = 12,selectInput(ns('numeric'), h6(lang$t("Continuous variables"),style="color:orange"), c(""), multiple = T )),
                     hr(),
                     downloadButton(ns("downloadtable"),lang$t("Sample data") )
                 ) )
               ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("Logistics result"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("contents1"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     selectInput(ns("group"), h6(lang$t("group: Dicategorical var"),style="color:orange"), c(""),multiple = F ),
                     selectInput(ns("var_select"), h6("var.select",style="color:orange"), c(""), multiple = T ),
                     selectInput(ns("type"), lang$t("Filter variables"), selected = "full",
                                 c('all variables'='full',  'step regression'='step') ),
                     hr(),
                     downloadButton(ns("downloadtable1"),lang$t("Result data") )
                 )
                 )
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Forestplot"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     fluidRow(
                       column(width =3,
                              dropdownButton(circle=FALSE, label= lang$t("basic"),size = "sm", br(),br(),
                                             fluidRow(
                                               column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                                               column(width = 6, textInput(ns('title'), label = "plot.title", value = "Logistic Forstplot" )),
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
                                       dropdownButton(circle=FALSE, label= lang$t("basic"), size = "sm",br(),br(),
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
                                ) ) ) ) ,
                     hr(),
                     splitLayout(cellWidths = c("100%"), plotOutput(ns('plot') ) )  
                 ) ,
                 box(width = 3, status="success",
                     selectInput(ns("background"),  lang$t("background color"), colors() , selected = "skyblue" ),
                     selectInput(ns("zero"),        lang$t("vline color"), colors() , selected = "black" ),
                     selectInput(ns("lines"),       lang$t("errorbarh color"), colors() , selected = "black" ) 
                     , br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rds0"),  label = "RDS",  size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Plot data',
               fluidRow(
                 box(title=lang$t("Forestplot data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("contents2"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     downloadButton(ns("downloadtable2"),lang$t("Plot data") )
                 )
               )
               ),
      tabPanel(title = 'Help',logis_helpUI("logis") )
      )
  ) #  tagList
} # function(id)
