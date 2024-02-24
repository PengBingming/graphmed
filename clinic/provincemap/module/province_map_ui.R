
# library('shinycssloaders')
# library('rhandsontable')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

select_province <- c("中华人民共和国","北京市","天津市","河北省","山西省","内蒙古自治区",
                     "辽宁省","吉林省","黑龙江省","上海市","江苏省","浙江省","安徽省",
                     "福建省","江西省","山东省","河南省","湖北省","湖南省","广东省",
                      "广西壮族自治区","海南省","重庆市","四川省","贵州省","云南省",
                     "西藏自治区","陕西省","甘肃省","青海省","宁夏回族自治区",
                     "新疆维吾尔自治区","台湾省","香港特别行政区","澳门特别行政区")
# select_province <- unique(readRDS('./www/map_sample.RDS')[,"province"])

province_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6("1、province 为省份。name 列为标识列，列名“name”及列内容（省份名称）不可编辑修改。",style="color:orange"),
                 tags$h6("2、示例数据：“Show Data”→“开始画图”。"),
                 tags$h6("3、labels 列为标签，提供一种默认选择；value 列为数值型数据。
                         两列的列名不可修改，列内容可编辑修改。",style="color:orange"),
                 tags$h6("4、自己数据：下载参考数据→编辑 labels、value 列内容→上传→“Show Data”检查数据→“开始画图”出图"),
                 tags$h6("5、可修改标题内容、图例内容及标签大小。")
    )
    ) )
}

provinceUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
     tabPanel(title = 'Data',
              fluidRow(
              box(title=lang$t("Map data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                  splitLayout(cellWidths = c("80%"), rhandsontable::rHandsontableOutput(ns("table") ) ) 
                  ),
              box(width = 3,status="success",
                  lang$t("Data sources"),a("GeoAtlas,v3", href = "http://datav.aliyun.com/portal/school/atlas/area_selector"),
                  hr(),
                  fileInput(ns("file"),label = lang$t("Input file"), multiple = FALSE),
                  actionBttn(ns("show"), label = "Show Data",style = "fill", color = "primary", size = "sm" ),
                  hr(),
                  selectizeInput(inputId=ns('province'), label=lang$t('province'),choices=select_province,
                                 selected = "重庆市", multiple = T, options = NULL),
                  # selectInput(ns('province'),lang$t('省份'),selected = "重庆市",
                  #             choices = select_province, multiple = T ),
                  downloadButton(ns("downloadSampleData"),lang$t("Sample data"))
                  )
      ) ),
     tabPanel(title = 'Plot',
      fluidRow(
       box(title = lang$t("ProvinceMap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
           fluidRow(
             column(width =3,
                    dropdownButton(circle=FALSE, label = lang$t("basic"),size = "sm", br(),br(),
                                   fluidRow(
                                     column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'void',choices = theme_select )),
                                     column(width = 6, textInput(ns('title'), label = "plot.title", value = "ProvinceMap" )),
                                     column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "" )),
                                     column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "" ) ),
                                     # column(width = 6, textInput(ns("color_name"),label = "color.name",value = "") ),
                                     # column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) )
                                     column(width = 6, textInput(ns("fill_name"),label = "fill.name",value = "value") )
                                     # column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                                   )
                    ) ),
             column(width =9,
                    fluidRow(
                      column(width =2,
                             dropdownButton(circle=FALSE, label= lang$t("title"), size = "sm",br(),br(),
                                            fluidRow(
                                              column(width = 12,numericInput(ns('size_title'),  'size', value = 40 )),
                                              column(width = 12,selectInput(ns('color_title'),  "color",
                                                                            choices = colors(), selected = "black" )),
                                              column(width = 12,numericInput(ns('hjust_title'), "hjust",value = 0.5 )),
                                              column(width = 12,numericInput(ns('vjust_title'), 'vjust', value = 0 )),
                                              column(width = 12,numericInput(ns('angle_title'), 'angle', value = 0 ))
                                            )
                             )
                      ),
                      column(width = 2,
                             dropdownButton(circle=FALSE, label= lang$t("axis.title"), size = "sm",br(),br(),
                                            fluidRow(
                                              column(width = 12,numericInput(ns('size_axis.title'), 'size', value = 30 )),
                                              column(width = 12,selectInput(ns('color_axis.title'), "color",
                                                                            choices = c(colors(),"transparent"),  selected = "transparent" )),
                                              column(width = 12,numericInput(ns('hjust_axis.title'), "hjust", value = 0.5 )),
                                              column(width = 12,numericInput(ns('vjust_axis.title'), 'vjust', value = 0.5)),
                                              column(width = 12,numericInput(ns('angle_axis.title'), 'angle', value = 0 ))
                                            )
                             ) ) ,
                      
                      column(width = 2,
                             dropdownButton(circle=FALSE, label= lang$t("axis.text"), size = "sm",br(),br(),
                                            fluidRow(
                                              column(width = 12,numericInput(ns('size_axis.text'), 'size', value = 30 )),
                                              column(width = 12,selectInput(ns('color_axis.text'), "color",
                                                                            choices = c(colors(),"transparent"), selected = "transparent" )),
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
                                                                            choices = c(colors(),"transparent"), selected = "transparent" ) )
                                            )
                             ) ),
                      column(width = 2,
                             dropdownButton(circle=FALSE, label= lang$t("legend.title"),size = "sm", br(),br(),
                                            fluidRow(
                                              column(width = 12,numericInput(ns('size_legend.title'), 'size', value = 20 )),
                                              column(width = 12,selectInput(ns('color_legend.title'), "color",choices = colors(),  selected = "black" )),
                                              column(width = 12,numericInput(ns('hjust_legend.title'),"hjust",value = 0 )),
                                              column(width = 12,numericInput(ns('vjust_legend.title'), 'vjust', value = 0 )),
                                              column(width = 12,numericInput(ns('angle_legend.title'), 'angle', value = 0 ))
                                            )
                             ) ),
                      column(width = 2,
                             dropdownButton(circle=FALSE, label= lang$t("legend.text"), size = "sm",br(),br(),
                                            fluidRow(
                                              column(width = 12,numericInput(ns('size_legend.text'), 'size', value = 15 )),
                                              column(width = 12,selectInput(ns('color_legend.text'), "color",choices = colors(),  selected = "black")),
                                              column(width = 12,numericInput(ns('hjust_legend.text'),"hjust",value = 0 )),
                                              column(width = 12,numericInput(ns('vjust_legend.text'), 'vjust', value = 0 )),
                                              column(width = 12,numericInput(ns('angle_legend.text'), 'angle', value = 0 ))
                                            )
                             )
                      ) ) ) ) ,hr(),
           splitLayout(cellWidths = c("100%"),
                       shinycssloaders::withSpinner( plotOutput(ns("plot0"),height = 500 ) )
                       
                       ) 
       ),
       box(width=3,status="success",
           actionBttn(ns("submit"), label = lang$t("Start drawing"),style = "fill", color = "primary", size = "sm" ),
           hr(),
           fluidRow(
             column( width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('比例尺'),
                                           choices = c("show","hide"), selected = "show") ),
             column( width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('指北针'),
                                           choices = c("show","hide") ,selected = "show" ) ),
             column( width = 6,selectInput(inputId = ns("low") , lang$t("Low-value color"), colors() , selected = "white") ),
             column( width = 6,selectInput(inputId = ns("high"), lang$t("High-value color"), colors() , selected = "red" ) ),
             column( width = 6,numericInput(inputId = ns('size'),label = lang$t('Label font size'),value = 4     ) ),
             column( width = 6,selectInput(inputId = ns('label_color'),label = lang$t('label.color'),choices = colors(),selected = "black") ),
             column( width = 6,selectInput(ns('legend.position'),lang$t('legend.position'),selected = 'right',
                                           choices = c( "left","top", "right" , "bottom")))
           ),br() ,
           dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                          br(),br() ,
                          numericInput(inputId = ns('w'),label = lang$t('plot.weight'),value = 15),
                          numericInput(inputId = ns('h'),label = lang$t('plot.high'),value = 15),
                          numericInput(inputId = ns('ppi'),label = lang$t('plot.dpi'),value = 72),
                          downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("rds"),  label = "RDS",  size='sm', block=TRUE )
           )
       )
     )
     ),
     tabPanel(title = 'Plotly',
              fluidRow(
                box(title = lang$t("ProvinceMap"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                    splitLayout(cellWidths = c("100%"),
                                shinycssloaders::withSpinner( plotly::plotlyOutput(ns("plot1") ) )
                                ) 
                ),
                box(width=3,status="success",
                    actionBttn(ns("submitPlotly"), label = lang$t("Start drawing"),style = "fill", 
                               color = "primary", size = "sm" ),hr(),
                    downloadButton(ns("downloadplot"),  label = ".html")
                    )
              )
     ),
     tabPanel(title = 'Help', province_helpUI("province") )
    ) # tabsetPanel
  ) # tagList
} # function(id)

