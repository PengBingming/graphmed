
theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

basic_plot_UI <- function(id) { # basic ggplot ui setting
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width =3,
             dropdownButton(circle=FALSE, label = lang$t("basic"),size = "sm", br(),br(),
                            fluidRow(
                              column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'void',choices = theme_select )),
                              column(width = 6, textInput(ns('title'), label = "plot.title", value = "ChinaMap" )),
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
    splitLayout(cellWidths = c("100%"),plotOutput(ns("plot0"), height = 600 )
    )
  ) 
}

download_plot_UI <- function(id){# download plot ui setting
  ns <- NS(id)
  tagList(
    hr(),
    dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                   br(),br() ,
                   numericInput(inputId  = ns('w'),    label = lang$t("plot.weight"),value = 12),
                   numericInput(inputId  = ns('h'),    label = lang$t("plot.high"),value = 8),
                   numericInput(inputId  = ns('ppi'),  label = lang$t("plot.dpi"),value = 150),
                   downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("rds"),  label = "RDS",  size='sm', block=TRUE )
    )
  )
}

map_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("Help"),
                 tags$hr(),
                 tags$video(width="50%", controls = NA,
                            src="ChinaMap.mp4", type = "video/mp4"),
                 tags$hr(),
                 tags$h6("1、name 列为标识列，列名“name”及列内容（省份名称）不可编辑修改。",style="color:orange"),
                 tags$h6("2、示例数据：“Show Data”→“开始画图”。"),
                 tags$h6("3、labels 列为标签，提供四种选择；value 列为数值型数据。
                         两列的列名不可修改，列内容可编辑修改。",style="color:orange"),
                 tags$h6("4、自己数据：下载参考数据→编辑 labels、value 列内容→上传→“Show Data”检查数据→“开始画图”出图"),
                 tags$h6("5、可修改标题内容、图例内容及标签大小。"),
                 h6("6、“刻度尺”可能不一定准确：Scale on map varies by more than 10%, scale bar may be inaccurate")
    )
    ) )
}
