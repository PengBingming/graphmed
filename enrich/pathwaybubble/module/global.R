
basic_plot_UI <- function(id) { # basic ggplot ui setting
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width =3,
             dropdownButton(circle=FALSE, label= lang$t("basic"),size = "sm", br(),br(),
                            fluidRow(
                              column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                              column(width = 6, textInput(ns('title'), label = "plot.title", value = "Pathway enrich" )),
                              column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "" )),
                              column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "" ) ),
                              column(width = 6, textInput(ns("color_name"),label = "color.name",value = "") ),
                              # column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) ),
                              column(width = 6, textInput(ns("size_name"),label = "size.name",value = "") ),
                              column(width = 6, textInput(ns("fill_name"),label = "fill.name",value = "") )
                              # column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                            )
             ) ),
      column(width =9,
             fluidRow(
               column(width =2,
                      dropdownButton(circle=FALSE, label= lang$t("title"), size = "sm",br(),br(),
                                     fluidRow(
                                       column(width = 12,numericInput(ns('size_title'),  'size', value = 40 )),
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
                                       column(width = 12,numericInput(ns('size_axis.text'), 'size', value = 20 )),
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
               ) ) ) ) ,
    hr(),
    splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 600 ) )
  )
}

