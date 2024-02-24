

library(ggplot2)
library('ggspatial')

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

lang <- Translator$new(translation_csvs_path = "./lang/info/")

worldmap_helpUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 h6("点击“show data”后“start drawing”，
                    依据画图情况修改参考数据（demo data）。"),
                 h6("参考数据的region包含251个国家或地区，label为标签，
                    long/lat分别为对应标签的经纬度坐标
                    （有75个国家或地区缺失坐标，如有需要请自行搜索后添加上去）。
                    value为对应数值。")
    )
    ) )
}

worldmapUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("table") ) )
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     downloadButton(ns("downloadSampleData"),lang$t("参考数据"))
                 )
               )
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title = lang$t("世界地图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     fluidRow(
                       column(width =3,
                              dropdownButton(circle=FALSE, label = lang$t("基础"),size = "sm", br(),br(),
                                             fluidRow(
                                               column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'void',choices = theme_select )),
                                               column(width = 6, textInput(ns('title'), label = "plot.title", value = "WorldMap" )),
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
                                       dropdownButton(circle=FALSE, label= lang$t("标题"), size = "sm",br(),br(),
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
                                       dropdownButton(circle=FALSE, label= lang$t("轴标题"), size = "sm",br(),br(),
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
                                       dropdownButton(circle=FALSE, label= lang$t("轴文本"), size = "sm",br(),br(),
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
                                       dropdownButton(circle=FALSE, label= lang$t("轴刻度"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_axis.ticks'), 'size', value = 0.5 ) ),
                                                        column(width = 12,numericInput(ns('linetype_axis.ticks'), 'linetype', value = 1 ) ),
                                                        column(width = 12,selectInput(ns('color_axis.ticks'), "color",
                                                                                      choices = c(colors(),"transparent"), selected = "transparent" ) )
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("图例标题"),size = "sm", br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_legend.title'), 'size', value = 20 )),
                                                        column(width = 12,selectInput(ns('color_legend.title'), "color",choices = colors(),  selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_legend.title'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.title'), 'angle', value = 0 ))
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("图例文本"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_legend.text'), 'size', value = 15 )),
                                                        column(width = 12,selectInput(ns('color_legend.text'), "color",choices = colors(),  selected = "black")),
                                                        column(width = 12,numericInput(ns('hjust_legend.text'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.text'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.text'), 'angle', value = 0 ))
                                                      )
                                       )
                                ) ) ) ) ,hr(),
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot") ) ) 
                 ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     fluidRow(
                       column( width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('比例尺'),
                                                     choices = c("show","hide"), selected = "show") ),
                       column( width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('指北针'),
                                                     choices = c("show","hide"), selected = "show") ),
                       column(width = 6,selectInput(inputId = ns("low") , lang$t("低值颜色"), colors() , selected = "gray" ) ),
                       column(width = 6,selectInput(inputId = ns("high"), lang$t("高值颜色"), colors() , selected = "red"   )),
                       column(width = 6,numericInput(inputId = ns('size'),label = lang$t('标签字号'),value = 5     ) ),
                       column(width = 6,selectInput(inputId = ns("label_color"), lang$t("标签颜色"), colors() , selected = "black"   )),
                       column(width = 6,selectInput(inputId = ns("line"), lang$t("界线颜色"), colors() , selected = "white"   ) ),
                       column(width = 6,selectInput(ns('legend.position'),lang$t('图例位置'),selected = 'right',
                                                    choices = c( "left","top", "right" , "bottom") ) )
                     ),
                     br() ,
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 20),
                                    numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 10),
                                    numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 120),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rds"),  label = "RDS",  size='sm', block=TRUE )
                     )
                 )
               )
      ),
      tabPanel(title = 'Help', worldmap_helpUI("worldmap") )
    ) )
}


worldmapServer <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        observeEvent(input$show, { 
          
          # Load the data # 读取数据
          data <- reactive({
            file1 <- input$file1
            if ( is.null(file1) ){
              data <- read.csv('./www/map/worldmap.csv')
            } 
            else{
              d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
              if( d=='csv' ){
                data <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
              } else{
                data <- data.frame( read_excel(file1$datapath,1) )
              } 
            } # else
            return( data )
          })
          
          # 输入数据
          if(!is.null(data() ) ){
            output$table <- renderRHandsontable(
              rhandsontable(data(),rowHeaderWidth = 40,  height = 400) %>% 
                hot_cols(columnSorting = TRUE) %>% 
                hot_col('region',readOnly = T) %>% 
                hot_col(c('long','lat','value'),type = 'numeric')
            )
          }
          
        } )
        
        observeEvent(input$submit, {
          
          plot <- reactive({
            
            
            labels <- hot_to_r( input$table)

            world <- map_data("world")
            world[which(world$region=='Taiwan'),'region'] <- "China"
            
            world$value <- 0
            for (i in 1:nrow(labels)) {
              world[which(world$region==labels$region[i] ),'value'] <- labels[which(labels$region==labels$region[i]),'value']
            }

              p <- ggplot() +
                geom_polygon(data = world, 
                             aes( x = long, y = lat,group=group, fill=value),show.legend = T,
                             color= input$line ) +
                scale_fill_gradient( low= input$low, high= input$high )+
                geom_text(data = labels ,
                          aes(x = long, y= lat, label =label),
                          colour = input$label_color,
                          size = input$size)
             
              # 添加比例尺
              if(input$annotation_scale=="show"){
                p <- p + annotation_scale(location='bl',plot_unit = "km") 
              }
              
              # 添加指北针
              if(input$annotation_north_arrow=="show"){
                p <- p + annotation_north_arrow(location = "tl", which_north = "false",
                                                style = north_arrow_fancy_orienteering)
              }
              
            p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
            
            if(!input$title==''){
              p <- p + ggtitle( input$title )
            }
            if(!input$x.axis.title==''){
              p <- p + xlab( input$x.axis.title )
            }
            if(!input$y.axis.title==''){
              p <- p + ylab( input$y.axis.title )
            }
            
            # labs(color=input$color_name) # 图例名
            # labs(fill=input$fill_name)
            # if(!input$color_name==''){
            #   color <- paste0("labs(color='",input$color_name,"')")
            #   p <- p + eval(parse(text = color ))
            # }
            if(!input$fill_name==''){ # 填充图例名
              fill <- paste0("labs(fill='",input$fill_name,"')")
              p <- p + eval(parse(text = fill ))
            }
            
            p <- p +
              theme(
                # 标题设置
                plot.title   = element_text(size  = input$size_title,
                                            hjust = input$hjust_title,
                                            color = input$color_title,
                                            vjust = input$vjust_title,
                                            angle = input$angle_title
                ),
                # 坐标轴标题
                axis.title   = element_text(size  = input$size_axis.title,
                                            color = input$color_axis.title,
                                            hjust = input$hjust_axis.title,
                                            vjust = input$vjust_axis.title,
                                            angle = input$angle_axis.title
                ),
                # 坐标轴标签
                axis.text    = element_text(size  = input$size_axis.text,
                                            color = input$color_axis.text,
                                            hjust = input$hjust_axis.text,
                                            vjust = input$vjust_axis.text,
                                            angle = input$angle_axis.text
                ),
                # 坐标轴刻度
                axis.ticks   = element_line(linewidth = input$size_axis.ticks,
                                            linetype  = input$linetype_axis.ticks,
                                            color     = input$color_axis.ticks
                ),
                legend.position = input$legend.position,  # # c( "left","top", "right" , "bottom")
                # 图例标签
                legend.title = element_text(size  = input$size_legend.title,
                                            hjust = input$hjust_legend.title,
                                            color = input$color_legend.title,
                                            vjust = input$vjust_legend.title,
                                            angle = input$angle_legend.title
                ), 
                # 图例文字
                legend.text  = element_text(size  = input$size_legend.text,
                                            hjust = input$hjust_legend.text,
                                            color = input$color_legend.text,
                                            vjust = input$vjust_legend.text,
                                            angle = input$angle_legend.text
                )
              )


            return(p)
            
          })
          
          output$plot <- renderPlot({
            return(plot() )
          })
          
          if(T){
            output$pdf <- downloadHandler(
              filename="map.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="map.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="map.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="map.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              } )
            output$rds <- downloadHandler( 
              filename="map.RDS",
              content = function(file){
                saveRDS(plot(), file)
              } )
          }
          
        })

        # 2、下载参考数据
        output$downloadSampleData <- downloadHandler(
          filename = function() {
            paste('map.csv')
          } ,
          content = function(file) {
            data <- read.csv('./www/worldmap.csv')
            write.csv(data , file, row.names = F, fileEncoding = "GB18030")
          }  )
        
      } 
)
}
      
      
      
