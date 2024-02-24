
library('RColorBrewer')
library('ggrepel')
library('sf') 
library('geojsonsf')
library(shinycssloaders)
library('showtext') # 解决画图中文乱码
showtext_auto()

library('readxl')
library('rhandsontable')
library('ggspatial')

library('plotly')
library('ggplot2')
library('shinyWidgets')
library('htmlwidgets')


theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

select_province <- unique(readRDS('./www/map/map_sample.RDS')[,"province"])

lang <- Translator$new(translation_csvs_path = "./lang/info/")
province_helpUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
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
              box(title=lang$t("地图数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                  splitLayout(cellWidths = c("80%"), rHandsontableOutput(ns("table") ) ) 
                  ),
              box(width = 3,status="success",
                  lang$t("数据来源："),a("GeoAtlas,v3", href = "http://datav.aliyun.com/portal/school/atlas/area_selector"),
                  hr(),
                  fileInput(ns("file"),label = lang$t("输入文件"), multiple = FALSE),
                  actionBttn(ns("show"), label = "Show Data",style = "fill", color = "primary", size = "sm" ),
                  hr(),
                  selectInput(ns('province'),lang$t('省份'),selected = "重庆市",
                              choices = select_province, multiple = T ),
                  downloadButton(ns("downloadSampleData"),lang$t("参考数据"))
                  )
              
      ) ),
     tabPanel(title = 'Plot',
      fluidRow(
       box(title = lang$t("省市地图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
           fluidRow(
             column(width =3,
                    dropdownButton(circle=FALSE, label = lang$t("基础"),size = "sm", br(),br(),
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
           splitLayout(cellWidths = c("100%"),
                       withSpinner( plotOutput(ns("plot0"),height = 500 ) )
                       
                       ) 
       ),
       box(width=3,status="success",
           actionBttn(ns("submit"), label = lang$t("开始画图"),style = "fill", color = "primary", size = "sm" ),
           hr(),
           fluidRow(
             column( width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('比例尺'),
                                           choices = c("show","hide"), selected = "show") ),
             column( width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('指北针'),
                                           choices = c("show","hide") ,selected = "show" ) ),
             column( width = 6,selectInput(inputId = ns("low") , lang$t("低值颜色"), colors() , selected = "white") ),
             column( width = 6,selectInput(inputId = ns("high"), lang$t("高值颜色"), colors() , selected = "red" ) ),
             column( width = 6,numericInput(inputId = ns('size'),label = lang$t('标签字号'),value = 4     ) ),
             column( width = 6,selectInput(inputId = ns('label_color'),label = lang$t('标签颜色'),choices = colors(),selected = "black") ),
             column( width = 6,selectInput(ns('legend.position'),lang$t('图例位置'),selected = 'right',
                                           choices = c( "left","top", "right" , "bottom")))
           ),br() ,
           dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                          br(),br() ,
                          numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                          numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                          numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
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
                box(title = lang$t("中国地图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                    splitLayout(cellWidths = c("100%"),
                                withSpinner( plotlyOutput(ns("plot1") ) )
                                ) 
                ),
                box(width=3,status="success",
                    actionBttn(ns("submitPlotly"), label = lang$t("开始画图"),style = "fill", 
                               color = "primary", size = "sm" ),hr(),
                    downloadButton(ns("downloadplot"),  label = ".html")
                    )
              )
     ),
     tabPanel(title = 'Help', province_helpUI("province") )
    ) # tabsetPanel
  ) # tagList
} # function(id)


provinceServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$plot0 <- renderPlot({ return(NULL) })
      output$plot1 <- renderPlotly({ return(NULL) })

      observeEvent(input$show, {
        
        # Load the data # 读取数据
        df1 <- reactive({
            file1 <- input$file
            if ( is.null(file1) ){
              
              df_province <- readRDS('./www/map/map_sample.RDS')
              data <- data.frame()
              for (i in input$province ) {
                data <- rbind(data,subset(df_province, province == i ))
              }
  
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
          
        output$table <- renderRHandsontable(
          if( is.null(df1() ) ){return(NULL)}else{
            return( rhandsontable(df1(),rowHeaderWidth = 22,height = 500) %>% 
                      hot_cols(columnSorting = TRUE) %>% 
                      hot_col(colnames(df1())[1:2], readOnly = TRUE) %>% 
                      hot_col('value',type = 'numeric')
                    )
            }
          
        )
        
        observeEvent(input$submit, {

        # 整合数据 china_data
        data_map <- reactive({
          
          if( is.null(df1() ) ){return(NULL)}
          
          df1 <- as.data.frame( hot_to_r( input$table ) )
          
          df <- readRDS('www/map/map_province.RDS')

          ids <- intersect(df$name,df1$name)
          
          df0 <- data.frame()
          for (i in ids) {
            df0 <- rbind(df0, filter(df,name==i ))
          }
          
          df0$labels <- ''
          df0$value <- 0

          for (i in 1:nrow(df1) ){
            df0[which(df0$name ==df1$name[i] ),c('labels',"value")] <- df1[i,c('labels','value')]
          }
          
          if(input$size==0){ df0$labels <- '' }
          
          df0$jd <- c(matrix(unlist(df0$center), ncol = 2, byrow = T)[,1] )
          df0$wd <- c(matrix(unlist(df0$center), ncol = 2, byrow = T)[,2] )

          df0$value <- as.numeric(df0$value )
          
          return(df0)
          
        })
        
          # 编写函数
          myfun <- reactive({
            
            if( is.null(df1() ) ){return(NULL)}
            
            data_map <- data_map() 
            
            p <- ggplot(data=data_map)+ geom_sf(aes(fill = value) ) + 
              
              fixed_plot_aspect(ratio = 1.25)
              # coord_sf(crs = 4326)
              # 添加比例尺
              if(input$annotation_scale=="show"){
                p <- p + annotation_scale(location='bl',plot_unit = "km") 
              }
            # 添加指北针
            if(input$annotation_north_arrow=="show"){
              p <- p + annotation_north_arrow(location = "tl", which_north = "false",
                                              style = north_arrow_fancy_orienteering)
            }
            
            p <- p +
              # 颜色
              scale_fill_gradient(low= input$low, high= input$high )+
              # 标签
              geom_text(data = data_map,
                        aes(x=jd, y=wd, label= labels),
                        position = "identity",
                        colour = input$label_color,
                        size = input$size)
            
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
            
            # 配色
            # p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
            # p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))

            return( p)
            
          } )
          
          output$plot0 <- renderPlot({
            if( is.null(df1() ) ){return(NULL)}
            return( myfun() )
            
          }  )
          
          if(T){
            output$pdf <- downloadHandler(
              filename="map.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(myfun() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="map.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="map.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              } )
            
            output$tiff <- downloadHandler( 
              filename="map.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              } )
            
            output$rds <- downloadHandler( 
              filename="map.RDS",
              content = function(file){
                saveRDS(myfun(),file = file)
              } )
          }
          
          observeEvent(input$submitPlotly, {
            
            output$plot1 <- renderPlotly({
              if( is.null(df1() ) ){return(NULL)}
              return( ggplotly(myfun(),height = 550,width = 700) )
              
            }  )

            # # 下载图形 .html
            output$downloadplot <- downloadHandler(
              filename = function() {
                paste("ChinaMap.html")
              },
              content = function(file) {
                p <- myfun()
                htmlwidgets::saveWidget(as_widget(ggplotly(p)), file)
                
              }  )
          })
          
      })
      
      } )  #  observeEvent(input$submit, {
      
      # 下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('ProvinceMap_data.csv')
        },
        content = function(file) {

          write.csv(readRDS('./www/map/map_sample.RDS'), file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
    } # function(input, output, session)
) # moduleServer
} # function(id) 

