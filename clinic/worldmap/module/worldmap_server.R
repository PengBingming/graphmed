
library('readxl')
library('rhandsontable')
library('sf')
library('ggspatial')
library('ggplot2')     # 画图

# library("showtext") #中文问题
showtext::showtext_auto()

worldmapServer <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        observeEvent(input$show, { 
          
          # Load the data # 读取数据
          data <- reactive({
            file1 <- input$file1
            if ( is.null(file1) ){
              data <- read.csv('./www/worldmap.csv')
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
          
          output$plot0 <- renderPlot({
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
      
      
      
