
# library('readxl')
library('rhandsontable')
library('ggplot2')
# library(showtext)
showtext::showtext_auto() 

roseServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data  # 读取数据
        mydata <- reactive({
          file1 <- input$file1
          
          if ( is.null(file1) ){
            mydata <- data.frame(var =c("hsa","mmu","rat",'cat',"dog","pig",'rabbit','monkey') ,
                                 value = c(8,10,6,5,3,4,4,2) ,
                                 label = c(8,10,6,5,3,4,4,'a') )
          }
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              mydata <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              mydata <- data.frame( readxl::read_excel(file1$datapath,1) )
            } 
          } # else
          return( mydata )
        })
        
        
        if(!is.null(mydata()) ){
          output$mydata <- renderRHandsontable(
            rhandsontable( mydata() ,rowHeaderWidth = 50, height = 410) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
      
        
        observe({
          if(!is.null( input$mydata ) ){
            df <- as.data.frame(hot_to_r( input$mydata ) )
        
            # 数据类型
            updateSelectInput(session, 'factor', choices = colnames(df) ,
                              selected = colnames(df)[1] )
            updateSelectInput(session,"numeric" , choices = colnames(df) ,
                              selected = colnames(df)[2] )
            updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                              selected = "" )
            
            updateSelectInput(session, "x", label = 'x', choices = colnames(df) ,
                              selected = colnames(df)[1] )
            updateSelectInput(session, "y", label = 'y', choices = colnames(df) ,
                              selected = colnames(df)[2] )
            updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                              selected = ""  )
            updateSelectInput(session, "fill", label = 'fill', choices = colnames(df) ,
                              selected = colnames(df)[2] )
            updateSelectInput(session, "label", label = 'label', choices = colnames(df) ,
                              selected = colnames(df)[3] )
            
            observe({
              
              # 因子型数据
              if(length(input$factor)>=1){
                for ( i in input$factor) {
                  df[,i] <- factor(df[,i], ordered = F)
                }
              }
              # 有序数据
              if(length(input$factor_order)>=1){
                for ( i in input$factor_order) {
                  df[,i] <- factor(df[,i], ordered = T)
                }
              }
              
              # 数值型数据
              if(length(input$numeric)>=1){
                for ( i in input$numeric ) {
                  df[,i] <- as.numeric(df[,i])
                }
              }
            
        observeEvent(input$submit1, {
          
          # if(is.null( df )){return(NULL)}

          plot <- reactive({      
            
            # mydata <- df
            n <- which(is.na(df[,input$x]) )
            if(length(n)>0){ df <- df[-n,] }
            
            df <- df[order(df[,input$y]),]
            df[,input$x] <- factor(df[,input$x],levels = df[,input$x] ,ordered = F)
            
            library(ggplot2)
            expr <-  paste0("ggplot(df,aes(x =", input$x,
                        ", y =", input$y , 
                        ",color=",input$color,
                        ",fill=",input$fill,")",
                        " )")
            p <- eval( parse(text = expr ) )
            p <- p +
              geom_bar(width=1, stat = "identity", 
                       colour=input$line ) + # 分割线颜色
              scale_fill_gradient(low = input$low, high = input$high) # 填充渐变颜色
              # 数值标签、颜色、字号
            text <- paste0("geom_text(df,mapping = aes(y=",input$y,
                           ",label= ",input$label,")",
                           ",color='",input$label_color,"'",
                           ",size=",input$size,
                           ")"
                           )
            p <- p + eval( parse(text = text ) )
              # geom_text(aes(y= value , label= label ),
              #           color=input$label_color, size= input$size) +
            p <- p +  coord_polar(theta = "x",start=0)
              
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
           if(!input$color_name==''){
             color <- paste0("labs(color='",input$color_name,"')")
             p <- p + eval(parse(text = color ))
           }
           if(!input$fill_name==''){
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
               axis.ticks.y = element_blank(),
               axis.text.y  = element_blank(),
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
            
            return( p )
            
          })
          
          output$plot <- renderPlot({
            return( plot() )
          })
          
          # 下载图形
          if(T){
            output$pdf0 <- downloadHandler(
              filename="plot.pdf",
              content = function(file){
                pdf(file,width=input$w0,height=input$h0)
                print( plot() )
                dev.off()
              }
            )
            output$png0 <- downloadHandler(
              filename="plot.png",
              content = function(file){
                png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                print( plot() )
                dev.off()
              }
            )
            output$jpeg0 <- downloadHandler(
              filename="plot.jpeg",
              content = function(file){
                jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                print(plot() )
                dev.off()
              }
            )
            output$tiff0 <- downloadHandler( 
              filename="plot.tiff",
              content = function(file){
                tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                print(plot() )
                dev.off()
              }  )
            output$rds0 <- downloadHandler( 
              filename="plot.RDS",
              content = function(file){
                saveRDS( plot() ,file )
              }  )
            
          } # 下载图形
          
          })
        
          })
        
          }
      })

        }) #  show
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        
        filename = function() {
          paste('rose.csv')
        },
        content = function(file) {
          mydata <- data.frame(var =c("hsa","mmu","rat",'cat',"dog","pig",'rabbit','monkey'),
                               value = c(8,10,6,5,3,4,4,2) ,
                               label = c(8,10,6,5,3,4,4,2) )
          
          write.csv( mydata , file , row.names = F, fileEncoding = 'GB18030') 
        } ) 
    }
    
  ) }
      


