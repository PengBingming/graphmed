
library('readxl')
library('ggplot2') 
library('ggpubr')
library('ggsci')

library("showtext")
showtext_auto()


lmServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # init
      output$plot <- renderPlot({
        NULL
      })
      
      observeEvent(input$show, {
        
        # 读取文件 file
        df <- reactive({
          
          file1 <- input$file1
          if( is.null(file1) ){
            
            set.seed(1234) # 随机种子
            df <- data.frame( x = rep(1:50,4),  # 创建数据
                              y = (1:200/10 + rnorm(200, mean = 20, sd = 5)/10 )^3  ,
                              group = rep(c("G1", "G2","G3","G4"), each = 50),
                              face = c(rep("F1", each =100),rep( "F2", each = 100) ) )
            
          }
          else if( !is.null(file1) ){
            
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1) # 文件格式 csv xlsx xls
            
            if( d =='csv' ){
              df <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
            } 
            else{
              df <- data.frame( read_excel(file1$datapath,1) ) 
            } 
          } 
          colnames(df) <-  tolower(colnames(df))
          return(df) # 输出文件
        })
        
        output$table <- renderRHandsontable(
          rhandsontable(df(), rowHeaderWidth = 22, width = 300, height = 360) %>% 
            hot_cols(columnSorting = TRUE) %>% 
            hot_col(c('x','y'), type = 'numeric' ) %>% 
            hot_col(c('group','face'), type = 'dropdown' )
        )
        
        
        
        observe({
          if(!is.null(input$table ) ){
            df <- as.data.frame(hot_to_r( input$table ) )
            
            updateSelectInput(session, 'factor', choices = colnames(df) ,
                              selected = colnames(df)[3:ncol(df)] )
            updateSelectInput(session,"numeric" , choices = colnames(df) ,
                              selected = colnames(df)[1:2] )
            updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                              selected = "" )
            
            updateSelectInput(session, "x", label = 'x', choices = colnames(df) ,
                              selected = colnames(df)[1]  )
            updateSelectInput(session, "y", label = 'y', choices = colnames(df) ,
                              selected = colnames(df)[2]  )
            updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                              selected = colnames(df)[3]  )
            updateSelectInput(session, "fill", label = 'fill', choices = colnames(df) ,
                              selected = ''  )
            
            
            # 各种图形参数
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
              
              observeEvent(input$submit, {
                # 分析处理数据
                
                plot <- reactive({
                  
                  df <- hot_to_r( input$table )
                  df <- as.data.frame( df )
                  df <- na.omit(df)
                  
                  
                  expr <- paste0("ggplot(df,aes(x =", input$x,
                                 ", y =", input$y , 
                                 ",color=",input$color,
                                 ",fill=",input$fill,")",
                                 " )")
                  
                  p <- eval( parse(text =expr ) )
                  
                  if(input$plot=='s'){
                    p <- p + geom_point(size=input$size_plot)
                  }
                  else if(input$plot=='n'){
                    
                    if(input$method=='gam'){
                      
                      p <- p + geom_smooth( method = input$method, 
                                            show.legend = F,
                                            se = (input$se=='T') ,
                                            level=input$se_level,
                                            size=input$size_line)
                      
                    }else{
                      p <- p + 
                        geom_smooth( method = input$method, 
                                     show.legend = F, 
                                     formula = input$formula,
                                     se = (input$se=='T') ,
                                     level=input$se_level,
                                     size=input$size_line)
                    }
                  }
                  else{
                    if(input$method=='gam'){
                      p <- p + 
                        geom_point(size=input$size_plot)+
                        geom_smooth( method = input$method, 
                                     show.legend = F, 
                                     se = (input$se=='T'),
                                     level=input$se_level,
                                     size=input$size_line)
                    } else{
                      p <- p + 
                        geom_point( size=input$size_plot )+ 
                        geom_smooth( method = input$method, 
                                     show.legend = F, 
                                     formula = input$formula,
                                     se = c(input$se=='T'),
                                     level=input$se_level,
                                     size=input$size_line )
                    }
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
                      plot.title   = element_text(size  = input$size_title,
                                                  hjust = input$hjust_title,
                                                  color = input$color_title,
                                                  vjust = input$vjust_title,
                                                  angle = input$angle_title
                      ),
                      axis.title   = element_text(size  = input$size_axis.title,
                                                  color = input$color_axis.title,
                                                  hjust = input$hjust_axis.title,
                                                  vjust = input$vjust_axis.title,
                                                  angle = input$angle_axis.title
                      ),
                      axis.text    = element_text(size  = input$size_axis.text,
                                                  color = input$color_axis.text,
                                                  hjust = input$hjust_axis.text,
                                                  vjust = input$vjust_axis.text,
                                                  angle = input$angle_axis.text
                      ),
                      axis.ticks   = element_line(linewidth  = input$size_axis.ticks,
                                                  color = input$color_axis.ticks
                      ),
                      legend.title = element_text(size  = input$size_legend.title,
                                                  hjust = input$hjust_legend.title,
                                                  color = input$color_legend.title,
                                                  vjust = input$vjust_legend.title,
                                                  angle = input$angle_legend.title
                      ),
                      legend.text  = element_text(size  = input$size_legend.text,
                                                  hjust = input$hjust_legend.text,
                                                  color = input$color_legend.text,
                                                  vjust = input$vjust_legend.text,
                                                  angle = input$angle_legend.text
                      )
                    )
                  
                  # 配色
                  p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
                  p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))
                  
                  # 不分面
                  if ( input$face== F ) {
                    
                    # 计算相关性系数 R 与 P
                    if(!input$cor== F ){ 
                      p <- p +  stat_cor(method = input$cor , size=6,  show.legend = F,
                                         data = df, p.accuracy = 0.001, r.accuracy = 0.01,na.rm = T ) 
                    } # else 不计算 
                    
                  }
                  # 分面 
                  else if(input$face == T){    
                    
                    # 计算相关性系数 R 与 P
                    if(!input$cor== F ){ 
                      
                      for (i in 1:length(unique(df$face) ) ) {
                        p <- p + # 自定义 填充颜色
                          stat_cor(method = input$cor , aes(x,y), size=5, show.legend = F,
                                   data = subset(df,face== unique(df$face)[i] ),
                                   p.accuracy = 0.001, r.accuracy = 0.01 ,na.rm = T)
                      }
                    } # else 不计算 
                    p <- p + facet_wrap("~ face")
                    
                  }
                  
                  
                  return(p)
                })
                
                
                # 4、拟合图像，网页呈现
                output$plot <-  renderPlot({
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
                    }
                  )
                  output$rds0 <- downloadHandler( 
                    filename="plot.RDS",
                    content = function(file){
                      saveRDS( plot() ,file)
                    }  )
                }
                
                
              })  # observeEvent(input$submit )
              
            }) # observe
          }
        }) # observeEvent( input$table ) 
        
      })
      
      # 下载参考数据
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('cor.csv')
        },
        content = function(file) {
          
          if(is.null( input$table ) ){
            df <- df()
          }
          else{
            df <- hot_to_r( input$table )
          }
          write.csv(df, file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
    } # function(input, output, session)
  ) # moduleServer
} # function(id)