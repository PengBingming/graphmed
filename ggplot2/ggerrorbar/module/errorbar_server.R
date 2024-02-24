
# library('readxl')
library('rhandsontable')
library('ggplot2')
library("ggsci")
library('ggpubr')

errorbarServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        # Load the data # 读取数据
        df <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            df <- ToothGrowth[,c(2,3,1)]
            colnames(df)[1:2] <- c('group1','group2')
            # rownames(df) <- 1:nrow(df)
          } 
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              df <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              df <- data.frame( readxl::read_excel(file1$datapath,1) )
            } 
          } # else
          return( df )
        })
        
        # 输入数据
        if(!is.null(df() ) ){
          output$table <- renderRHandsontable(
            rhandsontable(df(),rowHeaderWidth = 100,  height = 400) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
        
        observe({
          if(!is.null(input$table ) ){
            
            dat <- as.data.frame(hot_to_r( input$table ) )
            updateSelectInput(session, 'factor', choices = colnames(dat) ,
                              selected = colnames(dat)[1] )
            updateSelectInput(session, 'factor_order', choices = colnames(dat) ,
                              selected = "" )
            updateSelectInput(session,"numeric", choices = colnames(dat) ,
                              selected = colnames(dat)[2] )
            
            updateSelectInput(session,"group1", choices = colnames(dat) ,
                              selected = colnames(dat)[1] )
            updateSelectInput(session,"group2", choices = colnames(dat) ,
                              selected = colnames(dat)[2] )
            updateSelectInput(session,"var_select", choices = colnames(dat) ,
                              selected = colnames(dat)[3:ncol(dat)] )
            
            observe({ # observe inner
              
              # 因子型数据
              if(length(input$factor)>=1){
                for ( i in input$factor) {
                  dat[,i] <- factor(dat[,i], ordered = F)
                }
              }
              # 有序数据
              if(length(input$factor_order)>=1){
                for ( i in input$factor_order) {
                  dat[,i] <- factor(dat[,i], ordered = T)
                }
              }
              
              # 数值型数据
              if(length(input$numeric)>=1){
                for ( i in input$numeric ) {
                  dat[,i] <- as.numeric(dat[,i])
                }
              }
              
              observeEvent(input$result, { 
                
                resultTable <- reactive({
                  
                  dat <- dat[,c(input$group1,input$group2,input$var_select)]
                  
                  # 计算平均值（mean）与标准差（sd）
                  df <- data.frame()
                  for (i in unique(dat$group1 )) {
                    if(input$na_omit){dat <- na.omit(dat)}
                    dfz <- dat[which(dat[,input$group1]==i), setdiff(colnames(dat),input$group1 ) ]
                    
                    for (j in unique(dat$group2 ) ) {
                      df0 <- dfz[which(dfz[,input$group2]==j), setdiff(colnames(dfz),input$group2 ) ]

                      if(dim(dfz)[2]>2 ){
                        df1 <- data.frame( group1  = i,
                                           group2 = j,
                                           var   = colnames(df0),
                                           mean  = apply(df0, 2, mean ),
                                           sd    = apply(df0, 2, sd ), 
                                           median= apply(df0, 2, median ), 
                                           min   = apply(df0, 2, min ),
                                           max   = apply(df0, 2, max ) 
                        )
                        df <- rbind(df, df1)
                      }
                      else{
                        df1 <- data.frame( group1  = i,
                                           group2 = j,
                                           mean  = mean(df0),
                                           sd    = sd(df0), 
                                           median= median(df0), 
                                           min   = min(df0 ),
                                           max   = max(df0 ) 
                        )
                        df <- rbind(df, df1)
                      }
                      }

                  }
                  
                  return(df)
                })
                
                # 输入数据
                if(!is.null(resultTable() ) ){
                  output$resultTable <- renderRHandsontable(
                    rhandsontable(resultTable(),rowHeaderWidth = 100,  height = 400) %>% 
                      hot_cols(columnSorting = TRUE)
                  )
                }
                
                observe({
                  if(!is.null(input$resultTable ) ){
                    
                    df <- as.data.frame(hot_to_r( input$resultTable ) )
                    
                    df[,input$group1] <- as.factor( df[,input$group1] )
                    
                    # plot() 参数
                    updateSelectInput(session, "x.group1", label = 'group1', choices = colnames(df) ,
                                      selected = input$group1 )
                    updateSelectInput(session, "x.group2", label = 'group2', choices = colnames(df) ,
                                      selected = input$group2 )

                    updateSelectInput(session, "y", label = 'mean', choices = colnames(df) ,
                                      selected = 'mean'  )
                    updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                                      selected = input$group1   )

                    
                    # 各种图形参数
                    observe({
                      
                      observeEvent(input$submit, {
                        
                        plot <- reactive({

                          expr <- paste0("ggplot(df,aes(x = ", input$x.group2,
                                         ", y =", input$y ,
                                         ",group=",input$x.group1,
                                         ",color=",input$color,")",
                                         " )")
                          
                          p <- eval( parse(text =expr ) )
                          
                          # plot
                          if( T ){
                            if(!input$x.group1==''|!input$x.group2==''|!input$y==''){
                              p <- p + 
                                geom_errorbar(aes(ymin  = mean - sd,
                                                  ymax  = mean + sd ),
                                              width     = input$width_errorbar,
                                              alpha     = input$alpha_errorbar,
                                              position  = position_dodge(input$dodge),
                                              show.legend = F)+
                                geom_point(size=input$size_point,
                                          alpha=input$alpha_point) +
                                geom_line(size=input$size_line,
                                           alpha=input$alpha_line)
                                
                            }
                          }
                          
                          p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
                          
                          if(!input$title==''){
                            p <- p + ggtitle( input$title ) # 标题
                          }
                          if(!input$x.axis.title==''){
                            p <- p + xlab( input$x.axis.title ) # x 轴标签
                          }
                          if(!input$y.axis.title==''){
                            p <- p + ylab( input$y.axis.title ) # y 轴标签
                          }
                          
                          # labs(color=input$color_name) # 图例名
                          # labs(fill=input$fill_name)
                          if(!input$color_name==''){
                            color <- paste0("labs(color='",input$color_name,"')")
                            p <- p + eval(parse(text = color ))
                          }
                          # if(!input$fill_name==''){
                          #   fill <- paste0("labs(fill='",input$fill_name,"')")
                          #   p <- p + eval(parse(text = fill ))
                          # }
                          
                          if(length(input$var_select)>1){
                            p <- p + facet_wrap("~ var")
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
                              axis.ticks   = element_line(size      = input$size_axis.ticks,
                                                          linetype  = input$linetype_axis.ticks,
                                                          color     = input$color_axis.ticks
                              ),
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
                          p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
                          # p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))
                          
                          return(p)
                          
                        })
                        
                        output$plot <- renderPlot({return( plot() ) })
                        
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
                              saveRDS( plot() ,file)
                            }  )
                        } # 下载图形
                        
                      } )
                      
                    }) # obersve
                    
                    
                    
                  }
                })
                
                
                output$downloadResult <- downloadHandler(
                  filename = function() {
                    paste('result.csv')
                  } ,
                  content = function(file) {
                    write.csv(resultTable() , file, row.names = F, fileEncoding = "GB18030")
                    
                  } )
              })
              # obersve
              
            }) # observe inner
            
          }
        })
        
      })
      
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        } ,
        content = function(file) {
          
          df <- ToothGrowth[,c(2,3,1)]
          colnames(df)[1:2] <- c('group1','group2')
          write.csv(df , file, row.names = F, fileEncoding = "GB18030")
          
        } )
      
    } ) }