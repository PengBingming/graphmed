
# library('readxl')
library('rhandsontable')
library("pROC")
library('ggplot2')     # 画图
library("ggsci")
# library("showtext") #中文问题
showtext::showtext_auto()

roc_logiServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data # 读取数据
        df <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            df <- readRDS('./www/df.RDS')
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
            df <- as.data.frame(hot_to_r( input$table ) )
            
            updateSelectInput(session, 'factor', choices = colnames(df) ,
                              selected = colnames(df)[1] )
            updateSelectInput(session,"numeric" , choices = colnames(df) ,
                              selected = colnames(df)[2:ncol(df)] )
            updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                              selected = "" )
            
            updateSelectInput(session,"group" , choices = colnames(df) ,
                              selected = colnames(df)[1] )
            updateSelectInput(session,"var_select" , choices = colnames(df) ,
                              selected = colnames(df)[2:ncol(df)] )
            
            
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
                
                var_select <- reactive({
                  var <- setdiff(input$var_select,input$group)
                  var_select <- var[1]
                  for (i in 2:length(var)) {
                    var_select <- paste(var_select,var[i],sep="+")
                  }
                  
                  return(var_select)
                })
                
                data_df <- reactive({
                  
                  # model_1 <- glm(outcome~s100b+ndka,data = aSAH, family = binomial(link ="logit") )

                  text <- paste0("glm(",input$group,"~",var_select(),
                                 ",family = binomial(link ='logit'),data = df)")
                  model_1 <- eval(parse(text = text))
                  fitted.prob<-predict(model_1, newdata = df, type = "response")
                  
                  df$pred <- model_1$fitted.values
                  
                  return(df)
                })
                
                roc_list <- reactive({
                  df <- data_df()
                  roc_list <- roc(df[,input$group],df[,"pred"],
                                  direction=input$direction,
                                  ci=TRUE ,aur=TRUE, percent=F)
                  
                  return(roc_list)
                })
                
                data.auc <- reactive({
                  roc.list <- roc_list()
                  
                  data.auc <- as.data.frame(t(data.frame(ci(roc.list)) ))
                  data.auc <- round(data.auc ,3)
                  colnames(data.auc) <- c("ci_lower","auc","ci_upper")
                  data.auc$name <- var_select()
                  
                  return(data.auc)
                })
                
                best.point <- reactive({
                  roc.list <- roc_list()
                  
                  best.point <- coords(roc.list, "best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))
                  best.point <- round(best.point,3)
                  best.point <- data.frame(best.point)
                  colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
                  best.point$name <- var_select()
                  
                  return(best.point)
                })
                
                plot <- reactive({
                  
                  roc_list <-roc_list()
                  data.auc <- data.auc()
                  best.point <- best.point()
                  
                  p <- ggroc(roc_list, legacy.axes = T,
                             linetype = input$linetype,
                             color    = input$linecolor, 
                             size     = input$linesize) +
                    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                                 linetype = input$linetype, 
                                 color    = input$linecolor,
                                 size     = input$linesize, 
                                 show.legend= F)
                  
                  if(input$auc=="show"){
                    p <- p+
                      geom_text(data.auc, hjust = 0.5,
                                show.legend= F,
                                size= input$auc_size, 
                                color   = input$textcolor,
                                mapping= aes(0.6, 0.4, 
                                             label =paste("AUC=",auc,"\n","95% CI: ",ci_lower,"~",ci_upper)) )
                  }
                  
                  if(input$best_point=="show"){
                    p <- p +
                      geom_point(data = best.point,show.legend = F,size= input$point_size/2, 
                                 mapping = aes(x = `1-specificity`,y = sensitivity),
                                 color   = input$textcolor)+
                      geom_text(data = best.point, 
                                show.legend = F,
                                size= input$point_size, 
                                color   = input$textcolor,
                                mapping = aes(x = `1-specificity`+0.1,y = sensitivity,
                                              label=paste(threshold,"\n","(",sensitivity,",",`1-specificity`,")")) )
                    
                  }
                  
                  # # 分面展示 # OR faceting
                  # if(input$facet=="row"){
                  #   p <- p + facet_grid(.~name)
                  # }
                  # else if(input$facet=="column"){
                  #   p <- p + facet_grid(name~.)
                  # }
                  
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
                  
                  # if(!input$fill_name==''){
                  #   fill <- paste0("labs(fill='",input$fill_name,"')")
                  #   p <- p + eval(parse(text = fill ))
                  # }
                  
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
                  
                  
                  output$data_df <- downloadHandler( 
                    filename="data_pred.csv",
                    content = function(file){
                      write.csv(data_df() , file, row.names = F, fileEncoding = "GB18030")
                    }  )
                  output$table0 <- downloadHandler( 
                    filename="data_auc.csv",
                    content = function(file){
                      write.csv(data.auc() , file, row.names = F, fileEncoding = "GB18030")
                    }  )
                  output$table1 <- downloadHandler( 
                    filename="best_point.csv",
                    content = function(file){
                      write.csv(best.point() , file, row.names = F, fileEncoding = "GB18030")
                    }  )
                } # 下载图形
                
              } )
              
            }) # obersve
            
          }
          
        } ) # obersve
        
      })
      
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        } ,
        content = function(file) {
          
          df <- readRDS('./www/df.RDS')
          write.csv(df , file, row.names = F, fileEncoding = "GB18030")
          
        } )
      
    } ) }
