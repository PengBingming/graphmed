
library('rhandsontable')
library('rstatix') # 添加 P 值
# library('readxl')
library('ggplot2')
library('ggpubr')
library("ggsci")
# library(showtext) # 解决画图乱码
showtext::showtext_auto()

anovaServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

   observeEvent(input$show, { 
      
     # Load the data # 读取数据
      data <- reactive({
        file1 <- input$file1
        if ( is.null(file1) ){
          
          if(input$num=='two'){
            data <- readRDS('www/anova.RDS')
            data <- data[which(!data$group=='setosa'),]
          }
          else if(input$num=='multi'){
            data <- readRDS('www/anova.RDS')
          }
        } 
        else{
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          if( d=='csv' ){
            data <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
          } else{
            data <- data.frame( readxl::read_excel(file1$datapath,1) )
          } 
        } # else
        return( data )
      })
      
      # 输入数据
      if(!is.null(data() ) ){
        output$table <- rhandsontable::renderRHandsontable(
          rhandsontable(data(),rowHeaderWidth = 22, height = 400) %>% 
            hot_cols(columnSorting = TRUE) %>% 
            hot_col("value",type = 'numeric') %>% 
            hot_col("group",type = 'dropdown')
        )
      }
      
      observeEvent(input$submit, {

        result  <- reactive({
          # Perform ANOVA
          # 方差分析
          data  <- hot_to_r( input$table )
          df <- as.data.frame( data  )
          data <- na.omit(data )
          data$group <- factor(data$group)
          
          if(input$num=='two'){
            
            if(input$method=='t_anova'){
              
              t.test <- t.test( value ~ group,data = data , piar = c(input$pair=='T') ,alternative = input$alt)
              test_table <- t(as.data.frame( unlist(t.test)[1:3]))
              stat.test <- test_table
              
              # 画图
              plot <- ggplot(data=data, aes(x= group, y= value, group = group, color= group ) ) +
                geom_boxplot(alpha = 1, size = input$box ) +
                stat_boxplot(geom = 'errorbar' , size = input$errorbar, width = .3) +
                geom_violin(aes( (fill= group),col=group), alpha = .1, size=input$violin ) +
                geom_jitter(size = input$point, alpha= .5 , position =  position_jitter(width = .1) ) +
                ggtitle(input$title) +
                labs(x= input$group ,y= input$value)+
                theme_classic(base_size =  18) +
                stat_compare_means(method = 't.test',size=8 )  # 添加 P 值。 library(ggpubr)
                
                plot <- plot + eval(parse(text = paste0("theme_",input$theme,"()")))
              
                plot <- plot +
                theme(legend.position = "none",
                      plot.title = element_text(size = 30, hjust = 0.5) ,
                      axis.title = element_text(size = 20),
                      axis.text  = element_text(size = 20)
                )+ 
                scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) # 调整y轴
            }
            
            else if(input$method=='wilcox'){
              wilcox.test <- wilcox.test( value ~ group,data = data, piar= c(input$pair=='T'),alternative = input$alt )
              test_table <- t(as.data.frame( unlist(wilcox.test)[1:2]))
              stat.test <- test_table
              # 画图
              plot <- ggplot(data=data, aes(x= group, y= value, group = group, color= group ) ) +
                geom_boxplot(alpha = 1, size = input$box ) +
                stat_boxplot(geom = 'errorbar' , size = input$errorbar, width = .3) +
                geom_violin(aes( (fill= group),col=group), alpha = .1, size=input$violin ) +
                geom_jitter(size = input$point, alpha= .5 , position =  position_jitter(width = .1) ) +
                ggtitle(input$title) +
                labs(x= input$group ,y= input$value)+
                theme_classic(base_size =  18) +
                stat_compare_means( size=8) # 添加 P 值。 library(ggpubr)
              
                plot <- plot + eval(parse(text = paste0("theme_",input$theme,"()")))
              
                plot <- plot +
                theme(legend.position = "none",
                      plot.title = element_text(size = 30, hjust = 0.5) ,
                      axis.title = element_text(size = 20),
                      axis.text  = element_text(size = 20)
                )+ 
                scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) #调整y轴
            }
          }
          else if(input$num=='multi'){
            
            if(input$method=='t_anova'){
              
              aov1 <- aov( value ~ group,data = data )
              
              # 统计结果
              test_table <- summary(aov1)[[1]]
              test_table<- cbind('Item' =rownames(test_table), test_table)  
              
              stat.test <- pairwise_t_test(data,value~group, p.adjust.method =  input$padj, paired = (input$pair=="T")) %>% add_y_position()
              logi <- (min(stat.test$p.adj) < 0.05)
              
              # 画图
              plot <- ggplot(data=data, aes(x= group, y= value, group = group, color= group ) ) +
                geom_boxplot(alpha = 1, size = input$box ) +
                stat_boxplot(geom = 'errorbar' , linewidth = input$errorbar, width = .3) +
                geom_violin(aes( (fill= group),col=group), alpha = .1, size=input$violin ) +
                geom_jitter(size = input$point, alpha= .5 , position =  position_jitter(width = .1) ) +
                ggtitle(input$title) +
                labs(x= input$group ,y= input$value)+
                theme_classic(base_size =  18) +
                stat_compare_means(method = 'anova',size=8 ) # 添加 P 值。 library(ggpubr)
                
                plot <- plot + eval(parse(text = paste0("theme_",input$theme,"()")))
                
                plot <- plot +
                  theme(legend.position = "none",
                      plot.title = element_text(size = 30, hjust = 0.5) ,
                      axis.title = element_text(size = 20),
                      axis.text  = element_text(size = 20)
                ) + 
                stat_pvalue_manual(stat.test,label = "p.adj.signif", tip.length = 0.01,
                                   hide.ns =  logi ,remove.bracket = (input$star=='F') , size = input$star_size) + #隐藏无意义的p值
                scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) #调整y轴
            }
            
            else if(input$method=='wilcox'){
              
              test_table <- kruskal.test(data= data, value~group) %>% unlist() %>% as.data.frame()
              test_table<- cbind('Item' =rownames(test_table), test_table)  
              
              stat.test <- data %>% pairwise_wilcox_test(value~group, p.adjust.method = input$padj, paired = (input$pair=='T')) %>% add_y_position()
              logi <- (min(stat.test$p.adj) < 0.05)
              
              # 画图
              plot <- ggplot(data=data, aes(x= group, y= value, group = group, color= group ) ) +
                geom_boxplot(alpha = 1, size = input$box ) +
                stat_boxplot(geom = 'errorbar' , size = input$errorbar, width = .3) +
                geom_violin(aes( (fill= group),col=group), alpha = .1, size=input$violin ) +
                geom_jitter(size = input$point, alpha= .5 , position =  position_jitter(width = .1) ) +
                ggtitle(input$title) +
                labs(x= input$group ,y= input$value)+
                theme_classic(base_size =  18) +
                stat_compare_means(size=8)  # 添加 P 值。 library(ggpubr)
                
                plot <- plot + eval(parse(text = paste0("theme_",input$theme,"()")))
              
                plot <- plot +  theme(legend.position = "none",
                      plot.title = element_text(size = 30, hjust = 0.5) ,
                      axis.title = element_text(size = 20),
                      axis.text  = element_text(size = 20)
                ) + 
                stat_pvalue_manual(stat.test,label = "p.adj.signif", tip.length = 0.01,
                                   hide.ns =  logi , remove.bracket = (input$star=='F'),size = input$star_size ) + #隐藏无意义的p值
                scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) #调整y轴
            }
            
          }

          # 配色
          plot <- plot + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
          
          result <- list()
          result$plot <- plot
          
          result$stat.test <- stat.test
          result$test_table <-test_table
          
          return(result )
          
        } )
        
        output$stat.test <- renderDataTable({
          
          result <- result()
          stat.test <- result$stat.test 
          
          return( stat.test )
          
        } ) # 统计结果
        
        output$test_table <- renderDataTable({
          
          result <- result()
          test_table <- result$test_table
          
          return( test_table )
          
        } ) # 统计结果
        
        output$plot <- renderPlot( {
          plot <- result()$plot
          
          return( plot )
        })
        
        # 3.1、下载差异结果1
        output$downloadcontent1 <- downloadHandler(
          
          filename = function() {
            paste('analysis.csv')
          } ,
          
          content = function(file) {
            
            result <- result()
            test_table <-  result$test_table
            
            # 输出
            write.csv(test_table , file, row.names = F, fileEncoding = 'GB18030')
            
          }  ) 
        
        # 3.2、下载差异结果2
        output$downloadcontent2 <- downloadHandler(
          
          filename = function() {
            paste('contrast.csv')
          } ,
          
          content = function(file) {
            
            result <- result()
            stat.test <- as.matrix( result$stat.test )
            stat.test[,11] <- gsub(',','vs', stat.test[,11])
            # 输出
            write.csv(stat.test , file, row.names = F, fileEncoding = 'GB18030')
            
          }  ) 
        
        if(T){
          output$pdf <- downloadHandler(
            filename="test.pdf",
            content = function(file){
              pdf(file,width=input$w,height=input$h)
              print(result()$plot)
              dev.off()
            }
          )
          output$png <- downloadHandler(
            filename="test.png",
            content = function(file){
              png(file,width=input$w,height=input$h,units="in",res=input$ppi)
              print(result()$plot)
              dev.off()
            }
          )
          output$jpeg <- downloadHandler(
            filename="test.jpeg",
            content = function(file){
              jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
              print(result()$plot)
              dev.off()
            }
          )
          output$tiff <- downloadHandler( 
            filename="test.tiff",
            content = function(file){
              tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
              print(result()$plot)
              dev.off()
            }
          )
        }

      })
      
})
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        } ,
        content = function(file) {
          if(input$num=='two'){
            data <- readRDS('www/anova.RDS')
            data <- data[which(!data$group=='setosa'),]
          }
          else if(input$num=='multi'){
            data <- readRDS('www/anova.RDS')
          }
          
          write.csv(data , file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
    } # function(input, output, session)
) # moduleServer
} # function(id)
