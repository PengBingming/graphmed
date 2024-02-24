
library(rstatix) # 添加 P 值
library(readxl)
library(ggplot2)
library(ggpubr)
library("ggsci")

library(showtext) # 解决画图乱码
showtext_auto()

color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

lang <- Translator$new(translation_csvs_path = "./lang/info/")

anova_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，列名大小写保持一致，不要改变。")),
                 tags$h6(lang$t("2、首先选择两组还是多组（参数设置）。")),
                 tags$h6(lang$t("3、两组可选择t检验和秩和检验，多组可选择方差分析和秩和检验。")),
                 tags$h6(lang$t("4、多组还可得到两两比较的结果。"))
                 
    )
    ) )
  
}

anovaUI <- function(id) {
    ns <- NS(id)
    shiny.i18n::usei18n(lang)
    tagList(
      bs4Dash::tabsetPanel(
        tabPanel(title = 'Data',
                 fluidRow(
                   box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("table") ) ) ),
                   box(width = 3,status="success",
                       fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                       h6(lang$t('格式：.csv .xlsx .xls')),
                       actionBttn( inputId = ns("show"), label = "Show Data",
                                   style = "fill", color = "primary", size = "sm" ),
                       hr(),
                       dropdownButton(circle=FALSE, label=lang$t("参数设置"), br(),br(),
                                      selectInput(inputId = ns('num'),label = lang$t('样本组数：'),
                                                  c("two group" = 'two', "multi group" = 'multi'), selected = 'multi' ),
                                      selectInput(inputId = ns('method'), label = lang$t('检验方法：'), selected = 't_anova',
                                                  c("t.test/anova" = 't_anova', "wilcox" = 'wilcox')),
                                      selectInput(inputId = ns('pair'),label = lang$t('是否配对：'), 
                                                  c("pair" = 'T', "unpair" = 'F'),selected = 'F' ),
                                      selectInput(inputId = ns('alt'),label = lang$t('单边/双边检验：'), selected = 'two.sided', 
                                                  c("two.sided" = 'two.sided', "less" = 'less', "greater"="greater") ),
                                      selectInput(ns('p.level'),"p.level",selected = '0.001',choices = c("0.001","NULL")) ,
                                      selectInput(inputId = ns('padj'),label = lang$t('校正方法'),selected = 'bonferroni' , 
                                                  c("Bonferroni" = 'bonferroni',  "Holm" = 'holm','hochberg'='hochberg',
                                                    'BH'='BH', 'BY'='BY','fdr'='fdr', 'hommel'='hommel',"none"="none") ),
                                      h6(lang$t("（多组存在差异时，两两比较的校正方法）")) ),br(),
                       downloadButton(ns("downloadSampleData"),lang$t("参考数据")) ) )
                 ),
        tabPanel(title = 'Plot',
                 fluidRow(
                   box(title=lang$t("图形"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                   box(width=3,status="success",
                       actionBttn( inputId = ns("submit"), label = "Analyze Data",
                                   style = "fill", color = "primary", size = "sm" ),hr(),
                       dropdownButton(circle=FALSE,icon=icon('location-pin'), label=lang$t("点线大小"),  br(),br(),
                                      numericInput(inputId = ns('point'),label = lang$t('散点：'), value = 0.5,  min = 0,max = 5),
                                      numericInput(inputId = ns('errorbar'), label = lang$t('误差条：'), value = 0.5,   min = 0,  max = 2) ,
                                      numericInput(inputId = ns('box'), label = lang$t('箱图线条：'), value = 0.5,min = 0,max = 2 ) ,
                                      numericInput(inputId = ns('violin'), label = lang$t('小提琴图线条：'), value = 0.5, min = 0, max = 2 ) ,
                                      numericInput(inputId = ns('star_size'), label = lang$t('星号大小：'),max = 20,min = 0,value = 5 ) ), br(),
                       dropdownButton(circle=FALSE, label=lang$t("图形标签"),icon = icon('image'),  br(),br(),
                                      textInput(inputId = ns('title'), label = lang$t('标题：'),value ="title" ),
                                      textInput(inputId = ns('value'), label = lang$t('y轴标签：'), value ='value' ),
                                      textInput(inputId = ns('group'), label = lang$t('x轴标签：'), value ='group' ),
                                      selectInput(inputId = ns('star'), label = lang$t('显示横杠：'),c('show'='T','hide'="F"),
                                                  selected = "T" ),
                                      selectInput(ns('theme'),lang$t('主题'),selected = 'bw',choices = theme_select ) ,
                                      selectInput(ns("color_type"), 'color_type', color_type ) ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                      br(),br() ,
                                      numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                                      numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                                      numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                                      downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
                       )  ) )
        ),
        tabPanel(title = 'Result',
                 fluidRow(
                   box(title=lang$t("结果数据"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"), dataTableOutput(ns("test_table") ) )  ),
                   box(width=3,
                       downloadButton(ns("downloadcontent1"), lang$t("结果数据")) ),
                   box(title=lang$t("两两比较"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"), dataTableOutput(ns("stat.test") )  )  ),
                   box(width=3,solidHeader=TRUE,
                       downloadButton(ns("downloadcontent2"), lang$t("两两比较")) )
                 ) # fluidRow
        ),
        tabPanel(title = 'Help',anova_helpUI("anova"))
        )
    ) # tagList
} # function(id)

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
            data <- read.csv('./www/anova.csv')
            data <- data[which(!data$group=='setosa'),]
          }
          else if(input$num=='multi'){
            data <- read.csv('./www/anova.csv')
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
      
      # 输入数据
      if(!is.null(data() ) ){
        output$table <- renderRHandsontable(
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
                stat_boxplot(geom = 'errorbar' , size = input$errorbar, width = .3) +
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
            data <- read.csv('./www/anova.csv')
            data <- data[which(!data$group=='setosa'),]
          }
          else if(input$num=='multi'){
            data <- read.csv('./www/anova.csv')
          }
          
          write.csv(data , file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
    } # function(input, output, session)
) # moduleServer
} # function(id)
