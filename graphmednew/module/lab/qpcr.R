
library(readxl)
library(reshape)
library(rhandsontable)
library(ggplot2)
library(ggpubr)

# File with translations
# lang <- Translator$new(translation_csvs_path = "./lang/")
# lang$set_translation_language("cn") # here you select the default translation to display

# 编写
myfun1 <- function(data, input){

  # 去重复
  df1 <- data
  colnames(df1) <- toupper(colnames(df1))

  dfs <- df1[which(df1$CQ.STD.DEV < input$Cq.Std.Dev), -which(colnames(df1)=='CQ.STD.DEV')]
  
  df1 <- melt( dfs, id.vars = c('GROUP','SAMPLE','TARGET') )
  
  m = t( reshape::cast(df1,TARGET~GROUP+SAMPLE,mean) )
  m <- as.data.frame.array(m)
  
  # 计算过程变量
  dat1 <- data.frame( matrix(0, ncol = 4*(ncol(m)-1) , nrow = nrow(m) )   )
  rownames(dat1) <- rownames(m)
  
  gene <- setdiff(  unique(df1$TARGET) , input$gene )
  
  n1 <- vector()
  for (i in gene ) {
    n1 <- c(n1 , paste0(  i ,"_",c("▲ct","2^(-▲ct)","mean","2^(-▲▲ct)" )))
  }


  
  colnames(dat1 ) <- n1
  
  m <- cbind(Sample=do.call( rbind, strsplit( rownames(m),'_') )[, 2],m)
  m <- cbind(Group=do.call( rbind, strsplit( rownames(m),'_') )[, 1],m)
  
  m <- cbind(m, dat1)
  
  for (i in 1:length(gene) ) {
    
    n <- i*4+ length(gene) 
    
    m[ , n] <-      m[, gene[i] ] - m[, input$gene]  # ▲ct
    
    m[ , n+1] <- 2^(-(m[, gene[i] ] - m[, input$gene]) ) # '2^(-▲ct)'
    
    
    mean <- mean(na.omit( m[ which(m$Group == input$group),n+1 ]) ) # 对照组 '2^(-▲ct)' 平均值
    
    m[ which(m$Group == input$group), n+2 ] <- mean # 赋值
    
    m[ , n+3 ] <- m[ , n+1] / mean    # 所有组除以对照组平均（'2^(-▲ct)'）
    
  }
  
  result <- list()
  result$dat1 <- m
  result$dat2 <- m[, c( 1:(3+length(gene) ), 1:length(gene)*4+(3+length(gene) ) ) ]
  
  return(result)
  
}

qpcr_dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("sample") ) ) 
      ),
      box(width = 3,status="success",
          fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
          h6(lang$t('格式：.csv .xlsx .xls')),
          actionBttn( inputId = ns("show"), label = "Show Data",
                      style = "fill", color = "primary", size = "sm" ), hr(),
          dropdownButton(circle=FALSE,label=lang$t("参数设置"),  br(),br(),
                         numericInput(inputId = ns("Cq.Std.Dev"),label = "Cq.Std.Dev < ", value = 0.5),
                         selectInput(ns("gene") , lang$t("内参基因"), c("") ),
                         selectInput(ns("group"), lang$t("对照样本"), c("") )
          ) ,br(),
          downloadButton(ns("downloadSampleData"), lang$t("参考数据")),br(),br(),
          dropdownButton( label = lang$t("使用说明"), icon = icon('tachometer-alt'), circle = FALSE,br(),br(),
                          h6("1、参考数据中，共5列，列名分别为Group、Target、Sample、Cq、Cq.Std.Dev，
                             不可改变列名，列数不可减少或增多。
                             "),
                          h6("2、Group 为分组信息，自行添加；Target 为基因；Sample 为样本，
                              相同则为 同一样本的副孔；Cq 为 Ct 值；Cq.Std.Dev 为 Cq/Ct 的标准偏差。")
          )
      ) )
  )
}

qpcr_resultUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("结果数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), dataTableOutput(ns("results") ) ) ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit"), label = "Analyze Data",
                      style = "fill", color = "primary", size = "sm" ), hr(),
          radioButtons(ns("type"), lang$t("结果选择"), selected = "dat2",
                       choices = c('result1' = "dat2", 'result2' = "dat1") ),
          downloadButton(ns("downloadData"), lang$t("结果数据")) 
      ) )
  )
}

qpcr_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("表达图形"),width=9,solidHeader=TRUE,status='primary',background = "white",
          splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) 
      ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit1"), label = lang$t("开始画图"),
                      style = "fill", color = "primary", size = "sm" ),hr(),
          selectInput(ns("select"), lang$t("目的基因"), c("") ),
          dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                         br(),br() ,
                         numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                         numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                         numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                         downloadBttn(ns("downloadplot"),  label = ".html",size='sm', block=TRUE),
                         downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                         downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
          )
      )
    ) # fluidRow
  )
}

qpcr_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，共5列，列名分别为Group、Target、Sample、Cq、Cq.Std.Dev，
                             不可改变列名，列数不可减少或增多。")),
                 tags$h6(lang$t("2、Group 为分组信息，自行添加；Target 为基因；Sample 为样本，
                              相同则为 同一样本的副孔；Cq 为 Ct 值；Cq.Std.Dev 为 Cq/Ct 的标准偏差。")),
                 tags$h6(lang$t("3、选择过滤指标、内参基因和对照组（参数设置）后运行。"))
                 
                 
    )
    ) )
  
}

qpcrUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', qpcr_dataUI("qpcr") ),
      tabPanel(title = 'Result', qpcr_resultUI("qpcr") ),
      tabPanel(title = 'Plot', qpcr_plotUI("qpcr") ),
      tabPanel(title = lang$t("使用说明"), qpcr_helpUI("qpcr"), icon = ionicon(name="information-circle") )
    )
  ) # NS(id)
} # function(id)

qpcrServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
       # 读取 qpcr数据
      df1 <- reactive({
        
        file1 <- input$file1
        if(is.null(file1)){
          df1 <- read.csv('./www/qpcr_sample.csv')
        }
        else{
          
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          
          if(d=='csv'){
            df1 <- data.frame( read.csv(file1$datapath,1) )
          } else{
            df1 <- data.frame( read_excel(file1$datapath,1) )
          } 
        } 
        return(df1)
      })
      
      output$sample  <- renderRHandsontable(
        rhandsontable(df1() ,rowHeaderWidth = 50, height = 360) %>% 
          hot_cols(columnSorting = TRUE) %>% 
          hot_col(intersect(colnames(df1() ),c("CT","Ct","CQ","Cq","CQ.STD.DEV","Cq.Std.Dev")),type = 'numeric') %>% 
          hot_col(setdiff(colnames(df1() ),c("CT","Ct","CQ","Cq","CQ.STD.DEV","Cq.Std.Dev")),type = 'dropdown')
      )
      
      observeEvent(input$sample, { 
        
        df1 <- hot_to_r(input$sample)
        colnames(df1) <- toupper(colnames(df1))
        
        observe({
          # 实验组与对照组情况
          updateSelectInput(session, "gene",  label = '内参基因', choices = unique(df1$TARGET) , selected = unique(df1$TARGET)[1] )
          updateSelectInput(session, "group", label = '对照样本', choices = unique(df1$GROUP)  , selected = unique(df1$GROUP )[1] )
        })
        
      observeEvent(input$submit, {
        
          # 3、展示计算结果
          output$results <- renderDataTable({
            
            if ( is.null( df1 ) ) { return() }
            result <- myfun1(data=df1, input = input)
            
            if(input$type=='dat1'){
              dat <- result$dat1
            } 
            else if(input$type=='dat2'){
              dat <- result$dat2
            } 
            return(dat)
          })
          
          # 3、输出计算结果
          output$downloadData <- downloadHandler(
            
            filename = function() {
              paste("计算结果.csv")
            },
            content = function(file) {
              result <- myfun1(data=df1, input)
              if(input$type=='dat1'){
                dat <- result$dat1
              } 
              else if(input$type=='dat2'){
                dat <- result$dat2
              } 
                write.csv(dat , file, row.names = F,fileEncoding = 'GB18030')
            } )

        observe({
          
          if ( is.null( df1 ) ) { return() }
          gene <- setdiff( unique(df1$TARGET), input$gene )
          updateSelectInput(session, "select", label = '目的基因', choices =  gene , selected = gene[1] )
      
       observeEvent(input$submit1, { 

         plot <- reactive({
           result <- myfun1(data=df1, input = input)
           
           dat2 <-result$dat2
           
           if(is.null(input$select ) ){ return(NULL) }
           n1 <- which(gene==input$select)
           
           p <- ggplot(dat2, aes(x= Group, y= dat2[,n1+3+length(gene)], color= Group ) ) +
             geom_boxplot(alpha = 1, size = .5 ) +
             geom_jitter(size = .5, alpha= .5 , position =  position_jitter(width = .1) ) +
             ggtitle("") +
             labs( y= paste0( input$select,'/', input$gene) )+
             stat_compare_means( # method = "anova"
             ) +
             theme_classic(base_size =  9)
           
           return( p )
         })
         
         
          output$plot <- renderPlot({
            return(plot() )
          })
          
          # 下载图片
          if(T){
            output$pdf <- downloadHandler(
              filename="qpcr.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="qpcr.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="qpcr.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="qpcr.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
          }
          

          
       } )
       
    } )
   
        
      } )
      
      })
      
      })
      
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('q-pcr参考数据.csv')
        },
        content = function(file) {
          
          df1 <- read.csv('./www/qpcr_sample.csv')
          write.csv(df1 , file, row.names = F,fileEncoding = 'GB18030')
        }
      ) 
      

      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id)

