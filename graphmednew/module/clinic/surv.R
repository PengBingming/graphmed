
# install.packages(c("survival", "survminer"))

library("survival")
library("survminer")
# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

surv_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，选择时间、状态和分组列。"))
                 
    )
    ) )
  
}

survUI <- function(id) {
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
                     dropdownButton(circle=FALSE, label=lang$t("参数设置"), br(),br(),
                     selectInput(ns("time"),   lang$t("时间"), c("") ),
                     selectInput(ns("status"), lang$t("状态"), c("") ),
                     selectInput(ns("group"),  lang$t("分组"), c("") ) ),br(),
                     downloadButton(ns("downloadSampleData"),lang$t("参考数据")),br(),br()
               )
      ) 
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("图形"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     dropdownButton(circle=FALSE, label=lang$t("参数设置"), br(),br(),
                      selectInput(ns("conf.int"),  lang$t("置信区间"),selected ="T", 
                                  choices = c("T","F") ),
                      selectInput(ns("pval"),  lang$t("P 值"),selected ="T", 
                                  choices = c("T","F") ),
                      selectInput(ns("risk.table"),  lang$t("风险表"),selected ="T", 
                                  choices = c("T","F") ),
                      selectInput(ns("add.all"),  lang$t("添加整体"),selected ="T", 
                                  choices = c("T","F") ),
                      selectInput(ns("median.line"),  lang$t("中位线"),selected ="hv", 
                                  choices = c("none", "hv", "h", "v") )
                      ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("图形标签"),icon = icon('image'),  br(),br(),
                      selectInput(ns("palette"),  lang$t("调色板"),selected ="hue", 
                                   c("hue","grey","npg","aaas","lancet","jco",
                                     "ucscgb","uchicago","simpsons","rickandmorty") ),
                      textInput(ns('xlab'),label = lang$t('x轴标签'),value = "Follow up time(d)" ),
                      textInput(ns('legend'),label = lang$t('图例'),value = "group" ) 
                      ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata"), label = "Rdata", size='sm', block=TRUE )
                     )
                 ) ) 
               ),
      tabPanel(title = 'Cumulative curve',
               fluidRow(
                 box(title=lang$t("图形"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot1"),height = 500 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit1"), label = lang$t("开始画图"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                         dropdownButton(circle=FALSE, label=lang$t("参数设置"), br(),br(),
                                        selectInput(ns("conf.int1"),  lang$t("置信区间"),selected ="T", 
                                                    choices = c("T","F") ),
                                        selectInput(ns("pval1"),  lang$t("P 值"),selected ="T", 
                                                    choices = c("T","F") ),
                                        selectInput(ns("risk.table1"),  lang$t("风险表"),selected ="T", 
                                                    choices = c("T","F") ),
                                        selectInput(ns("add.all1"),  lang$t("添加整体"),selected ="T", 
                                                    choices = c("T","F") )
                         ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("图形标签"),icon = icon('image'),  br(),br(),
                                    selectInput(ns("palette1"),  lang$t("调色板"),selected ="hue", 
                                                c("hue","grey","npg","aaas","lancet","jco",
                                                  "ucscgb","uchicago","simpsons","rickandmorty") ),
                                    textInput(ns('xlab1'),label = lang$t('x轴标签'),value = "Follow up time(d)" ),
                                    textInput(ns('legend1'),label = lang$t('图例'),value = "group" ) 
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w1'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h1'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi1'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf1") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png1") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg1"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff1"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata1"), label = "Rdata", size='sm', block=TRUE )
                     )
                     
                 ) ) 
      ),
      tabPanel(title = 'Help', surv_helpUI("sur"))
      ) ) }

survServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data # 读取数据
        data <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            data <- read.csv('./www/surv.csv')
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
            rhandsontable(data(),rowHeaderWidth = 22,  height = 400) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
        
        
        observe({
          if(!is.null(input$table ) ){
            data <- as.data.frame(hot_to_r( input$table ) )
            
            # 实验组与对照组情况
            updateSelectInput(session, "time", label = '时间', choices = colnames(data), selected = colnames(data)[1]  )
            updateSelectInput(session, "status"  , label = '状态', choices = colnames(data) , selected = colnames(data)[2]  )
            updateSelectInput(session, "group"  , label = '分组', choices = colnames(data) , selected = colnames(data)[3]  )
          }
        } ) 
        
        observeEvent(input$submit, {
          
          plot <- reactive({
            data <- hot_to_r( input$table)
            expr <- paste0("survfit(Surv(",input$time,",",input$status,") ~ ",input$group,", data = data)" )
            fit <- eval( parse(text = expr ) )
            
            p <- ggsurvplot(fit, # 创建的拟合对象
                       data = data,  # 指定变量数据来源
                       conf.int = (input$conf.int=='T'), # 显示置信区间
                       pval = (input$pval =='T'), # 添加 P 值
                       surv.median.line = input$median.line,  # 添加中位生存时间线
                       risk.table = (input$risk.table =='T'), # 添加风险表
                       add.all = (input$add.all =='T'), # 添加总患者生存曲线
                       palette = input$palette,
                       xlab = input$xlab, # 指定x轴标签
                       legend.title = input$legend # 设置图例标题
                       ) 
            
            return(p)
            
          })
          
          output$plot <- renderPlot({
            return(plot() )
          })
          
          if(T){
            output$pdf <- downloadHandler(
              filename="plot.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="plot.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="plot.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="plot.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }  )
            
            output$rdata <- downloadHandler( 
              filename="sur.rdata",
              content = function(file){
                p <- plot()
                save(p,file = file)
              } )
          }

        })
        
        observeEvent(input$submit1, {
          # 累计曲线
          plot1 <- reactive({
            data <- hot_to_r( input$table)
            expr <- paste0("survfit(Surv(",input$time,",",input$status,") ~ ",input$group,", data = data)" )
            fit <- eval( parse(text = expr ) )
            
            p <-  ggsurvplot(fit, # 创建的拟合对象  
                             data = data,  # 指定变量数据来源
                             conf.int = (input$conf.int1=='T'), # 显示置信区间
                             pval = (input$pval1 =='T'), # 添加 P 值
                             risk.table = (input$risk.table1 =='T'), # 添加风险表
                             add.all = (input$add.all1 =='T'), # 添加总患者生存曲线
                             palette = input$palette1,
                             xlab = input$xlab1, # 指定x轴标签
                             legend.title = input$legend1, # 设置图例标题
                             fun = "cumhaz") 
              
            return(p)
          })

          output$plot1 <- renderPlot({
            return(plot1() )
          })
          
          if(T){
            output$pdf1 <- downloadHandler(
              filename="plot1.pdf",
              content = function(file){
                pdf(file,width=input$w1,height=input$h1)
                print(plot1() )
                dev.off()
              }
            )
            output$png1 <- downloadHandler(
              filename="plot1.png",
              content = function(file){
                png(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }
            )
            output$jpeg1 <- downloadHandler(
              filename="plot1.jpeg",
              content = function(file){
                jpeg(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }
            )
            output$tiff1 <- downloadHandler( 
              filename="plot1.tiff",
              content = function(file){
                tiff(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }  )
            
            output$rdata1 <- downloadHandler( 
              filename="roc1.rdata",
              content = function(file){
                p <- plot1()
                save(p,file = file)
              } )
            
          }
          

          })
        
      }) # show
      
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('surv.csv')
        } ,
        content = function(file) {
          data <- read.csv('./www/surv.csv')
          write.csv(data , file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
    } )
}


