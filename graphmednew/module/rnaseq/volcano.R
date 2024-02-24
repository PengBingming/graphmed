
options(digits = 10)
library(ggplot2) # 基因 ID 转换、富集
library(readxl)    # 人数据库 hsa
library(ggrepel)
library("ggsci")

library(showtext) # 解决画图乱码
showtext_auto()

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

volcano_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6("1、点击“Show Data”可查看参考数据或输入的数据。"),
                 tags$h6("2、对照参考数据，矩阵 “ID”列名不可改变，样本名与分组 sample 列对应。"),
                 tags$h6("3、矩阵 ID 列“基因名”不要有重复，可自定义排序方法（logFC / pvalue）。")
    )
    ) )
  
}

volcanoUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("DEG") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(inputId = ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data",style = "fill",
                                 color = "primary", size = "sm" ),hr(),
                     downloadButton(ns("downloadSampleData"), lang$t("参考数据"))

                     )
               ) # fluidRow
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("火山图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot") ,height = 500) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("order"),label = lang$t("基因排序方式："),selected = "pvalue",
                                 choices = c('pvalue'='pvalue',"logFC"="logFC") ),
                     dropdownButton(circle=FALSE,label=lang$t("图形参数"),  br(),br(),
                                    numericInput(ns("pvalue"), lang$t("P 值"), value = 0.05),
                                    numericInput(ns("padj")  , lang$t("矫正 P 值"), value = 0.05),
                                    numericInput(ns("logFC"),label = "logFC", value = 1),
                                    numericInput(ns("volcano_num"),label = lang$t("火山图标签数"), value = 10)
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("图形标签"),icon = icon('image'),  br(),br(),                     selectInput(ns('theme'),lang$t('主题'),selected = 'bw',choices = theme_select ),
                                    selectInput(ns("color_down"), 'color_down', colors(),selected = 'blue' ) ,
                                    selectInput(ns("color_not"), 'color_not', colors() ,selected = 'black') ,
                                    selectInput(ns("color_up"), 'color_up', colors() ,'red')
                                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata"), label = "Rdata", size='sm', block=TRUE )
                     )
               )
      )
    ),
    tabPanel(title = 'Help', volcano_helpUI("volcano")) 
  ) # bs4Dash::tabsetPanel
  )# tagList
}



volcanoServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
      
      DEG <- reactive({
        file1 <- input$file1
        if(is.null(file1) ){
          DEG <- read.csv('./www/DEG.csv')
          # DEG <- na.omit(DEG)
        }
        else{
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          if(d =='csv'){
            DEG <- data.frame( read.csv(file1$datapath,1) )
          }
          else{
            DEG <- data.frame( read_excel(file1$datapath,1) )
          }
        }
        
        return(DEG)
      })
      
      output$DEG <- renderDataTable(
             return(DEG() )
      )
      
      observeEvent(input$submit, { 
        
        plot <- reactive({
          
          DEG <- DEG()
          if(length(intersect(colnames(DEG),"logFC") ) > 0){
            colnames(DEG)[which(colnames(DEG)=="logFC")] <- 'log2FoldChange'
          }
          if(length(intersect(colnames(DEG),"P.Value") ) > 0){
            colnames(DEG)[which(colnames(DEG)=="P.Value")] <- 'pvalue'
          }
          if(length(intersect(colnames(DEG),"adj.P.Val") ) > 0){
            colnames(DEG)[which(colnames(DEG)=="adj.P.Val")] <- 'padj'
          }
          
          DEG$gene <- DEG$ID
          DEG$FC <- abs(DEG$log2FoldChange )
          
          DEG$change <- as.factor(
            ifelse(
              DEG$pvalue < input$pvalue & DEG$padj < input$padj & abs(DEG$log2FoldChange) >= input$logFC,
              ifelse(
                DEG$log2FoldChange >= input$logFC,'UP','DOWN'),
              'NOT'))
          # table(DEG$change ) # 查看基因上、下调情况
          
          # 设置火山图的标题
          this_tile=paste('Cutoff for logFC is ',round(input$logFC,3),
                          '\nThe number of up gene is ',nrow(DEG[DEG$change=='UP',]),
                          '\nThe number of down gene is ',nrow(DEG[DEG$change=='DOWN',]))
          
          if(input$order=='pvalue'){
            DEG <- DEG[order(DEG$pvalue),]
          }
          else if(input$order=='logFC'){
            DEG <- DEG[order(DEG$FC,decreasing = T),]
          }
        
            # 画火山图
            p <- ggplot(data=DEG,aes(x = log2FoldChange, y = -log10( pvalue ), color= change)) +
              geom_point(alpha=0.4,size=1)   # 绘制点图

            p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
            
            p <- p +  xlab("log2 fold change")+
              ylab("-log10 pvalue") +    # 轴标签
              ggtitle(this_tile)+
              geom_text_repel(
                data = DEG[ DEG$pvalue < input$pvalue &  DEG$padj < input$padj &  DEG$FC > input$logFC,][1:input$volcano_num,],
                aes(label = gene),
                size = 4.5,
                color = "black",
                segment.color = "black", show.legend = FALSE )+ # 添加关注的点的基因名
              theme(plot.title=element_text(size=25, hjust=0.5),
                    axis.title = element_text(size = 25),
                    axis.text = element_text(size = 15) ) +
              scale_color_manual(values=c(input$color_down,input$color_not,input$color_up ) )   # 设定颜色

          return(p)
          
        })
        
        output$plot <- renderPlot({
          return(plot() )
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
            } )
          output$rdata <- downloadHandler( 
            filename="volanco.rdata",
            content = function(file){
              volanco <- plot()
              save(volanco,file = file)
            } )
        }
        
        })

      
      } )# show
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('DEG.csv')
        },
        content = function(file) {
          data <- read.csv(('./www/DEG.csv'))
          write.csv(data, file, row.names = F, fileEncoding = "GB18030")
        }
      )
      
    })
}


