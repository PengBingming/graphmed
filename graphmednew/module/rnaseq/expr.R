
library(readxl)
library("ggsci")
library(ggpubr)

color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

expr_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、点击“Show Data”可查看参考数据或输入的数据。")),
                 tags$h6(lang$t("2、对照参考数据，矩阵 “ID”列名不可改变，样本名与分组 sample 列对应。")),
                 tags$h6(lang$t("3、矩阵 ID 列“基因名”不要有重复，若存在重复，则取平均。"))
    )
    ) )
  
}

exprUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("表达矩阵"),width=6,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("exp") ) ) 
                 ),
                 box(title=lang$t("分组信息"),width=3,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("group") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("表达矩阵"),multiple = FALSE ),
                     fileInput(ns("file2"), label = lang$t("分组信息"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     dropdownButton(circle=FALSE, label=lang$t("参考数据"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    downloadBttn(ns("downloadExp"), lang$t("矩阵"), size='sm', block=TRUE),
                                    downloadBttn(ns("downloadGroup"), lang$t("分组"), size='sm', block=TRUE) )
                 ) )
               ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("箱图"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput( inputId = ns("gene"),  label = lang$t('选择基因'), c(""),multiple = F),
                     selectInput( inputId = ns("method"),  label = lang$t('检验方法'),selected = "anova",
                                  c('none'='wu',"anova",'t.test','wilcox.test','kruskal.test') ),
                     dropdownButton(circle=FALSE, label=lang$t("图形标签"), icon = icon("image"),
                                    br(),br() ,
                                    numericInput(inputId = ns('label.x'),label = lang$t('p 值 x 坐标'),value = 0),
                                    numericInput(inputId = ns('label.y'),label = lang$t('p 值 y 坐标'),value = 0),
                                    textInput(ns('title'),lang$t("标题"),value = ''),
                                    textInput(ns('xlab'),lang$t("x 轴标签"),value = ''),
                                    textInput(ns('ylab'),lang$t("y 轴标签"),value = ''),
                                    selectInput(ns('theme'),lang$t('主题'),selected = 'bw',
                                                choices = theme_select ),
                                    selectInput(ns("color_type"), 'color_type', color_type ) 
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
                 ) ) # fluidRow    
      ),
      tabPanel(title = lang$t("使用说明"), expr_helpUI("expr"), icon = ionicon(name="information-circle") )
       )
  )  # tagList
} # function(id)


exprServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data
        # 读取数据
        exp <- reactive({
          file1 <- input$file1
          
          if ( is.null(file1) ){
            exp <- data.frame(matrix(1:70,nrow=10)) # 生成一个表达矩阵
            
            colnames(exp )=c('ID',paste0("trt",1:3),paste0("untrt",1:3)) # 设置列名，样本名字
            exp[,"ID"] <- paste0("gene",1:10)
            }
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              exp <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              exp <- data.frame( read_excel(file1$datapath,1) )
            } 
          } # else

          return( exp )
        })
        if(!is.null(exp()) ){
        output$exp<- renderRHandsontable(
          rhandsontable( exp() ,rowHeaderWidth = 50, height = 410) %>% 
            hot_cols(columnSorting = TRUE) %>% 
            hot_col(setdiff(colnames( exp() ),"ID"),type = 'numeric') %>% 
            hot_col("ID",type = 'dropdown')
        )
        }
        
        group <- reactive({
          file2 <- input$file2
          
          if ( is.null(file2) ){
            group <- data.frame(sample=c(paste0("trt",1:3),paste0("untrt",1:3)),
                                condition=c( rep('trt',3),rep('untrt',3) ) )
          }
          else{
            d <- tail( unlist(strsplit(file2$datapath,'[.]') ), 1)
              if( d=='csv' ){
              group <- data.frame( read.csv(file2$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              group <- data.frame( read_excel(file2$datapath,1) )
            } 
          } # else
          colnames(group) <- tolower(colnames(group))
          return(group)
        })
        if(!is.null(group()) ){
          output$group <- renderRHandsontable(
            rhandsontable( group() ,rowHeaderWidth = 50, height = 410) %>% 
              hot_cols(columnSorting = TRUE) %>% 
              hot_col(c('sample','condition'),type = 'dropdown')
          )  }
 
        observe({
          
          if(is.null(exp() ) ) {return(NULL) }  
          exp <- exp()
          gene <- exp$ID 
          
          updateSelectInput(session, "gene",label = '选择基因',choices = gene, selected = gene[1] )
          

        } ) 
        
        observeEvent(input$submit1, { 
          
          logi <- reactive({
            logi <- c(!is.null(input$file1) & !is.null(input$file2))|c(is.null(input$file1) &is.null(input$file2) )
          })
          
          if( logi() ){
        # 基因表达箱图函数
        plot <- reactive({
          if(is.null(input$exp)){return(NULL)}
          
          group <- as.data.frame( hot_to_r(input$group ) )
          group <- group[order(group$condition),]
          
          exp <- as.data.frame( hot_to_r(input$exp) )
          exp <- exp[,c("ID",group$sample ) ]
          
          exp <-   avereps(exp[ , setdiff(colnames(exp),'ID')], ID = exp$ID )
          name <- input$gene
          
            df = data.frame(gene = exp[name,], group = group$condition )
            p <- ggplot(df,aes(x=group , y=gene ) ) + 
              geom_point(aes(color = group) ) + 
              geom_jitter(aes(color = group) ) + 
              geom_boxplot(aes(color = group) ) 
            if(!input$title==''){
              p <- p + ggtitle(input$title )
            }
            
            if(!input$ylab==''){
              p <- p + ylab(label = input$ylab)
            }
            else{
              p <- p + ylab(label = name)
            }
            
            if(!input$xlab==''){
              p <- p + xlab(label = input$xlab)
            }
   
            p <- p + eval(parse(text = paste0('theme_',input$theme,"()") ))

            p <- p +  
              theme(
                title = element_text(size = 40),
                axis.title = element_text(size = 25),
                axis.text  = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20)
              )
              if(!input$method=='wu'){
                #  Add p-value
                if(!input$label.x==0 & !input$label.y==0){
                  p <- p+ stat_compare_means(method = input$method,size=8,
                                             label.x = input$label.x, label.y = input$label.y)
                }else{
                  p <- p+ stat_compare_means(method = input$method,size=8)
                }
                 
              }
            p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
            
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
            }  )
          
          output$rdata <- downloadHandler( 
            filename="expr.rdata",
            content = function(file){
              p <- plot()
              save(p,file = file)
            } )
          
        } # 下载图形

          } # if( logi())
        })
        
        }) # observeEvent(input$show, 
      

      # 2、下载参考数据
      output$downloadExp <- downloadHandler(
        filename = function() {
          paste('exp.csv')
        } ,
        content = function(file) {
          exp <- data.frame(matrix(1:70,nrow=10)) # 生成一个表达矩阵
          colnames(exp)=c('ID',paste0("trt",1:3),paste0("untrt",1:3)) # 设置列名，样本名字
          exp[,"ID"] <- paste0("gene",1:10)

          write.csv( exp, file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
      # 2、下载参考数据
      output$downloadGroup <- downloadHandler(
        filename = function() {
          paste('group.csv')
        } ,
        content = function(file) {
          
          group <- data.frame(sample=c(paste0("trt",1:3),paste0("untrt",1:3)),
                              condition=c( rep('trt',3),rep('untrt',3) ) )
          write.csv(group , file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
  
    } # function(input, output, session)
  ) # moduleServer
} # function(id)


