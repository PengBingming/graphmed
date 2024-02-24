
library('RColorBrewer')
library('pheatmap')
library("limma")

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

heatmap_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、ID 列为“基因”列，列名“ID”不可改变，不要重复。"))
                 
    )
    ) )
}

heatmapUI <- function(id) {
    ns <- NS(id)
    shiny.i18n::usei18n(lang)
    tagList(
      bs4Dash::tabsetPanel(
        tabPanel(title = 'Data',
                 fluidRow(
                   box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("data") ) ) ),
                   box(width = 3,status="success",
                       fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                       h6(lang$t('格式：.csv .xlsx .xls')),
                       actionBttn( inputId = ns("show"), label = "Show Data",
                                   style = "fill", color = "primary", size = "sm" ),
                       hr() ,
                       downloadButton(ns("downloadSampleData"), lang$t("参考数据"))
                   ) ) ),
        tabPanel(title = 'Annotation',
                 fluidRow(
                   box(title=lang$t("行注释"),width=4,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("anno_row") ) ) ),
                   box(title=lang$t("列注释"),width=5,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("anno_col") ) ) ),
                   box(width = 3,status="success",
                       dropdownButton(circle=FALSE, label=lang$t("设置行参数"), br(),br() ,
                                      selectInput(ns('rowname'),lang$t("行名"),choices = c("show"='T',"hide"='F'),selected = 'T'),
                                      selectInput(ns('row_cluster'),lang$t('行聚类'),choices = c("Yes"='T',"No"='F'),selected = 'T'),
                                      selectInput(ns('row'),lang$t('添加行注释'), c("Yes"='T',"No"='F'),selected = 'T'),
                                      textInput(ns('row1'),label = lang$t('行注释1'),value = 'cluster'),
                                      textInput(ns('row2'),label = lang$t('行注释2'),value = '')
                       ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("设置列参数"), br(),br() ,
                                      selectInput(ns('colname'),lang$t("列名"),choices = c("show"='T',"hide"='F'),selected = 'T'),
                                      selectInput(ns('col_cluster'),lang$t('列聚类'),choices = c("Yes"='T',"No"='F'),selected = 'F'),
                                      selectInput(ns('col'),lang$t('添加列注释'), c("Yes"='T',"No"='F'),selected = 'T'),
                                      textInput(ns('col1'),label = lang$t('列注释1'),value = 'group'),
                                      textInput(ns('col2'),label = lang$t('列注释2'),value = 'rep')
                       )
                   ) )    ),
        tabPanel(title = 'Heatmap',
                 fluidRow(
                   box(title=lang$t("热图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                   box(width=3,status="success",
                       actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                                   style = "fill", color = "primary", size = "sm" ),hr(),
                       
                       selectInput(ns("scale"),lang$t('标准化'),choices = c("row"='row',"column"='column','none'='none'),selected = 'none'),
                       selectInput(ns('disp_num'),lang$t("显示数值"),choices = c("show"='T',"hide"='F'),selected = 'F'),
                       selectInput(ns("angle_col"),lang$t('列名角度'),choices = c('270', '0', '45', '90', '315'),selected = '45' ),
                       dropdownButton(circle=FALSE, label=lang$t("图形颜色"),icon = icon("image"),
                                      selectInput(ns('color1'),lang$t('低值颜色'),colors(),selected = "navy"),
                                      selectInput(ns('color2'),lang$t('中值颜色'),colors(),selected = "white"),
                                      selectInput(ns('color3'),lang$t('高值颜色'),colors(),selected = "firebrick3") ),br(),
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
                       )  )
                 ) # fluidRow
        ),
        tabPanel(title = 'Help', heatmap_helpUI("heatmap"))
        )
    ) # tagList
  } # function(id)


heatmapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        data <- reactive({
          file1 <- input$file1
          if(is.null(file1) ){
            data <- read.csv('./www/heatmap/heatmap.csv')
          }
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if(d =='csv'){
              data <- data.frame( read.csv(file1$datapath,1) )
            }
            else{
              data <- data.frame( read_excel(file1$datapath,1) )
            }
          }
          
          return(data)
        })
        
        output$data <- renderRHandsontable(
          rhandsontable(data(),rowHeaderWidth = 50, height = 400) %>% 
            hot_cols(columnSorting = TRUE)
        )
        
        anno_col <- reactive({
          if(is.null(data() ) ){return(NULL) }
          if(input$col1==''&input$col2==''){return(NULL)}
          
         data <- data()
         data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)

         if(!input$col1==''&!input$col2==''){
           ifelse( ncol(data) %%2 ==0, 
                   anno_col <-  data.frame( group = rep('',ncol(data) ), rep = rep(1:(ncol(data)/2 ), 2 ) ) , 
                   anno_col <-  data.frame( group = rep('',ncol(data) ), rep = c(1:( (ncol(data)+1)/2),1:( (ncol(data)-1)/2)) )
           )
           colnames(anno_col) <- c(input$col1,input$col2)
         }
         else if(!input$col1==''){
           anno_col <-  data.frame( group = rep('',ncol(data) )  )
           colnames(anno_col) <- input$col1
         }
         else{
             ifelse( ncol(data) %%2 ==0, 
                     anno_col <-  data.frame( rep = rep(1:(ncol(data)/2 ), 2 ) ) , 
                     anno_col <-  data.frame( rep = c(1:( (ncol(data)+1)/2),1:( (ncol(data)-1)/2)) )
             ) 
            colnames(anno_col) <- input$col2

           }
         rownames(anno_col ) <- colnames(data)
         
         return(anno_col )
        } )
        

          output$anno_col <- renderRHandsontable(
            rhandsontable(anno_col(),rowHeaderWidth = 60, height = 240) %>% 
              hot_cols(columnSorting = TRUE) 
          ) 
        
        anno_row <- reactive({
          if(is.null(data() ) ){return(NULL) }
          
          if(input$row1==''&input$row2==''){return(NULL) }
          
          data <- data()
          data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)
          
          if(!input$row1==''& !input$row2==''){
            anno_row = data.frame(
              row1 = rep('', nrow(data) ) ,
              row2 = rep(1, nrow(data) ) 
            )
            colnames(anno_row) <- c(input$row1,input$row2)
            
          }
          else if(!input$row1==''){
            anno_row = data.frame(
              cluster = rep('', nrow(data) )
            )
            colnames(anno_row) <- input$row1
          }
          else if(!input$row2==''){
            anno_row = data.frame(
              cluster = rep('', nrow(data) )
            )
            colnames(anno_row) <- input$row2
          }

          rownames(anno_row ) <- rownames(data)
 
          return(anno_row )
        } )

        output$anno_row <- renderRHandsontable(
          rhandsontable(anno_row(),rowHeaderWidth = 60, height = 240) %>% 
            hot_cols(columnSorting = TRUE)
        )


        observeEvent(input$submit, {
          plot <- reactive({
            
            data <- hot_to_r( input$data )
            data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)
            
            
            
            anno_col <- hot_to_r(input$anno_col )
            anno_row <- hot_to_r(input$anno_row )

            if(input$row=='T'){

              if(input$col=='T'){
                p <- pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                              cluster_cols  = c(input$col_cluster=='T'),
                              cluster_rows  = c(input$row_cluster=='T'),
                              show_colnames = c( input$colname=='T'),
                              show_rownames = c(input$rowname=='T'),
                              annotation_row = anno_row ,
                              annotation_col = anno_col,
                              scale = input$scale,
                              display_numbers = c(input$disp_num=='T'),
                              angle_col = as.numeric(input$angle_col),
                              color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
              else{
                p <-pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                             cluster_cols  = c(input$col_cluster=='T'),
                             cluster_rows  = c(input$row_cluster=='T'),
                             show_colnames = c( input$colname=='T'),
                             show_rownames = c(input$rowname=='T'),
                              annotation_row = anno_row,
                              scale = input$scale,
                              display_numbers = input$disp_num,
                             angle_col = as.numeric(input$angle_col),
                             color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
            }
            else{
              
              if(input$col=='T'){
                p <-  pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                               cluster_cols  = c(input$col_cluster=='T'),
                               cluster_rows  = c(input$row_cluster=='T'),
                               show_colnames = c( input$colname=='T'),
                               show_rownames = c(input$rowname=='T'),
                               annotation_col = anno_col,
                               scale = input$scale,
                               display_numbers = c(input$disp_num=='T'),
                               angle_col = as.numeric(input$angle_col),
                               color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
              else{
                p <-  pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                               cluster_cols  = c(input$col_cluster=='T'),
                               cluster_rows  = c(input$row_cluster=='T'),
                               show_colnames = c( input$colname=='T'),
                               show_rownames = c(input$rowname=='T'),
                               scale = input$scale,
                               display_numbers = c(input$disp_num=='T'),
                               angle_col = as.numeric(input$angle_col),
                               color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
             
            }

            
            
            
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
              filename="heatmap.rdata",
              content = function(file){
                heatmap <- plot()
                save(heatmap,file = file)
              } )
          }

        })
        
      }) # observeEvent(input$show, 
      
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('heatmap.csv')
        },
        content = function(file) {
          data <- read.csv(('./www/heatmap/heatmap.csv'))
          write.csv(data, file, row.names = F, fileEncoding = "GB18030")
        }
      )
      
      
      
      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id) 


