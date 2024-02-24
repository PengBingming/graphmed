
library(limma)

lang <- Translator$new(translation_csvs_path = "./lang/info/")
lang$set_translation_language("cn") # here you select the default translation to display

reps_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、下载参考数据或点击运行可查看参考数据。")),
                 tags$h6(lang$t("2、ID 列为基因名 ，不要改变列名“ID”。")),
                 tags$h6(lang$t("3、去重复的方法有取平均、最大值行和最小值行，可自行选择。"))
                 
                 
    )
    ) )
  
}

repsUI <- function(id) {
ns <- NS(id)
shiny.i18n::usei18n(lang)
tagList(
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Data',
             fluidRow(
               box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("exp") ) ) 
               ),
               box(width = 3,status="success",
                   fileInput(inputId = ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                   h6(lang$t('格式：.csv .xlsx .xls')),
                   actionBttn( inputId = ns("show"), label = "Show Data",
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   downloadButton(ns("downloadSampleData"), lang$t("参考数据"))
               ) )
             ),
    tabPanel(title = 'Result',
             fluidRow(
               box(title=lang$t("去重复"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), dataTableOutput(ns("result") ) ) 
               ),
               box(width = 3,status="success",
                   actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   selectInput(ns('method'),label = lang$t("方法"),selected = "aver",
                               choices = c('max'="max","average"="aver","min"="min")),
                   downloadButton(ns("downloadResultData"), lang$t('结果') )
               )
               
             ) # fluidRow
    ),
    tabPanel(title = lang$t("使用说明"), reps_helpUI("reps"), icon = ionicon(name="information-circle") )
  )
  ) # tagList 
} # function(id)


repsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
     observeEvent(input$show, {
        
      dat <- reactive({
        file1 <- input$file1
        
        if(is.null(file1 )){
          
          dat=data.frame(matrix(1:70,nrow=10)) # 生成一个表达矩阵
          
          colnames(dat)=c('ID',paste0("trt",1:3),paste0("untrt",1:3)) # 设置列名，样本名字
          dat[,"ID"] <- paste0("gene",c(1,1,2,2,2,3,3,3,4,5))
        }
        else{
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          if(d=='csv'){
            dat <- data.frame( read.csv(file1$datapath,1) )
          } 
          else{
            dat <- data.frame( read_excel(file1$datapath,1) )
          }
        }
        return(dat)
      })
      
      output$exp <- renderRHandsontable(
        rhandsontable(dat(),rowHeaderWidth = 50, height = 300) %>% 
          hot_cols(columnSorting = TRUE) %>% 
          hot_col("ID",type = 'dropdown') %>% 
          hot_col(setdiff(colnames(dat() ),"ID"),type = 'numeric') 
        )
      
      observeEvent(input$submit1, {

          result <-  reactive({
            if(is.null( input$exp )){return(NULL)}
            
            dat <- hot_to_r( input$exp )
            dat <- as.data.frame( dat )
            
            if(input$method=="aver" ){
              result <- avereps(dat[ , setdiff(colnames(dat),'ID')], ID = dat$ID )
              result <- as.data.frame(result)
              result <- cbind("ID"=rownames(result),result)
              rownames(result) <- 1:nrow(result)
            }
            else if(input$method=="max"){
              
              # 计算行平均值，按降序排列
              index <- order(rowMeans(dat[,setdiff(colnames(dat),'ID')]),decreasing = T)
              dat_ordered <- dat[index,] # 排序
              keep <- !duplicated( dat_ordered$ID ) # 每个基因的第一行
              result <- dat_ordered[keep,] # 得到最后处理之后的表达谱矩阵
            }
            else if(input$method=="min"){
              
              #计算行平均值，按升序排列
              index <- order(rowMeans(dat[,setdiff(colnames(dat),'ID')]),decreasing = F)
              dat_ordered <- dat[index,]
              keep <- !duplicated( dat_ordered$ID )
              result <- dat_ordered[keep,]
            }

            
            return( result )
          })

          output$result <- renderDataTable({
            return( result() )
          })
          
          # 下载结果
          output$downloadResultData <- downloadHandler(
            filename = function() {
              paste('去重复结果.csv')
            },
            content = function(file) {
              write.csv( result() , file,row.names = F, fileEncoding = "GB18030") 
            } )

          }) # observeEvent(input$exp, {

        }) # submit1
      
      # 3.2.1 下载参考数据  DEseq2
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('参考数据.csv')
        },
        content = function(file) {
          dat=data.frame(matrix(1:70,nrow=10)) # 生成一个表达矩阵
          
          colnames(dat)=c('ID',paste0("trt",1:3),paste0("untrt",1:3)) # 设置列名，样本名字
          dat[,"ID"] <- paste0("gene",c(1,1,2,2,2,3,3,3,4,5))
          
          write.csv(dat, file,row.names = F, fileEncoding = "GB18030") 
        }
      )
      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id)





