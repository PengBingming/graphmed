
if (!require('limma', quietly = TRUE)) BiocManager::install('limma')

if (!require("DESeq2", quietly = TRUE)) BiocManager::install("DESeq2")
if (!require("readxl", quietly = TRUE)) BiocManager::install("readxl")

library(DT)

if (!require("clusterProfiler", quietly = TRUE)) BiocManager::install("clusterProfiler")  # 转换 ID
if (!require("org.Hs.eg.db", quietly = TRUE)) BiocManager::install("org.Hs.eg.db")
if (!require("org.Mm.eg.db", quietly = TRUE)) BiocManager::install("org.Mm.eg.db")
library('org.Rn.eg.db')    # 鼠数据库 rat


library(airway)
geneName <- intersect(columns(org.Mm.eg.db),columns(org.Hs.eg.db) )

lang <- Translator$new(translation_csvs_path = "./lang/info/")

geneID_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、点击“Show Data”可查看参考数据或输入的数据。
                             其中，ID 列必须有，对应基因名，其他列可无。")),
                 tags$h6(lang$t("2、选择转换的基因类型，点击“Analyze Data”得出结果。")),
                 tags$h6(lang$t("3、可以自行设置待转换“基因”的类型（推荐），也可选择 unknown 进行自行判断。")),
                 tags$h6(lang$t("4、设置为 unknown 时基因数不宜太少。"))
    )
    ) )
  
}

geneIDUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("exp_orgin") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data", 
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(inputId = ns("species"), lang$t("物种："),selected = "hsa",
                                 choices = c('human'= "hsa", 'mouse' = "mmu", "rat" ="rat") ),
                     downloadButton(ns("downloadSampleData"), lang$t("参考数据")) 
                 ) )
               ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("结果数据"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),dataTableOutput(ns("exp_change"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("from_geneName"), lang$t("ID列基因"), c(geneName,"unknown") , selected = "unknown"),
                     selectInput(ns("to_geneName")  , lang$t("转换基因")  , geneName , selected = "ENTREZID"),
                     downloadButton(ns("downloadResultData"), lang$t("结果数据"))
                 )
                 
               ) # fluidRow
      ),
      tabPanel(title = 'Help', geneID_helpUI("geneID") )
      
      )
   
  ) # tagList
} # function(id)


geneIDServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
      
      observeEvent(input$show, {
      
        dat <- reactive({
        file1 <- input$file1
        if(is.null(file1 )){
          dat <- data.frame( read.csv('./www/DEG.csv') )[1:2000,]
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
        
        lis <- strsplit(dat$ID,'[.]')
        for (i in 1:length(lis) ) {
          dat$ID[i] <- lis[[i]][1]
        } ; rm(i)
        
        return(dat)
      })
        
        output$exp_orgin <- renderRHandsontable(
          rhandsontable(dat(),rowHeaderWidth = 50, height = 390) %>% 
            hot_cols(columnSorting = TRUE) %>% 
            hot_col("ID",type = 'dropdown') %>% 
            hot_col(setdiff(colnames(dat() ),"ID"),type = 'numeric') 
            )
        
      observeEvent(input$submit1, {

        OrgDb.db <- reactive({
          
          if(input$species == 'hsa'){ OrgDb.db <- org.Hs.eg.db }
          else if(input$species == 'mmu'){ OrgDb.db <- org.Mm.eg.db }
          else if(input$species == 'rat'){ OrgDb.db <- org.Rn.eg.db }
          
          return(OrgDb.db)
        })

        # 自动判断输入基因名类型
        name <- reactive({
          dat1 <- dat()
          dfName <- data.frame()
          
          if( ncol(dat1)>200 ){
            dat1 <- dat1[1:200,"ID"] 
          }
          
          for (i in geneName){
            n <- try(bitr(unique(dat1[,"ID"] ), fromType = i, toType = "MAP", OrgDb = OrgDb.db() ),silent=T)
            logi <- 'try-error' %in% class( n ) 
            
            if(logi== F ){
              if(nrow(dfName) < nrow(n)){
                dfName <-  n
                name <- colnames(dfName)[1]
              }
            }
          }

          return(name)
        })

        id_change <- reactive({
          dat <- hot_to_r( input$exp_orgin )
          dat <- as.data.frame( dat )

           # 1、unknown
           if(input$from_geneName=="unknown"){
             name <- name()
             if(name == input$to_geneName){
               dat$name <- "基因 ID 相同，无需转换"
               colnames(dat)[ncol(dat)] <- input$to_geneName
             }
             else{
               dfName <- bitr(unique(dat$ID), OrgDb = OrgDb.db(),
                              fromType = name,toType = input$to_geneName )
               dat <- merge(dat,dfName,by.y=name, by.x='ID')
             }
           } 
          # 2、input geneName
          else if(!input$from_geneName==input$to_geneName){
            dfName <- bitr(unique(dat$ID), OrgDb = OrgDb.db(), 
                           fromType = input$from_geneName, toType = input$to_geneName )
            dat <- merge(dat,dfName,by.y=input$from_geneName, by.x='ID')
            }
          else{
            dat$m <- "基因 ID 相同，无需转换"
            colnames(dat)[ncol(dat)] <- input$to_geneName
          }
          
          # 保存数据待用
          return(dat)
        } )
        
        output$exp_change <- renderDataTable({
          if (is.null(id_change() ) ) { return() }
          return(id_change())
        })
        
        # 下载结果
        output$downloadResultData <- downloadHandler(
          filename = function() {
            paste('转换结果数据.csv')
          },
          content = function(file) {
            write.csv(id_change() , file,row.names = F, fileEncoding = "GB18030") 
          } )
        
      } )
      
      })

      # 3.2.1 下载参考数据  DEseq2
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('参考数据.csv')
        },
        content = function(file) {
          dat <-  data.frame( read.csv('./www/DEG.csv') )[1:2000,]
          write.csv(dat, file,row.names = F, fileEncoding = "GB18030") 
        }
      )

    } )
}




