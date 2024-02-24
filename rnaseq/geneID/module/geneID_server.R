
library('readxl')
library('clusterProfiler')
library('org.Hs.eg.db') # 人类数据库 hsa
library('org.Mm.eg.db') # 小鼠数据库 mmu
library('org.Rn.eg.db') # 大鼠数据库 rat

geneIDServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$show, {
      
        dat <- reactive({
        file1 <- input$file1
        if(is.null(file1 )){
          # dat <- data.frame( read.csv('./www/DEG.csv') )[1:2000,]
          dat <- readRDS('./www/DEG.RDS')[1:2000,]
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
        
        output$exp_orgin <- renderDataTable({
          return(dat())
        })

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

          dat <- dat()
          
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
