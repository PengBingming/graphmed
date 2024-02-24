
library('readxl')
library('limma')

repsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
     observeEvent(input$show, {
        
      dat <- reactive({
        file1 <- input$file1
        
        if(is.null(file1 )){
          
          dat <- data.frame(matrix(1:70,nrow=10)) # 生成一个表达矩阵
          
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
      
      output$exp <- renderDataTable({
        return(dat())
      })
      
      
      observeEvent(input$submit1, {

          result <-  reactive({
            if(is.null( dat() )){return(NULL)}
            dat <- dat()
            
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
              paste('Result.csv')
            },
            content = function(file) {
              write.csv( result() , file,row.names = F, fileEncoding = "GB18030") 
            } )

          }) # observeEvent(input$exp, {

        }) # submit1
      
      # 3.2.1 下载参考数据  DEseq2
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sampleData.csv')
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

