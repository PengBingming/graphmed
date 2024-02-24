
library('readxl')
library('ggplot2')     # 画图
# library('reshape')
# library('ggpubr')
showtext::showtext_auto()

# 编写
myfun1 <- function(data, input){

  # 去重复
  df1 <- data
  colnames(df1) <- toupper(colnames(df1))

  dfs <- df1[which(df1$CQ.STD.DEV < input$Cq.Std.Dev), -which(colnames(df1)=='CQ.STD.DEV')]
  
  df1 <- reshape::melt( dfs, id.vars = c('GROUP','SAMPLE','TARGET') )
  
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

qpcrServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        # 读取 qpcr数据
        df1 <- reactive({
          
          file1 <- input$file1
          if(is.null(file1)){
            df1 <- readRDS('www/qpcr.RDS')
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
            updateSelectInput(session, "gene", # label = '内参基因', 
                              choices = unique(df1$TARGET) , selected = unique(df1$TARGET)[1] )
            updateSelectInput(session, "group", # label = '对照样本',
                              choices = unique(df1$GROUP)  , selected = unique(df1$GROUP )[1] )
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
                    ggpubr::stat_compare_means( # method = "anova"
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
          paste('qpcr sample.csv')
        },
        content = function(file) {
          write.csv(readRDS('www/qpcr.RDS') , file, row.names = F,fileEncoding = 'GB18030')
        }
      ) 
    } # function(input, output, session) 
  ) # moduleServer
} # function(id)

