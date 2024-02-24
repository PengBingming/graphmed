
library('readxl')
library('DESeq2')
library('limma')
library('pheatmap')
library('ggplot2')
library("ggrepel")
library("FactoMineR") # 画主成分分析图需要加载这两个包
library("factoextra") 
library("showtext") # 中文问题
showtext_auto()

deseq2Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$show, {

        exp <- reactive({
          file1 <- input$file1
          if( is.null(file1)  ){
            # df1 <- data.frame(read.csv('./www/deseq2/exp.csv') )[1:2000,]
            df1 <- readRDS('www/exp.RDS')
          }
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              df1 <- data.frame( read.csv(file1$datapath,header=T, 
                                          stringsAsFactors = FALSE, 
                                          fileEncoding = 'GB18030') )
            } 
            else{
              df1 <- data.frame( read_excel(file1$datapath,1) )
            }
          }
          return(df1 )
        })
       if(!is.null(exp() ) ){ 
        observe({

            exp <- exp()
            updateSelectInput(session ,inputId = 'ID','基因列',choices = colnames(exp ),
                              selected = colnames(exp )[1] )
            
            if(!input$ID=='' ){
              # 输入数据
              output$exp <- renderRHandsontable(
                rhandsontable(exp(),rowHeaderWidth = 50, height = 410) %>% 
                  hot_cols(columnSorting = TRUE) %>% 
                  hot_col(input$ID,type = 'dropdown') %>% 
                  hot_col(setdiff(colnames(exp() ),input$ID),type = 'numeric') 
              )
              group <- reactive({
                df2 <- data.frame(
                  sample=setdiff(colnames(exp ),input$ID),
                  condition=rep('',ncol(exp )-1)
                )
                return(df2 )
              })
              
              # 输入分组
              output$group <- renderRHandsontable(
                rhandsontable(group(),rowHeaderWidth = 22, height = 410) %>% 
                  hot_cols(columnSorting = TRUE) %>% 
                  hot_col( colnames(group() ) ,type = 'dropdown')
              )
              
              # 1.3 选择实验组与对照组
              observe({
                if(!is.null(input$group ) ){
                  group <- as.data.frame(hot_to_r( input$group ) )
                  
                  group <- group[order( group$condition ), ]
                  condition <- group$condition
                  
                  # 实验组与对照组情况
                  updateSelectInput(session, "untrt", label = '对照组', choices = unique(condition) , selected = unique(condition)[1] )
                  updateSelectInput(session, "trt"  , label = '实验组', choices = unique(condition) , selected = unique(condition)[2] )
                }
                
              } )
            }
           
       observeEvent(input$submitDEG, { 
             # 设置整数，去低表达，排序
             exp2 <- reactive({
               if( is.null(input$exp ) ){ return( NULL ) }
               exp <- as.data.frame(hot_to_r( input$exp ) )
               
               exp <- dplyr::rename(exp, c('ID'=input$ID))
               
               # lis <- strsplit(exp$ID,'[.]')
               # for (i in 1:length(lis) ) {
               #   exp$ID[i] <- lis[[i]][1]
               # } ; rm(i)
               
               exp <- avereps(exp[ , setdiff(colnames(exp),'ID') ], ID = exp$ID ) # 去重复，ID赋值到行名
               
               # exp <- avereps(exp[ , setdiff(colnames(exp),'ID')], ID = exp$ID ) # 去重复，ID赋值到行名
               
               # 使用 DEseq 处理的数据必须是整数，设置数据为整数
               forceMatrixToInteger <- function(data){
                 apply (data, c (1, 2), function (x) {
                   (as.integer(x))
                 }) }
               
               exp <- forceMatrixToInteger(data = exp)
               # 2.1 对表达矩阵数据进行筛选
               index1 <- c(rowSums(exp) >= input$rowSum_filter ) # 判断
               exp2 <- exp[index1,] # 筛选
               
               return(exp2 )
             })
             
             if( !all(hot_to_r(input$group)[,'condition']=='') ){ 
               # 1.2 分析结果提取，变量赋值
               # 差异分析
               dds <- reactive({
                 if( is.null(exp2() ) ){ return( NULL ) }
                 
                 exp2 <- exp2()
                 
                 # 1.2 分组信息
                 group <- as.data.frame(hot_to_r( input$group ) )
                 colData <- data.frame(group$condition)
                 rownames(colData) <- as.character(group$sample )  # 设置行名为样本名信息
                 colnames(colData) <- 'condition' # 列名
                 
                 # 二、构建 DEseq对象 -------------------------------------------------------------
                 colData$condition <- factor(colData$condition) # 2.2 构建分组数据框 colData
                 
                 # 三、DEseq2处理 --------------------------------------------------------------
                 # 3.1 构建DEseq2对象
                 dds <- DESeqDataSetFromMatrix(countData = exp2, colData = colData, design = ~ condition)
                 dds <- DESeq(dds) # 3.2 DEseq2  # 差异分析
                 
                 return(dds )
               })   
               
               # observeEvent(input$submitDEG, { 
                 
                 # 2.1 分组后的差异结果，基因ID转换  
                 res <- reactive({
                   if(is.null(dds() )){return(NULL)}
                   
                   if(input$trt == input$untrt){return(NULL)}
                   dds <- dds()
                   res <- results(dds, contrast = c("condition", input$trt, input$untrt ) ) 
                   
                   return( res )
                 })
                 
                 DEG <- reactive({
                   if(is.null(res() ) ){ return() }
                   
                   res <- res()
                   # 5.2 排序、筛选
                   resOrdered <- res[order(res$pvalue), ] # order() 给出从小到大排序后的位置(默认升序)
                   DEG <- as.data.frame(resOrdered)
                   
                   return( DEG )
                 })
                 
                 output$DEG <- renderDataTable({
                   return( DEG() )
                 })
                 
                 # 3.3下载差异结果 DEG_gene()
                 output$downloadDEG <- downloadHandler(
                   
                   filename = function() {
                     paste("DEG.csv")
                   },
                   content = function(file) {
                     DEG <- DEG()
                     DEG <- cbind('ID'=rownames(DEG),DEG)
                     write.csv(DEG, file,row.names = F, fileEncoding = "GB18030")
                   } )
                 
                 observeEvent(input$submitNorm, { 
                   
                   # 标准化数据
                   norm_count <- reactive({ 
                     
                     if(is.null(dds() ) ){ return() }
                     # 标准化
                     norm_count <- counts(dds() , normalized=T)
                     norm_count_mad <- apply(norm_count, 1, mad)
                     
                     if(input$norm_chioce=='raw'){
                       norm_count <- exp2() 
                     }
                     else if(input$norm_chioce=="norm_count" ){
                       norm_count <- norm_count[order(norm_count_mad, decreasing = T),]
                     }
                     else if(input$norm_chioce=="vsdmat"){
                       vsd<- varianceStabilizingTransformation(dds(), blind=FALSE)
                       vsdmat <- assay(vsd ) # 提取转化后的矩阵
                       norm_count<-  vsdmat[order(norm_count_mad,decreasing = T),]
                     }
                     else if(input$norm_chioce=="rlogmat"){
                       rld <- rlog(dds(), blind = F)
                       rlogmat <- assay(rld )
                       norm_count <- rlogmat[order(norm_count_mad,decreasing = T),]
                     }
                     return(norm_count)
                   })
                   
                   output$norm <- renderDataTable({
                     return(as.data.frame(norm_count() ) )
                   })
                   
                   # 5.3 下载参考数据 DEseq2  Excel
                   output$downloadNorm<- downloadHandler(
                     filename = function() {
                       paste('norm.csv')
                     },
                     content = function(file) {
                       norm_count <- as.data.frame(norm_count() )
                       norm_count <- cbind('ID'=rownames(norm_count), norm_count)
                       write.csv(norm_count, file,  row.names = F, fileEncoding = 'GB18030')
                     } )
                   
                   observeEvent(input$submitPlot, { 
                     
                     plot <- reactive({
                       
                       if(is.null(DEG() ) ){return(NULL)}
                       
                       DEG <- DEG()
                       exp   <- norm_count()
                       
                       group <- as.data.frame( hot_to_r(input$group ) )
                       condition <- group$condition
                       
                       if(input$plot=='pca'){
                         df_pca <- t(exp ) # 画PCA图时要求是行名是样本名，列名是探针名，因此此时需要转换 t()
                         df_pca <- as.data.frame(df_pca ) # 将 matrix转换为data.frame
                         df_pca <- cbind(df_pca, condition ) # cbind横向追加，即将分组信息追加到最后一列
                         
                         dat.pca <- PCA(df_pca[,-ncol(df_pca)], graph = FALSE)
                         p <- fviz_pca_ind(dat.pca,
                                           geom.ind = "point", # show points only (nbut not "text")
                                           col.ind = df_pca$condition, # color by groups
                                           
                                           addEllipses = TRUE, # Concentration ellipses
                                           legend.title = "Groups" )
                       }
                       else if(input$plot=='heatmap'){
                         # 3、热图
                         # library(pheatmap)
                         # 选取存在差异的基因
                         DEG$change <- as.factor(
                           ifelse(
                             DEG$pvalue < input$pvalue & DEG$padj < input$padj & abs(DEG$log2FoldChange) >= input$logFC,
                             ifelse(
                               DEG$log2FoldChange >= input$logFC,'UP','DOWN'),
                             'NOT'))
                         
                         DEG <- DEG[which(!DEG$change=='NOT'),]
                         
                         # 设置热图分组
                         t <- table(group$condition)
                         
                         rep <- vector()
                         for (i in 1:length(t) ) {
                           rep <- c( rep, 1:t[i] )
                         }
                         
                         annotation_col = data.frame(
                           group = factor( group$condition ) ,
                           rep = rep )
                         rownames(annotation_col)<-colnames(exp)
                         
                         if(input$order=='pvalue'){
                           # 选择基因
                           choose_gene <- head(rownames(DEG), input$heatmap_num)
                           choose_matrix <- exp[choose_gene, ]
                           
                           p <- pheatmap(choose_matrix,
                                         annotation_col = annotation_col,
                                         scale = 'row',
                                         fontsize = 15, 
                                         fontsize_row=15, 
                                         fontsize_col = 20,
                                         cluster_cols = F,
                                         show_colnames =T,
                                         show_rownames = T)
                         }
                         else if(input$order=='logFC'){
                           # 按 logFC 排序
                           DEG$FC <- abs(DEG$log2FoldChange)
                           DEG <-  DEG[order( DEG$FC,decreasing = T),]
                           
                           choose_gene <- head(rownames(DEG), input$heatmap_num)
                           choose_matrix <- exp[choose_gene, ]
                           
                           p <- pheatmap(choose_matrix,
                                         annotation_col = annotation_col,
                                         scale = 'row',
                                         fontsize = 15, 
                                         fontsize_row=15, 
                                         fontsize_col = 20,
                                         cluster_cols = F,
                                         show_colnames =T,
                                         show_rownames = T)
                         }
                         
                       }
                       if(input$plot=='volcano'){
                         
                         DEG <- DEG()
                         DEG$gene <- rownames(DEG)
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
                         p <- ggplot(data=DEG,
                                     aes(x = log2FoldChange, 
                                         y = -log10( pvalue ),
                                      color= change)) +
                           geom_point(alpha=0.4,size=1)   # 绘制点图
                         
                         p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
                         
                         p <- p+  xlab("log2 fold change")+
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
                           scale_color_manual(values=c(input$color_down,input$color_not,input$color_up))   # 设定颜色
                         
                       }
 
                       return(p)
                     } )
                     
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
                         } )
                       
                       output$png0 <- downloadHandler(
                         filename="plot.png",
                         content = function(file){
                           png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                           print( plot() )
                           dev.off()
                         } )
                       
                       output$jpeg0 <- downloadHandler(
                         filename="plot.jpeg",
                         content = function(file){
                           jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                           print(plot() )
                           dev.off()
                         } )
                       
                       output$tiff0 <- downloadHandler( 
                         filename="plot.tiff",
                         content = function(file){
                           tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                           print(plot() )
                           dev.off()
                         } )
                       
                       output$rds0 <- downloadHandler( 
                         filename="DEseq2.rds",
                         content = function(file){
                           DEseq2 <- plot()
                           save(DEseq2,file = file)
                         } )
                     }
                   })
                 })
               # } ) # observeEvent(input$submitDEG, { 
             } # if(logi)
           })
        })
       }
      } ) # observeEvent(input$show, { 
        

      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('exp.csv')
        },
        content = function(file) {
          write.csv(head( readRDS('www/exp.RDS'), input$sample_exp_row),
                    file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData1 <- downloadHandler(
        filename = function() {
          paste('group.csv')
        },
        content = function(file) {
          write.csv(readRDS('www/group.RDS'), file, row.names = F, fileEncoding = 'GB18030')
        } )

    } # function(input, output, session)
) # moduleServer
} # function(id)
