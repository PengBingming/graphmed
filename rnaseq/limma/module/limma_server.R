
# library('readxl')
library('limma')
library('ggplot2')
library('ggrepel') # 标签用
library("FactoMineR") # 主成分分析图
library("factoextra") # 主成分分析图
library('pheatmap')

limmaServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
   observeEvent(input$show, {

       exp <- reactive({
         file1 <- input$file1
         
         if( is.null(file1)  ){
           df1 <- data.frame(readRDS('www/exp.RDS') )[1:2000,]
         }
         else{
           d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
           if( d=='csv' ){
             df1 <- data.frame( read.csv(file1$datapath,header=T, 
                                         stringsAsFactors = FALSE, 
                                         fileEncoding = 'GB18030') )
           } 
           else{
             df1 <- data.frame( readxl::read_excel(file1$datapath,1) )
           }
         }
         
         # lis <- strsplit(df1$ID,'[.]')
         # for (i in 1:length(lis) ) {
         #   
         #   df1$ID[i] <- lis[[i]][1]
         #   
         # } ; rm(i)
         
         return(df1 )
       })
       if(!is.null(exp() ) ){
         # 输入数据
         output$exp <- renderRHandsontable(
           rhandsontable(exp(),rowHeaderWidth = 50, height = 410) %>% 
             hot_cols(columnSorting = TRUE) %>% 
             hot_col("ID",type = 'dropdown') %>% 
             hot_col(setdiff(colnames(exp() ),'ID'),type = 'numeric') 
         )   }
       
       # if(!is.null(exp() ) ){ }
     observe({
       exp <- exp()
       updateSelectInput(session ,inputId = 'ID', choices = colnames(exp ),
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
             updateSelectInput(session, "untrt", label = 'Control group', choices = unique(condition) , selected = unique(condition)[1] )
             updateSelectInput(session, "trt"  , label = 'Case group', choices = unique(condition) , selected = unique(condition)[2] )
           }
           
         } )
       }
       
       observeEvent(input$submitDEG, { 
         
      if( !all(hot_to_r(input$group)[,'condition']=='') ){ 
         myLimma <- reactive( {   
           
           if(input$trt==input$untrt){return(NULL)}
           
           exp   <- hot_to_r( input$exp )
           exp <- dplyr::rename(exp, c('ID'=input$ID))
           
           group <- hot_to_r( input$group )
           
           group <- group[order(group$condition),]
           exp   <- exp[,c("ID",group$sample)]
           
           exp <- avereps(exp[,setdiff(colnames(exp),'ID')],ID=exp$ID)
           exp <- exp[rowSums(exp) >= input$rowSum_filter , ]
           exp <- as.matrix(exp)
           condition <- group$condition
           
           # 差异分析走标准的limma流程 ---------------------------------------------------------
           # 创建一个分组的矩阵
           design = model.matrix(~0+factor(condition ) )#创建一个分组的矩阵
           colnames(design) = levels(factor(condition ) )
           rownames(design) = colnames(exp)
           # 创建差异比较矩阵 ----------------------------------------------------------------
           
           # 这个矩阵声明，我们要 实验组 和 对照组 进行差异分析比较
           levels <- levels( factor(condition ) )
           
           contrast.matrix <- eval(parse(text = paste0("makeContrasts(",input$trt,'-',input$untrt,',levels = design)')))
           
           # 第一步 lmFit，# lmFit 为每个基因给定一系列的阵列来拟合线性模型
           fit <- lmFit(exp,design)
           
           # 第二步 eBayes，# eBayes 给出了一个微阵列线性模型拟合，通过经验贝叶斯调整标准误差到一个共同的值来计算修正后的t统计量、修正后的f统计量和微分表达式的对数概率。
           fit1 <- contrasts.fit(fit, contrast.matrix)
           fit1 <- eBayes(fit1)
           
           # 第三步 topTable, # topTable 从线性模型拟合中提取出排名靠前的基因表。
           # options(digits = 4) # 设置全局的数字有效位数为 4
           
           # topTable(fit1,coef=2,adjust='BH') 
           DEG <- topTable(fit1, coef=1, n=Inf,adjust.method = input$method ) 
           DEG <- na.omit(DEG ) # 移除 NA 值
           
           DEG <- DEG[order( DEG$P.Value ), ]
           
           # 输出结果
           result <- list()
           result$exp <- exp
           result$group <- group
           result$DEG <- DEG 
           
           return(result )
         } )
         
         output$DEG <- renderDataTable({
           DEG <- myLimma()$DEG
           DEG <- cbind(ID=rownames(DEG),DEG)
           
           return( DEG )
         })
         
         output$downloadDEG <-  downloadHandler(
           filename = function() {
             paste('Limma_DEG.csv')
           },
           content = function(file) {
             DEG <- myLimma()$DEG
             DEG <- cbind(ID=rownames(DEG),DEG)
             write.csv(DEG, file, row.names = F, fileEncoding = "GB18030")
           }  )
         
         observeEvent(input$submitPlot, { 
           
           plot <- reactive({
             
             if(is.null(myLimma() ) ){return(NULL)}
             
             DEG <- myLimma()$DEG
             exp   <- myLimma()$exp
             group <- myLimma()$group 
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
                   DEG$P.Value < input$pvalue & DEG$adj.P.Val < input$padj & abs(DEG$logFC) >= input$logFC,
                   ifelse(
                     DEG$logFC >= input$logFC,'UP','DOWN'),
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
                 DEG$FC <- abs(DEG$logFC) 
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
             else if(input$plot=='volcano'){
               
               DEG <- myLimma()$DEG
               DEG$gene <- rownames(DEG)
               DEG$FC <- abs(DEG$logFC) 
               
               DEG$change <- as.factor(
                 ifelse(
                   DEG$P.Value < input$pvalue & DEG$adj.P.Val < input$padj & abs(DEG$logFC) > input$logFC,
                   ifelse(
                     DEG$logFC > input$logFC,'UP','DOWN'),
                   'NOT'))
               # table(DEG$change ) # 查看基因上、下调情况
               
               # 设置火山图的标题
               this_tile=paste('Cutoff for logFC is ',round(input$logFC,3),
                               '\nThe number of up gene is ',nrow(DEG[DEG$change=='UP',]),
                               '\nThe number of down gene is ',nrow(DEG[DEG$change=='DOWN',]))
               
               if(input$order=='pvalue'){
                 DEG <- DEG[order(DEG$P.Value),]
               }
               else if(input$order=='logFC'){
                 DEG <- DEG[order(DEG$FC,decreasing = T),]
               }
               # 画火山图
               p <- ggplot(data=DEG,aes(x = logFC, y = -log10(P.Value ), color= change)) +
                 geom_point(alpha=0.4,size=1.75)   # 绘制点图
               
               p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
               
               p <- p +  
                 xlab("log2 fold change")+
                 ylab("-log10 pvalue") +    # 轴标签
                 ggtitle(this_tile)+
                 geom_text_repel(
                   data = DEG[ DEG$P.Value < input$pvalue &  DEG$adj.P.Val < input$padj & 
                                 abs(DEG$logFC ) > input$logFC,][1:input$volcano_num,],
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
               }
             )
             output$rds0 <- downloadHandler( 
               filename="plot.rds",
               content = function(file){
                 plot <- plot()
                 save(plot,file = file)
               } )
           }

         })
         
      } # if(logi)
         
       } )

        })

   })
      
      # 3.2.2 下载参考数据 limma
      output$downloadSampleExp <- downloadHandler(
        filename = function() {
          paste('Limma_exp.csv')
        },
        content = function(file) {
          write.csv( head( readRDS('www/exp.RDS'), input$sample_exp_row), 
                     file, row.names = F, fileEncoding = "GB18030")
        } )
      
      # 3.2.2 下载参考数据 limma
      output$downloadSampleGroup <- downloadHandler(
        filename = function() {
          paste('Limma_group.csv')
        },
        content = function(file) {
          write.csv(readRDS('./www/group.csv'), file, row.names = F, fileEncoding = "GB18030")
        } )

    } # function(input, output, session)
  ) # moduleServer
} # function(id)


