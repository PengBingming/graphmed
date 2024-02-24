
options(digits = 10)
library('ggplot2')
library('ggrepel')
library("ggsci")
library('readxl')
library("showtext")
showtext_auto()

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
          output$rds0 <- downloadHandler( 
            filename="volanco.rds",
            content = function(file){
              volanco <- plot()
              saveRDS(volanco,file = file)
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


