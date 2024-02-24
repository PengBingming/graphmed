
# library('clusterProfiler') # 基因 ID 转换、富集
library('enrichplot')
library('org.Hs.eg.db')    # 人数据库 hsa
library('org.Mm.eg.db')    # 鼠数据库 mmu
library('org.Rn.eg.db')    # 鼠数据库 rat

keggServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot_kegg0 <- renderPlot({
        return(NULL)
      })

    observeEvent(input$show, {

      # 输入数据
      DEG <- reactive({
        file1 <- input$file1
        if(is.null(file1) ){
          ifelse(input$type=="DEG",
                 DEG <- readRDS('www/DEG.RDS')[1:1000,],
                 DEG <- data.frame("ID"=readRDS('www/DEG.RDS')[1:200,1])
                 )
        }
        else{
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          if(d=='csv'){
            DEG <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
          } else{
            DEG <- data.frame( read_excel(file1$datapath,1) )
          }
        }
        # lis <- strsplit(DEG$ID,'[.]')
        # for (i in 1:length(lis) ) {
        #   DEG$ID[i] <- lis[[i]][1]
        # } ; rm(i)
        return( DEG )
      })

      if(!is.null(DEG() )){
        
      observe({
          DEG <- DEG()
          updateSelectInput(session ,inputId = 'ID',
                            choices = colnames(DEG),selected = colnames(DEG)[1])
          if(!input$ID==''){
            output$DEG <-  DT::renderDataTable({ return(DEG()) })
          }
          
          if(input$type=='DEG'){
          updateSelectInput(session ,inputId = 'pvalue_col', # label = 'pvalue 列',
                            choices = colnames(DEG),selected = '' )
          updateSelectInput(session ,inputId = 'padj_col',  # label = 'padj 列',
                            choices = colnames(DEG),selected = '' )
          updateSelectInput(session ,inputId = 'logFC_col', # label = 'logFC 列',
                            choices = colnames(DEG),selected = '' )

          }

            # KEGG 富集
            observeEvent(input$submit1, {

              # 加载界面
              output$plot_kegg0 <- renderPlot({ 
              OrgDb.db <- reactive({
                if(input$species == 'hsa'){ OrgDb.db <- org.Hs.eg.db }
                else if(input$species == 'mmu'){ OrgDb.db <- org.Mm.eg.db }
                else if(input$species == 'rat'){ OrgDb.db <- org.Rn.eg.db }
                return(OrgDb.db)
              })
            
              observe({
              DEG <- DEG()
              DEG <- dplyr::rename(DEG, c('ID'=input$ID))
              
              # 自动判断输入基因名类型
              name <- reactive({
                if(input$geneid=='unknown'){
                  dat1 <- DEG
                  if(nrow(dat1)>200){ dat2 <- dat1[1:200,] }
                  else{ dat2 <- dat1 }
                  dfName <- data.frame()
                  
                  for (i in geneName){
                    n <- try( clusterProfiler::bitr(unique(dat2$ID), fromType <- i, toType <- "MAP", OrgDb <- OrgDb.db() ),silent=T)
                    logi <- 'try-error' %in% class( n )
                    
                    if(logi== F ){
                      if(nrow(dfName) < nrow(n)){
                        dfName <-  n
                        name <- colnames(dfName)[1]
                      }
                    }
                  }
                }
                else{
                  name <- input$geneid
                }
                return(name)
              })
              
              # 选择 "ENTREZID" 基因
              gene_list <- reactive({
                
                if(is.null(DEG() )){return(NULL)}
                dat1 <- DEG
                name <- name()
                
                if(name=="ENTREZID"){
                  dat1$ENTREZID <- dat1$ID
                  DEG <- dat1
                }
                else if(!name=="ENTREZID"){
                  dfName <- clusterProfiler::bitr(unique(dat1$ID), fromType <- name, toType <- "ENTREZID" , OrgDb <- OrgDb.db())
                   DEG <- merge(dat1, dfName, by.x='ID', by.y= name())
                }
                
                if(input$type=='DEG'){
                  if(!input$logFC_col==""&!input$pvalue_col==""&!input$padj_col==''){        
                  expr <- paste0("ifelse(DEG$",input$pvalue_col, "<" ,input$pvalue,"&","abs(DEG$",input$logFC_col,")>",input$logFC_cutoff,
                                 ",ifelse( DEG$",input$logFC_col,">",input$logFC_cutoff,",'UP','DOWN'),'NOT')")
                  
                  DEG$change <- as.factor( eval(parse(text = expr ) ) )

                  # 3.1 选出上调基因的'ENTREZID' ID
                  gene_up <- DEG[DEG$change == 'UP','ENTREZID']
                  
                  # 3.2 下调基因ID
                  gene_down <- DEG[DEG$change == 'DOWN','ENTREZID']
                  
                  # 3.3 差异基因ID
                  gene_diff <- c(gene_up,gene_down)
                  
                  # 3.4 所有基因ID
                  gene_all <- as.character(DEG[ ,'ENTREZID'] )
                  
                  # 3.4 geneList: LogFC # DEG$log2FoldChange 
                  geneList <- eval(parse(text = paste0("DEG$",input$logFC_col) ) ) # 把 DEG 数据logFC列值赋值给数据geneList
                  
                  names(geneList) <- DEG$ENTREZID # 把ID赋值给geneList数据的名字
                  geneList <- sort(geneList, decreasing = T) # 把数据进行排序
                  
                  # 3.5 选取 logFC ＞ logFC_cutoff 的基因
                  gene <- names(geneList)[abs(geneList) > input$logFC_cutoff ]
                  
                  # 保存数据待用
                  gene_list <- list()
                  
                  gene_list$gene_up <- gene_up
                  gene_list$gene_down <- gene_down
                  gene_list$gene_diff <- gene_diff
                  
                  gene_list$gene <- gene
                  gene_list$gene_all <- gene_all
                  
                  gene_list$geneList <- geneList
                  }
                }
                else{ gene_list <- DEG[,'ENTREZID'] }
                return(gene_list)
              })
   
              # KEGG 富集、网页展示
              kk <- reactive({
                
                if(is.null(gene_list() )){return(NULL)}
                
                gene_list <- gene_list()
                
                if(input$type=='DEG'){
                  # 基因选择
                  
                  if(input$kegg_gene=='diff'){
                    gene_kegg <- gene_list$gene_diff
                  }
                  else if(input$kegg_gene=='up'){
                    gene_kegg <- gene_list$gene_up
                  }
                  else if(input$kegg_gene=='down'){
                    gene_kegg <- gene_list$gene_down
                  }
                  
                  geneList <- gene_list$geneList
                  
                  # 2.1 差异基因富集
                  kk <- clusterProfiler::enrichKEGG(gene          = gene_kegg ,
                                   organism      = input$species,
                                   minGSSize     = input$kegg_min,
                                   maxGSSize     = input$kegg_max,
                                   pAdjustMethod = 'BH',
                                   universe      = names(geneList),
                                   pvalueCutoff  = input$pvalue_kegg,
                                   qvalueCutoff  = input$padj_kegg )
                  
                  kk <- clusterProfiler::setReadable(kk, OrgDb = OrgDb.db(), keyType = "ENTREZID")
                }
                else{
                  # 基因选择
                  gene_kegg <- gene_list
                  
                  # 2.1 差异基因富集
                  kk <- clusterProfiler::enrichKEGG(gene          = gene_kegg ,
                                   organism      = input$species,
                                   minGSSize     = input$kegg_min,
                                   maxGSSize     = input$kegg_max,
                                   pAdjustMethod = 'BH',
                                   pvalueCutoff  = input$pvalue_kegg,
                                   qvalueCutoff  = input$padj_kegg )
                  
                  kk <- clusterProfiler::setReadable(kk, OrgDb = OrgDb.db(), keyType = "ENTREZID")
                }
                return(kk)
              } )
              
              observe({
                if(is.null(kk() ) ) {return(NULL) }
                kk <-kk()
                
                if(input$plot_kegg_id1 == "p_emap"){
                  kk_emap <- enrichplot::pairwise_termsim(kk)
                  
                  updateSelectInput(session, "kegg_num1",label = '通路选择',
                                    choices = kk_emap@result[["Description"]], selected = kk_emap@result[["Description"]][1:10] )
                }
                else if(!input$plot_kegg_id1 == "p_emap"){
                  updateSelectInput(session, "kegg_num1",label = '通路选择',
                                    choices = kk@result[["Description"]], selected = kk@result[["Description"]][1:10] )
                }
                updateSelectInput(session, "kegg_kk",label =   '通路选择',
                                  choices = kk@result[["Description"]], selected = kk@result[["Description"]][1] )
              } )
              
              observeEvent(input$submit1, {
                
                kegg_plot0 <- reactive({
                  if(is.null( kk() )){return(NULL)}
                  kk <- kk()
                  
                  if(input$plot_kegg_id1 == "p_dot") {
                    
                    p <- enrichplot::dotplot(kk, showCategory = input$kegg_num0 )
                  }
                  else if(input$plot_kegg_id1 == "p_bar"){
                    p <- barplot(kk, showCategory = input$kegg_num0 )
                  }
                  else {
                    p <-  emapplot(enrichplot::pairwise_termsim(kk),
                                   showCategory = input$kegg_num0,
                                   layout.params = list(layout = 'kk'),
                                   edge.params = list(min = 0.8),
                                   cex.params = list(category_node = 1.5),
                                   max.overlaps =10 )
                  }
                  return(p)
                })
                
                output$plot_kegg0 <- renderPlot({
                  if (is.null( kk() ) ){ return() }
                  return( kegg_plot0() )
                })
                if(T){
                  output$pdf0 <- downloadHandler(
                    filename="kegg0.pdf",
                    content = function(file){
                      pdf(file,width=input$w0,height=input$h0)
                      print( kegg_plot0() )
                      dev.off()
                    }
                  )
                  output$png0 <- downloadHandler(
                    filename="kegg0.png",
                    content = function(file){
                      png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print( kegg_plot0() )
                      dev.off()
                    }
                  )
                  output$jpeg0 <- downloadHandler(
                    filename="kegg0.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print(kegg_plot0() )
                      dev.off()
                    }
                  )
                  output$tiff0 <- downloadHandler(
                    filename="kegg0.tiff",
                    content = function(file){
                      tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print(kegg_plot0() )
                      dev.off()
                    }
                  )
                }
              })
              
              observeEvent(input$submit_kegg1, {
                
                kegg_plot1 <- reactive({
                  if(is.null( kk() )){return(NULL)}
                  kk <- kk()
                  
                  if(input$plot_kegg_id1 == "p_dot") {
                    
                    p <- enrichplot::dotplot(kk, showCategory = input$kegg_num1  )
                  }
                  else if(input$plot_kegg_id1 == "p_bar"){
                    p <- barplot(kk, showCategory = input$kegg_num1  )
                  }
                  else {
                    p <- emapplot(enrichplot::pairwise_termsim(kk),
                                  showCategory = input$kegg_num1,
                                  layout.params = list(layout = 'kk'),
                                  edge.params = list(min = 0.8),
                                  cex.params = list(category_node = 1.5),
                                  max.overlaps =10 )
                  }
                  
                  return(p)
                })
                
                
                # 3.1 展示KEGG 1
                output$plot_kegg1 <- renderPlot({
                  
                  if (is.null( kk() ) ){ return() }
                  return( kegg_plot1() )
                })
                
                if(T){
                  output$pdf1 <- downloadHandler(
                    filename="kegg1.pdf",
                    content = function(file){
                      pdf(file,width=input$w1,height=input$h1)
                      print( kegg_plot1() )
                      dev.off()
                    }
                  )
                  output$png1 <- downloadHandler(
                    filename="kegg1.png",
                    content = function(file){
                      png(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print( kegg_plot1() )
                      dev.off()
                    }
                  )
                  output$jpeg1 <- downloadHandler(
                    filename="kegg1.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print(kegg_plot1() )
                      dev.off()
                    }
                  )
                  output$tiff1 <- downloadHandler(
                    filename="kegg1.tiff",
                    content = function(file){
                      tiff(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print(kegg_plot1() )
                      dev.off()
                    }
                  )
                }
                
                
              })
              
              observeEvent(input$submit_kegg2, {
                
                kegg_plot2 <- reactive({
                  
                  if(is.null( kk() )){return(NULL)}
                  kk <- kk()
                  
                  if(input$type=='DEG'){
                    if(input$plot_kegg_id2 == "p1"){
                      y1 <- input$kegg_kk
                      gene_list <- gene_list()
                      p = cnetplot(kk, color.params = list(foldChange = gene_list$geneList, edge = T ),
                                   showCategory = y1, layout = "gem", node_label="gene")
                    }
                    else if(input$plot_kegg_id2 == "p2"){
                      y1 <- input$kegg_kk
                      gene_list <- gene_list()
                      p = cnetplot(kk, circular = T, showCategory = y1, layout = "gem", node_label="gene",
                                   color.params = list(foldChange = gene_list$geneList, edge = T )
                      )
                    }
                    
                  }
                  else{
                    if(input$plot_kegg_id2 == "p1"){
                      y1 <- input$kegg_kk
                      gene_list <- gene_list()
                      p = cnetplot(kk, color.params = list(edge = T ),
                                   showCategory = y1, layout = "gem", node_label="gene")
                    }
                    else if(input$plot_kegg_id2 == "p2"){
                      y1 <- input$kegg_kk
                      gene_list <- gene_list()
                      p = cnetplot(kk, circular = T, showCategory = y1, layout = "gem", node_label="gene",
                                   color.params = list( edge = T )
                      )
                    }
                  }
                  return(p)
                  
                  
                })
                
                
                # 3.1 展示KEGG 1
                output$plot_kegg2 <- renderPlot({
                  if (is.null( kk() ) ){ return() }
                  
                  return( kegg_plot2() )
                })
                
                if(T){
                  output$pdf2 <- downloadHandler(
                    filename="kegg2.pdf",
                    content = function(file){
                      pdf(file,width=input$w2,height=input$h2)
                      print( kegg_plot2() )
                      dev.off()
                    }
                  )
                  output$png2 <- downloadHandler(
                    filename="kegg2.png",
                    content = function(file){
                      png(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print( kegg_plot2() )
                      dev.off()
                    }
                  )
                  output$jpeg2 <- downloadHandler(
                    filename="kegg2.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print(kegg_plot2() )
                      dev.off()
                    }
                  )
                  output$tiff2 <- downloadHandler(
                    filename="kegg2.tiff",
                    content = function(file){
                      tiff(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print(kegg_plot2() )
                      dev.off()
                    }
                  )
                }
                
                
              })
              
            })
              })
            }) #  observeEvent(input$submit1
      })
      }
    } ) # show

      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('DEG.csv')
        },
        content = function(file) {
          ifelse(input$type=='DEG',
                 DEG <- readRDS('www/DEG.RDS')[1:1000,],
                 DEG <- data.frame('ID'=readRDS('www/DEG.RDS')[1:200,1])
          )
          write.csv(DEG, file, row.names = F, fileEncoding = "GB18030")
        }
      )
    } # function(input, output, session)
  ) # moduleServer
} # function(id)
