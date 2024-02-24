
library('readxl')
# library('clusterProfiler') # 基因 ID 转换、富集
library("enrichplot")
library('org.Hs.eg.db')    # 人数据库 hsa
library('org.Mm.eg.db')    # 鼠数据库 mmu
library('org.Rn.eg.db')    # 鼠数据库 rat

# library(R.utils)
R.utils::setOption("clusterProfiler.download.method",'auto')

gseagoServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$plot_gsea0 <- renderPlot({
        return(NULL)
      })
      
      observeEvent(input$show,{
 
        DEG <- reactive({
          file1 <- input$file1
          if(is.null(file1) ){
            DEG <- readRDS('www/DEG.RDS')
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
            output$DEG <- DT::renderDataTable({ return(DEG()) })
          }
          
          updateSelectInput(session ,inputId = 'logFC_col',
                            choices = colnames(DEG),selected = '')
          
          # gseGO 富集
          observeEvent(input$submit1,{ 
            
            # 加载界面
            output$plot_gsea0 <- renderPlot({
              
            if(!input$logFC_col==''){
              
            OrgDb.db <- reactive({
              if(input$species == 'hsa'){ OrgDb.db <- org.Hs.eg.db }
              else if(input$species == 'mmu'){ OrgDb.db <- org.Mm.eg.db }
              else if(input$species == 'rat'){ OrgDb.db <- org.Rn.eg.db }
              return(OrgDb.db)
            })
            
            observeEvent(input$DEG, { 

              DEG <- as.data.frame( hot_to_r( input$DEG ) )
              DEG <- dplyr::rename(DEG, c('ID'=input$ID) )
              
              # 自动判断输入基因名类型
              name <- reactive({
                if(input$geneid=='unknown'){
                  dat1 <- DEG
                  dat2 <- dat1[1:500,]
                  
                  dfName <- data.frame()
                  
                  for (i in geneName){
                    n <- try(clusterProfiler::bitr(unique(dat2$ID), fromType <- i, toType <- "MAP", OrgDb <- OrgDb.db() ),silent=T)
                    logi <- 'try-error' %in% class( n ) 
                    
                    if(logi== F ){
                      if(nrow(dfName) < nrow(n)){
                        dfName <-  n
                        name <- colnames(dfName)[1]
                      }
                    }
                  } 
                }
                else{ name <- input$geneid }
                
                return(name)
              })
              
              # 选择 "ENTREZID" 基因
              geneList <- reactive({
                
                if(is.null(DEG )){return(NULL)}
                dat1 <- DEG
                name <- name()
                
                if(name=="ENTREZID"){
                  dat1$ENTREZID <- dat1$ID
                  DEG <- dat1 
                }
                else if(!name=="ENTREZID"){
                  dfName <- clusterProfiler::bitr(unique(dat1$ID), fromType <- name, toType <- "ENTREZID" , OrgDb <- OrgDb.db())
                  DEG <- merge(dat1, dfName, by.y= name(), by.x='ID')
                }
                
                
                # 3.4 geneList: LogFC
                geneList <- eval(parse(text = paste0("DEG$",input$logFC_col) ) ) # 把 DEG 数据logFC列值赋值给数据geneList
                names(geneList) <- DEG$ENTREZID # 把ID赋值给geneList数据的名字
                geneList <- sort(geneList, decreasing = T) # 把数据进行排序
                
                return(geneList)
                
              })
              
              kk_gse <- reactive({
                
                if(is.null(geneList()  )){return(NULL)}
                geneList <- geneList()
                
                # 2.1 差异基因富集
                kk_gse <- clusterProfiler::gseGO(geneList      = geneList,
                                OrgDb         = OrgDb.db() ,
                                ont           = "ALL", # 'CC' ...
                                pAdjustMethod = "BH",
                                minGSSize     = input$gsea_min,
                                maxGSSize     = input$gsea_max,
                                pvalueCutoff  = input$pvalue_gsea,
                                verbose       = TRUE )
                kk_gse <- clusterProfiler::setReadable(kk_gse, OrgDb = OrgDb.db(), keyType = "ENTREZID")
              })
              
              observe({
                
                if(is.null(kk_gse() ) ) {return(NULL) }  
                kk_gse <- kk_gse()
                
                if(input$plot_gsea_id1 == "p_eamp"){
                  
                  kk_gse_emap <-  enrichplot::pairwise_termsim(kk_gse)
                  updateSelectInput(session, "gsea_num1", # label = '通路选择',
                                    choices = kk_gse_emap@result[["Description"]], selected = kk_gse_emap@result[["Description"]][1:10] )
                  
                }
                else if(!input$plot_gsea_id1 == "p_eamp" ){
                  
                  updateSelectInput(session, "gsea_num1", # label = '通路选择',
                                    choices = kk_gse@result[["Description"]], selected = kk_gse@result[["Description"]][1:10] )
                }
                
                updateSelectInput(session, "gsea_kk_gse", # label = '通路选择',
                                  choices = kk_gse@result[["Description"]], selected = kk_gse@result[["Description"]][1] )
                
              } ) 
              

              # GSEA 富集、网页展示
              observeEvent(input$submit1,{
                
                plot_gsea0 <- reactive({
                  if(is.null(kk_gse()  )){return(NULL)}
                  kk_gse <- kk_gse()
                  
                  if(input$plot_gsea_id1 == "p_dot"){
                    p <- dotplot( kk_gse, showCategory= input$gsea_num0 ,split=".sign") + 
                      facet_grid(.~.sign)
                  }
                  else {
                    p<- emapplot(enrichplot::pairwise_termsim(kk_gse), showCategory = input$gsea_num0, 
                                 layout.params = list(layout = 'kk'), 
                                 edge.params = list(min = 0.1), color = "NES", 
                                 cex.params = list(category_node = 1.5),max.overlaps =10 ) 
                  }
                  return(p)
                })
                
          
                # 3.1 展示 GSEA 1
                output$plot_gsea0 <- renderPlot({
                  if (is.null( plot_gsea0() ) ){ return() }
                  return( plot_gsea0() )
                })

                if(T){
                  output$pdf0 <- downloadHandler(
                    filename="gsea0.pdf",
                    content = function(file){
                      pdf(file,width=input$w0,height=input$h0)
                      print( plot_gsea0() )
                      dev.off()
                    }
                  )
                  output$png0 <- downloadHandler(
                    filename="gsea0.png",
                    content = function(file){
                      png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print( plot_gsea0() )
                      dev.off()
                    }
                  )
                  output$jpeg0 <- downloadHandler(
                    filename="gsea0.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print(plot_gsea0() )
                      dev.off()
                    }
                  )
                  output$tiff0 <- downloadHandler( 
                    filename="gsea0.tiff",
                    content = function(file){
                      tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                      print(plot_gsea0() )
                      dev.off()
                    }
                  )
                }
              })
              
              # GSEA 富集、网页展示
              observeEvent(input$submit_gsea1,{  
                
                plot_gsea1 <- reactive({
                  if(is.null(kk_gse()  )){return(NULL)}
                  kk_gse <- kk_gse()
                  
                  if(input$plot_gsea_id1 == "p_dot"){
                    p <- dotplot( kk_gse, showCategory= input$gsea_num1 , split=".sign") + 
                      facet_grid(.~.sign)
                  }
                  else {
                    p<- emapplot(enrichplot::pairwise_termsim(kk_gse), showCategory = input$gsea_num1, 
                                 layout.params = list(layout = 'kk'), 
                                 edge.params = list(min = 0.1), color = "NES", 
                                 cex.params = list(category_node = 1.5),max.overlaps =10 )
                  }
                  return(p)
                })
                
                # 3.1 展示GSEA 1
                output$plot_gsea1 <- renderPlot({
                  if (is.null( kk_gse() ) ){ return() }
                  return( plot_gsea1() )
                })
     
                if(T){
                  output$pdf1 <- downloadHandler(
                    filename="gsea1.pdf",
                    content = function(file){
                      pdf(file,width = input$w1,height=input$h1)
                      print( plot_gsea1() )
                      dev.off()
                    }
                  )
                  output$png1 <- downloadHandler(
                    filename="gsea1.png",
                    content = function(file){
                      png(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print( plot_gsea1() )
                      dev.off()
                    }
                  )
                  output$jpeg1 <- downloadHandler(
                    filename="gsea1.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print(plot_gsea1() )
                      dev.off()
                    }
                  )
                  output$tiff1 <- downloadHandler( 
                    filename="gsea1.tiff",
                    content = function(file){
                      tiff(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                      print(plot_gsea1() )
                      dev.off()
                    }
                    
                  ) }
              })
              
              # 3.1 展示GSEA 2
              observeEvent(input$submit_gsea2, {
                
                plot_gsea2 <-reactive({
                  if (is.null( kk_gse() ) ){ return() }
                  kk_gse <- kk_gse()
                  
                  if(input$plot_gsea_id2 == "p_gsea"){
                    y3 <- match(input$gsea_kk_gse , kk_gse@result[["Description"]] )
                    p <- gseaplot2(kk_gse, geneSetID = y3, pvalue_table = T)
                  }
                  else if(input$plot_gsea_id2 == "p1"){
                    y2 <- input$gsea_kk_gse
                    p = cnetplot(kk_gse, 
                                 color.params = list(foldChange = geneList(), edge = T ),
                                 showCategory = y2, layout = "gem", node_label="gene")
                  }
                  else if(input$plot_gsea_id2 == "p2"){
                    y2 <- input$gsea_kk_gse
                    p <-  cnetplot(kk_gse, circular = T,
                                   color.params = list(foldChange = geneList(), edge = T ), 
                                   showCategory = y2, layout = "gem", node_label="gene")
                  }
                  return(p)
                })
                
                output$plot_gsea2 <- renderPlot({
                  if (is.null( kk_gse() ) ){ return() }
                  return( plot_gsea2() )
                })
                
                if(T){
                  output$pdf2 <- downloadHandler(
                    filename="gsea2.pdf",
                    content = function(file){
                      pdf(file,width=input$w2,height=input$h2)
                      print( plot_gsea2() )
                      dev.off()
                    }
                  )
                  output$png2 <- downloadHandler(
                    filename="gsea2.png",
                    content = function(file){
                      png(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print( plot_gsea2() )
                      dev.off()
                    }
                  )
                  output$jpeg2 <- downloadHandler(
                    filename="gsea2.jpeg",
                    content = function(file){
                      jpeg(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print(plot_gsea2() )
                      dev.off()
                    }
                  )
                  output$tiff2 <- downloadHandler( 
                    filename="gsea2.tiff",
                    content = function(file){
                      tiff(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                      print(plot_gsea2() )
                      dev.off()
                    } )
                } # if
              })
            })
            }
            } ) # output$plot_gsea0    # 加载界面
          } )
        })
        }
      }) # show
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('DEG.csv')
        },
        content = function(file) {
          DEG <-  readRDS('www/DEG.RDS')
          write.csv(DEG, file, row.names = F, fileEncoding = "GB18030")
        } )
      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id)


