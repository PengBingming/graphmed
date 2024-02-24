
library('readxl')
library('ggplot2')
library('RColorBrewer')
library('pheatmap')
library("limma")
library("showtext")
showtext_auto()

heatmapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        data <- reactive({
          file1 <- input$file1
          if(is.null(file1) ){
            data <- read.csv('./www/heatmap/heatmap.csv')
          }
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if(d =='csv'){
              data <- data.frame( read.csv(file1$datapath,1) )
            }
            else{
              data <- data.frame( read_excel(file1$datapath,1) )
            }
          }
          
          return(data)
        })
        
        output$data <- renderRHandsontable(
          rhandsontable(data(),rowHeaderWidth = 50, height = 400) %>% 
            hot_cols(columnSorting = TRUE)
        )
        
        anno_col <- reactive({
          if(is.null(data() ) ){return(NULL) }
          if(input$col1==''&input$col2==''){return(NULL)}
          
         data <- data()
         data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)

         if(!input$col1==''&!input$col2==''){
           ifelse( ncol(data) %%2 ==0, 
                   anno_col <-  data.frame( group = rep('',ncol(data) ), rep = rep(1:(ncol(data)/2 ), 2 ) ) , 
                   anno_col <-  data.frame( group = rep('',ncol(data) ), rep = c(1:( (ncol(data)+1)/2),1:( (ncol(data)-1)/2)) )
           )
           colnames(anno_col) <- c(input$col1,input$col2)
         }
         else if(!input$col1==''){
           anno_col <-  data.frame( group = rep('',ncol(data) )  )
           colnames(anno_col) <- input$col1
         }
         else{
             ifelse( ncol(data) %%2 ==0, 
                     anno_col <-  data.frame( rep = rep(1:(ncol(data)/2 ), 2 ) ) , 
                     anno_col <-  data.frame( rep = c(1:( (ncol(data)+1)/2),1:( (ncol(data)-1)/2)) )
             ) 
            colnames(anno_col) <- input$col2

           }
         rownames(anno_col ) <- colnames(data)
         
         return(anno_col )
        } )
        

          output$anno_col <- renderRHandsontable(
            rhandsontable(anno_col(),rowHeaderWidth = 60, height = 240) %>% 
              hot_cols(columnSorting = TRUE) 
          ) 
        
        anno_row <- reactive({
          if(is.null(data() ) ){return(NULL) }
          
          if(input$row1==''&input$row2==''){return(NULL) }
          
          data <- data()
          data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)
          
          if(!input$row1==''& !input$row2==''){
            anno_row = data.frame(
              row1 = rep('', nrow(data) ) ,
              row2 = rep(1, nrow(data) ) 
            )
            colnames(anno_row) <- c(input$row1,input$row2)
            
          }
          else if(!input$row1==''){
            anno_row = data.frame(
              cluster = rep('', nrow(data) )
            )
            colnames(anno_row) <- input$row1
          }
          else if(!input$row2==''){
            anno_row = data.frame(
              cluster = rep('', nrow(data) )
            )
            colnames(anno_row) <- input$row2
          }

          rownames(anno_row ) <- rownames(data)
 
          return(anno_row )
        } )

        output$anno_row <- renderRHandsontable(
          rhandsontable(anno_row(),rowHeaderWidth = 60, height = 240) %>% 
            hot_cols(columnSorting = TRUE)
        )


        observeEvent(input$submit, {
          plot <- reactive({
            
            data <- hot_to_r( input$data )
            data <- avereps(data[, setdiff(colnames(data),'ID')],ID = data$ID)
            
            
            
            anno_col <- hot_to_r(input$anno_col )
            anno_row <- hot_to_r(input$anno_row )

            if(input$row=='T'){

              if(input$col=='T'){
                p <- pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                              cluster_cols  = c(input$col_cluster=='T'),
                              cluster_rows  = c(input$row_cluster=='T'),
                              show_colnames = c( input$colname=='T'),
                              show_rownames = c(input$rowname=='T'),
                              annotation_row = anno_row ,
                              annotation_col = anno_col,
                              scale = input$scale,
                              display_numbers = c(input$disp_num=='T'),
                              angle_col = as.numeric(input$angle_col),
                              color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
              else{
                p <-pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                             cluster_cols  = c(input$col_cluster=='T'),
                             cluster_rows  = c(input$row_cluster=='T'),
                             show_colnames = c( input$colname=='T'),
                             show_rownames = c(input$rowname=='T'),
                              annotation_row = anno_row,
                              scale = input$scale,
                              display_numbers = input$disp_num,
                             angle_col = as.numeric(input$angle_col),
                             color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
            }
            else{
              
              if(input$col=='T'){
                p <-  pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                               cluster_cols  = c(input$col_cluster=='T'),
                               cluster_rows  = c(input$row_cluster=='T'),
                               show_colnames = c( input$colname=='T'),
                               show_rownames = c(input$rowname=='T'),
                               annotation_col = anno_col,
                               scale = input$scale,
                               display_numbers = c(input$disp_num=='T'),
                               angle_col = as.numeric(input$angle_col),
                               color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
              else{
                p <-  pheatmap(data, fontsize = 15, fontsize_row=15, fontsize_col = 20, 
                               cluster_cols  = c(input$col_cluster=='T'),
                               cluster_rows  = c(input$row_cluster=='T'),
                               show_colnames = c( input$colname=='T'),
                               show_rownames = c(input$rowname=='T'),
                               scale = input$scale,
                               display_numbers = c(input$disp_num=='T'),
                               angle_col = as.numeric(input$angle_col),
                               color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                )
              }
             
            }

            
            
            
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
              }  )
            output$rds0 <- downloadHandler( 
              filename="heatmap.rds",
              content = function(file){
                heatmap <- plot()
                saveRDS(heatmap,file = file)
              } )
          }

        })
        
      }) # observeEvent(input$show, 
      
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('heatmap.csv')
        },
        content = function(file) {
          data <- read.csv(('./www/heatmap/heatmap.csv'))
          write.csv(data, file, row.names = F, fileEncoding = "GB18030")
        }
      )
      
      
      
      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id) 


