
# library('readxl')
library('rhandsontable')
library('ggplot2')
library("ggsci")
library('ggpubr')
# library("showtext") #中文问题
showtext::showtext_auto()

ggplotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data # 读取数据
        df <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            df <- mtcars[,c("cyl","mpg","disp","wt") ]
            colnames(df)[1] <- 'group'
            rownames(df) <- 1:nrow(df)
          } 
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              df <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              df <- data.frame( readxl::read_excel(file1$datapath,1) )
            } 
          } # else
          return( df )
        })
        
        # 输入数据
        if(!is.null(df() ) ){
          output$table <- renderRHandsontable(
            rhandsontable(df(),rowHeaderWidth = 100,  height = 400) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
        
        observe({
          if(!is.null(input$table ) ){
            df <- as.data.frame(hot_to_r( input$table ) )
            
            # ggplot() 参数
            updateSelectInput(session, 'factor', choices = colnames(df) ,
                              selected = colnames(df)[1] )
            updateSelectInput(session,"numeric" , choices = colnames(df) ,
                              selected = colnames(df)[2] )
            updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                              selected = "" )

            updateSelectInput(session, "x", label = 'x', choices = colnames(df) ,
                              selected = colnames(df)[1]  )
            updateSelectInput(session, "y", label = 'y', choices = colnames(df) ,
                              selected = "" )
            updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                              selected = colnames(df)[1]   )
            updateSelectInput(session, "fill", label = 'fill', choices = colnames(df) ,
                              selected = ''  )
            
            # 各种图形参数
            observe({
              
              # 因子型数据
              if(length(input$factor)>=1){
                for ( i in input$factor) {
                  df[,i] <- factor(df[,i], ordered = F)
                }
              }
              # 有序数据
              if(length(input$factor_order)>=1){
                for ( i in input$factor_order) {
                  df[,i] <- factor(df[,i], ordered = T)
                }
              }
              
              # 数值型数据
              if(length(input$numeric)>=1){
                for ( i in input$numeric ) {
                  df[,i] <- as.numeric(df[,i])
                }
              }
              
              if( 'point'     %in% input$plotType){
                updateSelectInput(session, "x_point", label = 'x', choices = colnames(df) , 
                                  selected = input$x  )
                updateSelectInput(session, "y_point"  , label = 'y', choices = colnames(df) , 
                                  selected = colnames(df)[2] )

                updateSelectInput(session, "color_point", label = 'color', choices = colnames(df) , 
                                  selected = ''  )
                updateSelectInput(session, "fill_point", label = 'fill', choices = colnames(df) , 
                                  selected = ''  )
                
              }
              if( 'jitter'    %in% input$plotType){
                updateSelectInput(session, "x_jitter", label = 'x', choices = colnames(df) , 
                                  selected = input$x )
                updateSelectInput(session, "y_jitter"  , label = 'y', choices = colnames(df) , 
                                  selected = colnames(df)[2] )
                updateSelectInput(session, "color_jitter", label = 'color', choices = colnames(df) , 
                                  selected = input$color  )
                updateSelectInput(session, "fill_jitter", label = 'fill', choices = colnames(df) , 
                                  selected = ''  )
                
              }
              if( 'boxplot'   %in% input$plotType){
                updateSelectInput(session, "x_boxplot", label = 'x', choices = colnames(df) , 
                                  selected = input$x  )
                updateSelectInput(session, "y_boxplot"  , label = 'y', choices = colnames(df) , 
                                  selected = colnames(df)[2] )
                updateSelectInput(session, "color_boxplot", label = 'color', choices = colnames(df) , 
                                  selected = ''  )
                updateSelectInput(session, "fill_boxplot", label = 'fill', choices = colnames(df) , 
                                  selected = '' )
                
              }
              if( 'violin'    %in% input$plotType){
                updateSelectInput(session, "x_violin", label = 'x', choices = colnames(df) , 
                                  selected = input$x  )
                updateSelectInput(session, "y_violin"  , label = 'y', choices = colnames(df) , 
                                  selected = colnames(df)[2] )
                updateSelectInput(session, "color_violin", label = 'color', choices = colnames(df) , 
                                  selected = ''  )
                updateSelectInput(session, "fill_violin", label = 'fill', choices = colnames(df) , 
                                  selected = '' )
                
              }
              if( 'histogram' %in% input$plotType ){
                updateSelectInput(session, "x_histogram", label = 'x', choices = colnames(df) , 
                                  selected =  "" )
                updateSelectInput(session, "color_histogram", label = 'color', choices = colnames(df) , 
                                  selected = ""  )
                updateSelectInput(session, 'fill_histogram', label = 'fill', choices = colnames(df) , 
                                  selected = ''  )
              }
              if( 'density'   %in% input$plotType ){
                updateSelectInput(session, "x_density", label = 'x', choices = colnames(df) , 
                                  selected = "" )
                updateSelectInput(session, "color_density", label = 'color', choices = colnames(df) , 
                                  selected = ""  )
                updateSelectInput(session, "fill_density", label = 'fill', choices = colnames(df) , 
                                  selected = "" )
              }
              if( 'dotplot'   %in% input$plotType ){
                updateSelectInput(session, "x_dotplot", label = 'x', choices = colnames(df) , 
                                  selected = "" )
                updateSelectInput(session, "color_dotplot", label = 'color', choices = colnames(df) , 
                                  selected = "" )
                updateSelectInput(session, "fill_dotplot", label = 'fill', choices = colnames(df) , 
                                  selected = "" )
              }
              if( 'text'      %in% input$plotType ){
                updateSelectInput(session, "x_text", label = 'x', choices = colnames(df) , 
                                  selected = input$x )
                updateSelectInput(session, "y_text", label = 'y', choices = colnames(df) , 
                                  selected = input$y  )
                updateSelectInput(session, "color_text", label = 'color', choices = colnames(df)  , 
                                  selected = ''  )
                updateSelectInput(session, "label_text", label = 'label', choices = colnames(df)  , 
                                  selected = input$y  )
              }

              
              observeEvent(input$submit, {
                
                plot <- reactive({

                  expr <- paste0("ggplot(df,aes(x =", input$x,
                                 ", y =", input$y , 
                                 ",color=",input$color,
                                 ",fill=",input$fill,")",
                                 " )")
                  
                  p <- eval( parse(text =expr ) )
                  
                  if( 'violin'    %in% input$plotType){
                    if(!input$x_violin==''|!input$y_violin==''){
                      violin <- paste0("geom_violin(mapping = aes(x=",input$x_violin,
                                       ",y=",input$y_violin,
                                       ",color=",input$color_violin,
                                       ",fill=",input$fill_violin,")",
                                       ",show.legend=",input$show.legend_violin,
                                       ",alpha=",input$alpha_violin,
                                       ",weight=",input$weight_violin,
                                       ",linewidth=",input$linewidth_violin,
                                       ",linetype=",input$linetype_violin,
                                       ")") 
                      p <- p+eval(parse(text = violin ))
                    }
                    
                  }
                  if( 'point'     %in% input$plotType){
                    if(!input$x_point==''|!input$y_point==''){
                    point <- paste0("geom_point(mapping = aes(x=",input$x_point,
                                    ",y=",input$y_point,
                                    ",color=",input$color_point,
                                    ",fill=",input$fill_point,")",
                                    ",show.legend=",input$show.legend_point,
                                    ",alpha=",input$alpha_point,
                                    ",size=",input$size_point,
                                    ",shape=",input$shape_point,
                                    " )") 
                    p <- p+eval(parse(text = point ))
                    }
                  }
                  if( 'jitter'    %in% input$plotType){
                    if(!input$x_jitter==''|!input$y_jitter==''){
                    jitter <- paste0("geom_jitter(mapping = aes(x=",input$x_jitter,
                                     ",y=",input$y_jitter,
                                     ",color=",input$color_jitter,
                                     ",fill=",input$fill_jitter,")",
                                     
                                     ",show.legend=",input$show.legend_jitter,
                                     ",alpha=",input$alpha_jitter,
                                     ",size=",input$size_jitter,
                                     ",shape=",input$shape_jitter,
                                     ",width=",input$width_jitter,
                                     ",height=",input$height_jitter,
                                     ")") 
                    p <- p+eval(parse(text = jitter ))
                    }
                  }
                  if( 'boxplot'   %in% input$plotType){
                    if(!input$x_boxplot==''|!input$y_boxplot==''){
                    boxplot <- paste0("geom_boxplot(mapping = aes(x=",input$x_boxplot,
                                      ",y=",input$y_boxplot,
                                      ",color=",input$color_boxplot,
                                      ",fill=",input$fill_boxplot,")",
                                      
                                      ",show.legend=",input$show.legend_boxplot,
                                      ",notch=",input$notch_boxplot,
                                      ",alpha=",input$alpha_boxplot,
                                      ",size=",input$size_boxplot,
                                      ",shape=",input$shape_boxplot,
                                      ",linewidth=",input$linewidth_boxplot,
                                      ",linetype=",input$linetype_boxplot,
                                      " )" ) 
                    p <- p+eval(parse(text = boxplot ))
                    }
                  }
                  if( 'histogram' %in% input$plotType ){
                    if(!input$x_histogram==''){
                    histogram <- paste0("geom_histogram(mapping = aes(x=",input$x_histogram,
                                        ",color=",input$color_histogram,
                                        ",fill=",input$fill_histogram,")",
                                        ",show.legend=",input$show.legend_histogram,
                                        ",alpha=",input$alpha_histogram,
                                        ",size=",input$size_histogram,
                                        ",bins=",input$bins_histogram,
                                        ",binwidth=",input$binwidth_histogram,
                                        " )") 
                    p <- p+eval(parse(text = histogram ))
                    }
                  }
                  if( 'density'   %in% input$plotType ){
                    if(!input$x_density==''){
                    density <- paste0("geom_density(mapping = aes(x=",input$x_density,
                                      ",color=",input$color_density,
                                      ",fill=",input$fill_density,")",
                                      ",show.legend=",input$show.legend_density,
                                      ",alpha=",input$alpha_density,
                                      ",size=",input$size_density,
                                      ",adjust=",input$adjust_density,
                                      ",lwd=",input$lwd_density,
                                      ",linetype=",input$linetype_density,
                                      ",linewidth=",input$linewidth_density,
                                      ",weight=",input$weight_density,
                                      " )") 
                    p <- p+eval(parse(text = density ))
                    }
                  }
                  if( 'dotplot'   %in% input$plotType ){
                    if(!input$x_dotplot==''){
                      dotplot <- paste0("geom_dotplot(mapping = aes(x=",input$x_dotplot,
                                        ",color=",input$color_dotplot,
                                        ",fill=",input$fill_dotplot,")",
                                        ",show.legend=",input$show.legend_dotplot,
                                        ",method=","'",input$method_dotplot,"'",
                                        ",alpha=",input$alpha_dotplot,
                                        ",dotsize=",input$dotsize_dotplot,
                                        ",binwidth=",input$binwidth_dotplot,
                                        ",linetype=",input$linetype_dotplot,
                                        ",stackdir=","'",input$stackdir_dotplot,"'",
                                        " )") 
                      p <- p+eval(parse(text = dotplot ))
                    }
                  }
                  if( 'text'      %in% input$plotType ){
                    if(c(!input$x_text==''|!input$y_text=='')& !input$label_text==''){
                      text <- paste0("geom_text(mapping = aes(x=",input$x_text,
                                        ",y=",input$y_text,
                                        ",color=",input$color_text,")",
                                        ",label=",'df$',input$label_text,
                                        ",show.legend=",input$show.legend_text,
                                        ",check_overlap=",input$check_overlap_text,
                                        ",parse=",input$parse_text,
                                        ",nudge_x=","'",input$nudge_x_text,"'",
                                        ",nudge_y=","'",input$nudge_y_text,"'",
                                        ",alpha=",input$alpha_text,
                                        ",size=",input$size_text,
                                        ",vjust=",input$vjust_text,
                                        ",angle=",input$angle_text,
                                        " )") 
                      p <- p+eval(parse(text = text ))
                    }
                  }
                  
                  p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
                  
                  if(!input$title==''){
                    p <- p + ggtitle( input$title ) # 标题
                  }
                  if(!input$x.axis.title==''){
                    p <- p + xlab( input$x.axis.title ) # x 轴标签
                  }
                  if(!input$y.axis.title==''){
                    p <- p + ylab( input$y.axis.title ) # y 轴标签
                  }
                  
                  # labs(color=input$color_name) # 图例名
                  # labs(fill=input$fill_name)
                  if(!input$color_name==''){
                    color <- paste0("labs(color='",input$color_name,"')")
                    p <- p + eval(parse(text = color ))
                  }
                  if(!input$fill_name==''){
                    fill <- paste0("labs(fill='",input$fill_name,"')")
                    p <- p + eval(parse(text = fill ))
                  }
                  
                  p <- p +
                    theme(
                      # 标题设置
                      plot.title   = element_text(size  = input$size_title,
                                                  hjust = input$hjust_title,
                                                  color = input$color_title,
                                                  vjust = input$vjust_title,
                                                  angle = input$angle_title
                      ),
                      # 坐标轴标题
                      axis.title   = element_text(size  = input$size_axis.title,
                                                  color = input$color_axis.title,
                                                  hjust = input$hjust_axis.title,
                                                  vjust = input$vjust_axis.title,
                                                  angle = input$angle_axis.title
                      ),
                      # 坐标轴标签
                      axis.text    = element_text(size  = input$size_axis.text,
                                                  color = input$color_axis.text,
                                                  hjust = input$hjust_axis.text,
                                                  vjust = input$vjust_axis.text,
                                                  angle = input$angle_axis.text
                      ),
                      # 坐标轴刻度
                      axis.ticks   = element_line(size      = input$size_axis.ticks,
                                                  linetype  = input$linetype_axis.ticks,
                                                  color     = input$color_axis.ticks
                      ),
                      # 图例标签
                      legend.title = element_text(size  = input$size_legend.title,
                                                  hjust = input$hjust_legend.title,
                                                  color = input$color_legend.title,
                                                  vjust = input$vjust_legend.title,
                                                  angle = input$angle_legend.title
                      ), 
                      # 图例文字
                      legend.text  = element_text(size  = input$size_legend.text,
                                                  hjust = input$hjust_legend.text,
                                                  color = input$color_legend.text,
                                                  vjust = input$vjust_legend.text,
                                                  angle = input$angle_legend.text
                      )
                      
                    )
                  
                  # 配色
                  p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
                  p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))
                  
                  return(p)
                  
                })
                
                output$plot <- renderPlot({return( plot() ) })
                
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
                    filename="plot.RDS",
                    content = function(file){
                      saveRDS( plot() ,file)
                    }  )
                } # 下载图形

              } )
              
            }) # obersve
            
          }
          
        } ) # obersve
  
      })
      
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        } ,
        content = function(file) {
          dat <- mtcars[,c("cyl","mpg","disp","wt") ]
          colnames(dat)[1] <- 'group'
          rownames(df) <- 1:nrow(df)
          write.csv(dat , file, row.names = F, fileEncoding = "GB18030")
        } )
      
    } ) }



