
library('ggplot2')
library("ggsci")
library('ggpubr')

color_type <- c("npg","aaas","nejm","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

barplotUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Data',
             fluidRow(
               box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("table") ) ) ),
               box(width = 3,status="success",
                   fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                   h6(lang$t('格式：.csv .xlsx .xls')),
                   actionBttn( inputId = ns("show"), label = "Show Data",
                               style = "fill", color = "primary", size = "sm" ),
                   hr(),
                   column(width = 12,selectInput(ns('factor'),  h6(lang$t("分类变量"),style="color:orange"), c(""), multiple = T )),
                   column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("有序变量"),style="color:orange"), c(""), multiple = T )),
                   column(width = 12,selectInput(ns('numeric'), h6(lang$t("连续变量"),style="color:orange"), c(""), multiple = T )),
                   column(width = 12,selectInput(ns('na_omit'), lang$t("去除缺失值"), c(T,F),selected = T )),
                   hr(),
                   downloadButton(ns("downloadSampleData"),lang$t("参考数据"))
                   
               )
             )
    ),
    tabPanel(title = 'Result',
             fluidRow(
               box(title=lang$t("计算结果"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("resultTable") ) ) ),
               box(width = 3,status="success",
                   actionBttn( inputId = ns("result"), label = "Calculate",
                               style = "fill", color = "primary", size = "sm" ),
                   hr(),
                   column(width = 12,selectInput(ns('group'),h6( "group",style="color:orange"), c(""),multiple = F )),
                   column(width = 12,selectInput(ns('var_select'), h6("var.select",style="color:orange"), c(""), multiple = T )),
                   hr(),
                   downloadButton(ns("downloadResult"),lang$t("计算结果"))
               )
             )
    ),
    tabPanel(title = 'Plot',
             fluidRow(
               box(width=9, title=lang$t("图形"),solidHeader=TRUE,status = "primary",background = "white",
                   fluidRow(
                     column(width =3,
                            dropdownButton(circle=FALSE, label= lang$t("基础"),size = "sm", br(),br(),
                                           fluidRow(
                                             column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                                             column(width = 6, textInput(ns('title'), label = "plot.title", value = "barplot" )),
                                             column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "" )),
                                             column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "" ) ),
                                             column(width = 6, textInput(ns("color_name"),label = "color.name",value = "") ),
                                             column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) ),
                                             column(width = 6, textInput(ns("fill_name"),label = "fill.name",value = "") ),
                                             column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                                           )
                            ) ),
                     column(width =9,
                            fluidRow(
                              column(width =2,
                                     dropdownButton(circle=FALSE, label= lang$t("标题"), size = "sm",br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_title'),  'size', value = 50 )),
                                                      column(width = 12,selectInput(ns('color_title'),  "color",choices = colors(), selected = "black" )),
                                                      column(width = 12,numericInput(ns('hjust_title'), "hjust",value = 0.5 )),
                                                      column(width = 12,numericInput(ns('vjust_title'), 'vjust', value = 0 )),
                                                      column(width = 12,numericInput(ns('angle_title'), 'angle', value = 0 ))
                                                    )
                                     )
                              ),
                              column(width = 2,
                                     dropdownButton(circle=FALSE, label= lang$t("轴标题"), size = "sm",br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_axis.title'), 'size', value = 30 )),
                                                      column(width = 12,selectInput(ns('color_axis.title'), "color",choices = colors(),  selected = "black" )),
                                                      column(width = 12,numericInput(ns('hjust_axis.title'),"hjust",value = 0.5 )),
                                                      column(width = 12,numericInput(ns('vjust_axis.title'), 'vjust', value = 0 )),
                                                      column(width = 12,numericInput(ns('angle_axis.title'), 'angle', value = 0 ))
                                                    )
                                     ) ) ,
                              
                              column(width = 2,
                                     dropdownButton(circle=FALSE, label= lang$t("轴文本"), size = "sm",br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_axis.text'), 'size', value = 30 )),
                                                      column(width = 12,selectInput(ns('color_axis.text'), "color",choices = colors(), selected = "black" )),
                                                      column(width = 12,numericInput(ns('hjust_axis.text'),"hjust",value = 0 )),
                                                      column(width = 12,numericInput(ns('vjust_axis.text'), 'vjust', value = 0 )),
                                                      column(width = 12,numericInput(ns('angle_axis.text'), 'angle', value = 0 ))
                                                    )
                                     ) ),
                              column(width = 2,
                                     dropdownButton(circle=FALSE, label= lang$t("轴刻度"), size = "sm",br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_axis.ticks'), 'size', value = 0.5 ) ),
                                                      column(width = 12,numericInput(ns('linetype_axis.ticks'), 'linetype', value = 1 ) ),
                                                      column(width = 12,selectInput(ns('color_axis.ticks'), "color",
                                                                                    choices = colors(), selected = "black" ) )
                                                    )
                                     ) ),
                              column(width = 2,
                                     dropdownButton(circle=FALSE, label= lang$t("图例标题"),size = "sm", br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_legend.title'), 'size', value = 20 )),
                                                      column(width = 12,selectInput(ns('color_legend.title'), "color",choices = colors(),  selected = "black" )),
                                                      column(width = 12,numericInput(ns('hjust_legend.title'),"hjust",value = 0 )),
                                                      column(width = 12,numericInput(ns('vjust_legend.title'), 'vjust', value = 0 )),
                                                      column(width = 12,numericInput(ns('angle_legend.title'), 'angle', value = 0 ))
                                                    )
                                     ) ),
                              column(width = 2,
                                     dropdownButton(circle=FALSE, label= lang$t("图例文本"), size = "sm",br(),br(),
                                                    fluidRow(
                                                      column(width = 12,numericInput(ns('size_legend.text'), 'size', value = 15 )),
                                                      column(width = 12,selectInput(ns('color_legend.text'), "color",choices = colors(),  selected = "black")),
                                                      column(width = 12,numericInput(ns('hjust_legend.text'),"hjust",value = 0 )),
                                                      column(width = 12,numericInput(ns('vjust_legend.text'), 'vjust', value = 0 )),
                                                      column(width = 12,numericInput(ns('angle_legend.text'), 'angle', value = 0 ))
                                                    )
                                     )
                              ) ) ) ) ,
                   hr(),
                   splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 600 ) ) ),
               box(width=3,status="success",
                   actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   fluidRow(
                     column(width = 6, selectInput(ns("x"), label = 'x', c("") ) ),
                     column(width = 6, selectInput(ns("y"), label = 'y', c("") ) ),
                     column(width = 6, selectInput(ns("color"), label = 'color', c("") )),
                     column(width = 6, selectInput(ns("fill"), label = 'fill', c("") ) )
                   ),
                   dropdownButton(circle=FALSE, label = "barplot", br(),br(),
                                  fluidRow(
                                    column(width = 6,numericInput(ns("width_barplot"), label =  'width', value = 0.6,min = 0,max = 1) ),
                                    column(width = 6,numericInput(ns("alpha_barplot"), label =  'alpha', value = 0.5,min = 0,max = 1) ),
                                    column(width = 6, selectInput(ns("show.legend_barplot"),label =  'show.legend',
                                                                c("T","F"), selected = 'T' ) )
                                    )
                   ),br(),
                   dropdownButton(circle=FALSE, label = "errorbar", br(),br(),
                                  fluidRow(
                                  column(width = 6,selectInput(ns("color_errorbar"), label =  'color', 
                                                               choices = colors(), selected = "black") ),
                                  column(width = 6,numericInput(ns("width_errorbar"), label =  'width', value = 0.2,min = 0,max = 1) ),
                                  column(width = 6,numericInput(ns("alpha_errorbar"), label =  'alpha', value = 0.5,min = 0,max = 1) )
                                  )
                   ),
                   hr(),
                   dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                  br(),br() ,
                                  numericInput(inputId = ns('w0'),label = lang$t('下载图宽'),value = 15),
                                  numericInput(inputId = ns('h0'),label = lang$t('下载图高'),value = 15),
                                  numericInput(inputId = ns('ppi0'),label = lang$t('分辨率'),value = 72),
                                  
                                  downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("rds0"),  label = "RDS", size='sm', block=TRUE )
                   )
               )
             ) )
  ) }


barplotServer <- function(id) {
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
              df <- data.frame( read_excel(file1$datapath,1) )
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
            
            dat <- as.data.frame(hot_to_r( input$table ) )
            updateSelectInput(session, 'factor', choices = colnames(dat) ,
                              selected = colnames(dat)[1] )
            updateSelectInput(session, 'factor_order', choices = colnames(dat) ,
                              selected = "" )
            updateSelectInput(session,"numeric", choices = colnames(dat) ,
                              selected = colnames(dat)[2] )
            
            updateSelectInput(session,"group", choices = colnames(dat) ,
                              selected = colnames(dat)[1] )
            updateSelectInput(session,"var_select", choices = colnames(dat) ,
                              selected = colnames(dat)[2:ncol(dat)] )
            
            observe({ # observe inner
              
              # 因子型数据
              if(length(input$factor)>=1){
                for ( i in input$factor) {
                  dat[,i] <- factor(dat[,i], ordered = F)
                }
              }
              # 有序数据
              if(length(input$factor_order)>=1){
                for ( i in input$factor_order) {
                  dat[,i] <- factor(dat[,i], ordered = T)
                }
              }
              
              # 数值型数据
              if(length(input$numeric)>=1){
                for ( i in input$numeric ) {
                  dat[,i] <- as.numeric(dat[,i])
                }
              }
              
              observeEvent(input$result, { 
                
                resultTable <- reactive({
                  
                  dat <- dat[,c(input$group,input$var_select)]
                  
                  # 计算平均值（mean）与标准差（sd）
                  df <- data.frame()
                  for (i in unique(dat$group )) {
                    if(input$na_omit){dat <- na.omit(dat)}
                    df0 <- dat[which(dat[,input$group]==i), setdiff(colnames(dat),input$group ) ]
                    
                    if(dim(dat)[2]>2 ){
                      df1 <- data.frame( group = i,
                                         var   = colnames(df0),
                                         mean  = apply(df0, 2, mean ),
                                         sd    = apply(df0, 2, sd ), 
                                         median= apply(df0, 2, median ), 
                                         min   = apply(df0, 2, min ),
                                         max   = apply(df0, 2, max ) 
                      )
                      df <- rbind(df, df1)
                    }
                    else{
                      df1 <- data.frame( group = i,
                                         # var   = colnames(df0),
                                         mean  = mean(df0),
                                         sd    = sd(df0), 
                                         median= median(df0), 
                                         min   = min(df0 ),
                                         max   = max(df0 ) 
                      )
                      df <- rbind(df, df1)
                    }
                    
                  }
                  
                  return(df)
                })
                
                # 输入数据
                if(!is.null(resultTable() ) ){
                  output$resultTable <- renderRHandsontable(
                    rhandsontable(resultTable(),rowHeaderWidth = 100,  height = 400) %>% 
                      hot_cols(columnSorting = TRUE)
                  )
                }
                
                observe({
                  if(!is.null(input$resultTable ) ){
                    
                    df <- as.data.frame(hot_to_r( input$resultTable ) )
                    
                    df[,input$group] <- as.factor( df[,input$group] )
                    
                    # barplot() 参数
                    updateSelectInput(session, "x", label = 'x', choices = colnames(df) ,
                                      selected = input$group )
                    updateSelectInput(session, "y", label = 'y', choices = colnames(df) ,
                                      selected = 'mean'  )
                    updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                                      selected = ""  )
                    updateSelectInput(session, "fill", label = 'fill', choices = colnames(df) ,
                                      selected = "var"  )
                    
                    # 各种图形参数
                    observe({
                      
                      observeEvent(input$submit, {
                        
                        plot <- reactive({
                          
                          expr <- paste0("ggplot(df,aes(x = ", input$x,
                                         ", y =", input$y ,
                                         ",color=",input$color,
                                         ",fill=",input$fill,")",
                                         " )")
                          
                          p <- eval( parse(text =expr ) )
                          
                          # barplot
                          if( T ){
                            if(!input$x==''|!input$y==''){
                              p <- p + geom_bar(stat  = "identity",
                                                position = position_dodge(),
                                                width    = input$width_barplot,
                                                alpha    = input$alpha_barplot,
                                                show.legend = (input$show.legend_barplot=='T')) +
                                geom_errorbar(aes(ymin  = mean - sd,
                                                  ymax  = mean + sd ),
                                              width     = input$width_errorbar,
                                              alpha     = input$alpha_errorbar,
                                              color     = input$color_errorbar,   #  误差线颜色
                                              position  = position_dodge(0.6),
                                              show.legend = F)
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
                })
                
                
                output$downloadResult <- downloadHandler(
                  filename = function() {
                    paste('result.csv')
                  } ,
                  content = function(file) {
                    write.csv(resultTable() , file, row.names = F, fileEncoding = "GB18030")
                    
                  } )
              })
              # obersve
              
            }) # observe inner
            
          }
        })
        
      })
      
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        } ,
        content = function(file) {
          
          dat <- mtcars[,c("cyl","mpg","disp","wt") ]
          colnames(dat)[1] <- 'group'
          write.csv(dat , file, row.names = F, fileEncoding = "GB18030")
          
        } )
      
    } ) }