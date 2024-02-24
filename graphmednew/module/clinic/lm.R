
library(readxl)
library(rhandsontable)
library(ggpubr)
library(ggplot2)
library(ggsci)

library(shinycssloaders) # 加载界面
library(showtext) # 解决画图中文乱码
showtext_auto()
library(shinyWidgets)

color_type <- c("npg","aaas","nejm","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

lm_dataUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
    box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white" ,
        splitLayout(cellWidths = c("60%"), rHandsontableOutput(ns("table") ) ) ),
    box(width = 3,status="success",
        fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
        h6(lang$t('格式：.csv .xlsx .xls')),
        actionBttn( inputId = ns("show"), label = "Show Data", 
                    style = "fill", color = "primary", size = "sm" ),
        hr(),
        column(width = 12,selectInput(ns('factor'),  h6(lang$t("分类变量"),style="color:orange"), c(""), multiple = T )),
        column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("有序变量"),style="color:orange"), c(""), multiple = T )),
        column(width = 12,selectInput(ns('numeric'), h6(lang$t("连续变量"),style="color:orange"), c(""), multiple = T )),
        hr(),
        downloadButton(ns("downloadtable"),lang$t("参考数据") )  ) )
  )
}

lm_plotUI <- function(id){
  ns <- NS(id)
  tagList(
    
  fluidRow(
    box(title=lang$t("拟合图形"),width=9,solidHeader=TRUE,status='primary',background = "white",
        fluidRow(
          column(width =3,
                 dropdownButton(circle=FALSE, label= lang$t("基础"),size = "sm", br(),br(),
                                fluidRow(
                                  column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                                  column(width = 6, textInput(ns('title'), label = "plot.title", value = "Correlation" )),
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
                   ) )
          ) ) ,
        hr(),
        splitLayout(cellWidths = c("100%"), 
                    shinycssloaders::withSpinner( plotOutput(ns("plot") ) )
                     ) ),
    box(width = 3,status="success",
        actionBttn( inputId = ns("submit"), label = "Analyze Data", 
                    style = "fill", color = "primary", size = "sm" ),hr(),
        fluidRow(
          column(width = 6, selectInput(ns("x"), label = 'x', c("") ) ),
          column(width = 6, selectInput(ns("y"), label = 'y', c("") ) ),
          column(width = 6, selectInput(ns("color"), label = 'color', c("") ) ),
          column(width = 6, selectInput(ns("fill"),  label = 'fill' , c("") ) )
        ),
        br(),
        dropdownButton( label = lang$t("参数设置"),icon = icon('image'),circle = FALSE,width = NULL,br(),br(),
                        fluidRow(
                        column( width = 6, selectInput(ns("method"), label = lang$t('拟合方式'),
                                                        selected = 'lm', 
                                                        choices = c('loess','lm','glm','gam') ) ),
                        column( width = 6, selectInput(ns("plot"), label = lang$t('图形选择'),
                                                        selected = "sn",
                                                        choices = c("scatter"="s",
                                                                    "fitting"="n", 
                                                                    "merge"="sn") ) ),
                        column( width = 6, textInput(ns("formula"), label = lang$t('拟合方程'),value = "y~x" ) ),
                        column( width = 6, selectInput(ns("se"),  label = lang$t('置信区间'),
                                                        choices = c('show'= 'T',
                                                                    'hide'= 'F'),
                                                        selected = 'T' ) ),
                        column( width = 6,numericInput(ns("se_level"),label = lang$t("置信水平"), value = 0.95)),
                        column( width = 6, selectInput(ns("cor"), label = lang$t('相关性'),
                                                        choices = c("pearson"="pearson", 
                                                                "kendall"="kendall",
                                                                "spearman"="spearman",
                                                                "不计算"= F ),
                                                        selected = "pearson" ) ),
                        column( width = 6,numericInput(ns("size_plot"),label = lang$t("点大小"), value = 1) ),
                        column( width = 6,numericInput(ns("size_line"),label = lang$t("线大小"), value = 1) ),
                        column( width = 6, selectInput(ns('face'),label = lang$t('是否分面'),
                                                       choices = c('Yes'=T, "No"=F),
                                                       selected = T) )
                        )
        ),br(),
        dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                       br(),br() ,
                       numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 20),
                       numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 10),
                       numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 150),
                       downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                       downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                       downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                       downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                       downloadBttn(outputId = ns("rds0"), label = "RDS",  size='sm', block=TRUE )
        )    )
  ) # fluidRow
  )
}

lm_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，x、y列分别为相关性的两变量，group为分组（color/fill），face为分面（可无）。
                             group对应分组，face对应分面，列名不可改变，但对应列内容可自行编辑。")),
                 tags$h6(lang$t("2、R 与 P仅为直线相关性，对应拟合为 lm, y~x ，其他拟合方式的时候，恐不具有代表性。")),
                 tags$h6(lang$t("3、主题、点线、填充颜色提供数种默认选择。")),
                 tags$h6(lang$t("4、方程中的x与y的对应关系有:")),
                 tags$h6(lang$t("y ~ x ")),
                 tags$h6(lang$t("y ~ poly(x, n)")),
                 tags$h6(lang$t("y ~ log(x)")),
                 tags$h6(lang$t("等。其中 y ~ poly(x, n) 为多项式关系，n为项数，为1、2、3等整数，
                             如 y ~ poly(x, 2) 表示二项式。"))
                 
    )
    ) )
  
}

lmUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', lm_dataUI("lm") ),
      tabPanel(title = 'Plot', lm_plotUI("lm") ),
      tabPanel(title = lang$t("使用说明"), lm_helpUI("lm"), icon = ionicon(name="information-circle") )
      )
  ) # NS(id)
} # function(id)

lmServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
     
      # init
      output$plot <- renderPlot({
        NULL
      })
      
      observeEvent(input$show, {

      # 读取文件 file
      df <- reactive({
        
        file1 <- input$file1
        if( is.null(file1) ){
          
          set.seed(1234) # 随机种子
          df <- data.frame( x = rep(1:50,4),  # 创建数据
                            y = (1:200/10 + rnorm(200, mean = 20, sd = 5)/10 )^3  ,
                            group = rep(c("G1", "G2","G3","G4"), each = 50),
                            face = c(rep("F1", each =100),rep( "F2", each = 100) ) )
          
        }
        else if( !is.null(file1) ){
          
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1) # 文件格式 csv xlsx xls
          
          if( d =='csv' ){
            df <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
          } 
          else{
            df <- data.frame( read_excel(file1$datapath,1) ) 
          } 
        } 
        colnames(df) <-  tolower(colnames(df))
        return(df) # 输出文件
      })

      output$table <- renderRHandsontable(
        rhandsontable(df(), rowHeaderWidth = 22, width = 300, height = 360) %>% 
          hot_cols(columnSorting = TRUE) %>% 
          hot_col(c('x','y'), type = 'numeric' ) %>% 
          hot_col(c('group','face'), type = 'dropdown' )
      )
      
     

        observe({
          if(!is.null(input$table ) ){
            df <- as.data.frame(hot_to_r( input$table ) )
            
            updateSelectInput(session, 'factor', choices = colnames(df) ,
                              selected = colnames(df)[3:ncol(df)] )
            updateSelectInput(session,"numeric" , choices = colnames(df) ,
                              selected = colnames(df)[1:2] )
            updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                              selected = "" )
            
            updateSelectInput(session, "x", label = 'x', choices = colnames(df) ,
                              selected = colnames(df)[1]  )
            updateSelectInput(session, "y", label = 'y', choices = colnames(df) ,
                              selected = colnames(df)[2]  )
            updateSelectInput(session, "color", label = 'color', choices = colnames(df) ,
                              selected = colnames(df)[3]  )
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
            
       observeEvent(input$submit, {
         # 分析处理数据
          
          plot <- reactive({
            
            df <- hot_to_r( input$table )
            df <- as.data.frame( df )
            df <- na.omit(df)
            
            
            expr <- paste0("ggplot(df,aes(x =", input$x,
                           ", y =", input$y , 
                           ",color=",input$color,
                           ",fill=",input$fill,")",
                           " )")
            
            p <- eval( parse(text =expr ) )
            
            if(input$plot=='s'){
              p <- p + geom_point(size=input$size_plot)
            }
            else if(input$plot=='n'){
              
              if(input$method=='gam'){
                
                p <- p + geom_smooth( method = input$method, 
                                      show.legend = F,
                                      se = (input$se=='T') ,
                                      level=input$se_level,
                                      size=input$size_line)
                
              }else{
                p <- p + 
                  geom_smooth( method = input$method, 
                               show.legend = F, 
                               formula = input$formula,
                               se = (input$se=='T') ,
                               level=input$se_level,
                               size=input$size_line)
              }
            }
            else{
              if(input$method=='gam'){
                p <- p + 
                  geom_point(size=input$size_plot)+
                  geom_smooth( method = input$method, 
                               show.legend = F, 
                               se = (input$se=='T'),
                               level=input$se_level,
                               size=input$size_line)
              } else{
                p <- p + 
                  geom_point( size=input$size_plot )+ 
                  geom_smooth( method = input$method, 
                               show.legend = F, 
                               formula = input$formula,
                               se = c(input$se=='T'),
                               level=input$se_level,
                               size=input$size_line )
              }
            }

            p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
            
            if(!input$title==''){
              p <- p + ggtitle( input$title )
            }
            if(!input$x.axis.title==''){
              p <- p + xlab( input$x.axis.title )
            }
            if(!input$y.axis.title==''){
              p <- p + ylab( input$y.axis.title )
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
                plot.title   = element_text(size  = input$size_title,
                                            hjust = input$hjust_title,
                                            color = input$color_title,
                                            vjust = input$vjust_title,
                                            angle = input$angle_title
                ),
                axis.title   = element_text(size  = input$size_axis.title,
                                            color = input$color_axis.title,
                                            hjust = input$hjust_axis.title,
                                            vjust = input$vjust_axis.title,
                                            angle = input$angle_axis.title
                ),
                axis.text    = element_text(size  = input$size_axis.text,
                                            color = input$color_axis.text,
                                            hjust = input$hjust_axis.text,
                                            vjust = input$vjust_axis.text,
                                            angle = input$angle_axis.text
                ),
                axis.ticks   = element_line(size  = input$size_axis.ticks,
                                            color = input$color_axis.ticks
                ),
                legend.title = element_text(size  = input$size_legend.title,
                                            hjust = input$hjust_legend.title,
                                            color = input$color_legend.title,
                                            vjust = input$vjust_legend.title,
                                            angle = input$angle_legend.title
                ),
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

            # 不分面
            if ( input$face== F ) {
              
              # 计算相关性系数 R 与 P
              if(!input$cor== F ){ 
                p <- p +  stat_cor(method = input$cor , size=6,  show.legend = F,
                                   data = df, p.accuracy = 0.001, r.accuracy = 0.01,na.rm = T ) 
              } # else 不计算 
              
            }
            # 分面 
            else if(input$face == T){    
              
              # 计算相关性系数 R 与 P
              if(!input$cor== F ){ 
                
                for (i in 1:length(unique(df$face) ) ) {
                  p <- p + # 自定义 填充颜色
                    stat_cor(method = input$cor , aes(x,y), size=5, show.legend = F,
                             data = subset(df,face== unique(df$face)[i] ),
                             p.accuracy = 0.001, r.accuracy = 0.01 ,na.rm = T)
                }
              } # else 不计算 
              p <- p + facet_wrap("~ face")
              
            }
            
            
            return(p)
          })
          
          
          # 4、拟合图像，网页呈现
          output$plot <-  renderPlot({
            return( plot() )
            
          })
          
          # 下载图形
          if(T){
            output$pdf <- downloadHandler(
              filename="cor.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print( plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="cor.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="test.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="cor.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( plot() )
                dev.off()
              }
            )
            output$rds0 <- downloadHandler( 
              filename="cor.RDS",
              content = function(file){
                saveRDS( plot() ,file)
              }  )
          }
          
          
        })  # observeEvent(input$submit )
        
            }) # observe
          }
      }) # observeEvent( input$table ) 
      
      })
      
      # 下载参考数据
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('cor.csv')
        },
        content = function(file) {
          
          if(is.null( input$table ) ){
            df <- df()
          }
          else{
            df <- hot_to_r( input$table )
          }
          write.csv(df, file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
    } # function(input, output, session)
  ) # moduleServer
} # function(id)