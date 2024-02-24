
library(rhandsontable)
# 编写函数
myfun_fp <- function(data){
  
  # 整理数据
  fit <- data
  
  df1 <- cbind( exp( coef(fit) ) ,  # OR
                summary(fit)$coefficients[,4], # P
                exp( confint(fit) ) # 置信区间
  ) 
  df1 <- data.frame(df1)[-1,]
  df1 <- cbind('Var'=rownames(df1),df1)
  colnames(df1)[-1] <- c("OR","Pvalue","OR_lower","OR_upper")
  
  df2 <- df1
  df2$OR_mean <- df2$OR
  df2$OR <- paste0(   round(df2$OR,2),   # OR
                      "(", round(df2$OR_lower,2), # OR_lower
                      "~", round(df2$OR_upper,2), # OR_upper
                      ")")
  df2$Pvalue <- ifelse( df2$Pvalue>=0.001 , round(df2$Pvalue,3), "<0.001")
  df2
  
  fp <- rbind("lables"=NA,df2)
  fp[1, 1:3]  <-  c('Variable', 'OR(95% CI)', 'Pvalue')
  
  return(fp)
}

# 画图
myfun_plot <-  function(data,input){
  library(ggplot2)
  fit <- data
  df1 <- cbind( 'OR' = round(exp( coef(fit) ), 2) ,  # OR
                "Pvalue"= round( summary(fit)$coefficients[,4], 3), # P
                "Lower"= round(exp( confint(fit) )[,1], 2 ), # 置信区间
                "Upper"= round(exp( confint(fit) )[,2] , 2)
  ) 
  
  df1 <- data.frame(df1)[-1,]
  df1 <- cbind('Var'=rownames(df1),df1)
  
  df1$Factor <- ifelse(df1$Lower>1,'Risk',ifelse(df1$Upper<1,'Protective','Not sig.'))
  
  df1$Pvalue1 <- ifelse(df1$Pvalue >= 0.001,df1$Pvalue,'<0.001') # p值 0.001 为界
  
  df1 <- df1[order(df1$OR,decreasing = T),]
  df1$Var1 <- factor(1:nrow(df1))
  
  annotation <- data.frame(matrix("",ncol = 3,nrow = c(3*nrow(df1)+3) ))
  colnames(annotation) <- c('x','y','label')
  annotation$label <- c('OR (95% CI)','Odds  Ratio','P Value',paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')'),df1$Pvalue1, df1$Var)
  annotation$x <- c( c(-0.3,1,-0.75),rep(-0.3, nrow(df1) ),rep(-0.75,nrow(df1) ), rep(-1.1, nrow(df1)) )
  annotation$y <- c(rep(nrow(df1)+0.47,3),seq(1, nrow(df1), 1),seq(1, nrow(df1), 1) ,seq(1, nrow(df1),1 ) )


  p <-  ggplot(df1, aes(OR, Var)) +
    geom_point(size=3.6, aes(col=Factor)) +
    geom_errorbarh(aes(xmax =Upper, xmin = Lower), col=input$lines, height = 0.4)+
    geom_vline(aes(xintercept=1),col=input$zero)+
    scale_x_continuous(limits=c(-0.9, max(df1$Upper)), breaks=seq(0,max(df1$Upper) , 0.5)) +
    geom_text(data=annotation,aes(x=x,y=y,label=label) ,size=6)
    
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
    if(!input$color_name==''){
      color <- paste0("labs(color='",input$color_name,"')")
      p <- p + eval(parse(text = color ))
    }
    
  p <- p +
    theme(
      legend.position ="top",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill =input$background),
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
      axis.ticks   = element_line(linewidth = input$size_axis.ticks,
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
  # p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))
  
  return(p)
}

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

color_type <- c("npg","aaas","nejm","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')


logis_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6("1、参考数据中，因变量为二分类，在参数设置中选定（y），
                             默认第一列。其余列为自变量，可为连续变量、分类变量。"),
                 tags$h6("2、自变量为计数变量（有序及无序）时，先在变量类型中选择对应数据。",style="color:orange"),
                 tags$h6("3、自变量为连续变量（计量）时，可不选择，但不要有缺失值。"),
                 tags$h6("4、可在参数设置中对这两类变量进行选定，然后点击“Analyze Data”。")
                 
    )
    ) )
  
}

logisUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("df")) ) 
                 ), 
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data", 
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     column(width = 12,selectInput(ns('factor'),  h6(lang$t("分类变量"),style="color:orange"), c(""), multiple = T )),
                     column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("有序变量"),style="color:orange"), c(""), multiple = T )),
                     column(width = 12,selectInput(ns('numeric'), h6(lang$t("连续变量"),style="color:orange"), c(""), multiple = T )),
                     hr(),
                     downloadButton(ns("downloadtable"),lang$t("参考数据") )
                 ) )
               ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("Logistics 结果"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("contents1"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     selectInput(ns("group"), h6(lang$t("group：二分类"),style="color:orange"), c(""),multiple = F ),
                     selectInput(ns("var_select"), h6("var.select",style="color:orange"), c(""), multiple = T ),
                     selectInput(ns("type"), lang$t("筛选变量"), selected = "full",
                                 c('all variables'='full',  'step regression'='step') ),
                     hr(),
                     downloadButton(ns("downloadtable1"),lang$t("结果数据") )
                 )
                 )
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("森林图"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     fluidRow(
                       column(width =3,
                              dropdownButton(circle=FALSE, label= lang$t("基础"),size = "sm", br(),br(),
                                             fluidRow(
                                               column(width = 6, selectInput(ns('theme'),label = "theme",selected = 'bw',choices = theme_select )),
                                               column(width = 6, textInput(ns('title'), label = "plot.title", value = "Logistic Forstplot" )),
                                               column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "OR" )),
                                               column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "Var" ) ),
                                               column(width = 6, textInput(ns("color_name"),label = "color.name",value = "Factor") ),
                                               column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) )
                                               # column(width = 6, textInput(ns("fill_name"),label = "fill.name",value = "") ),
                                               # column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                                             )
                              ) ),
                       column(width =9,
                              fluidRow(
                                column(width =2,
                                       dropdownButton(circle=FALSE, label= lang$t("标题"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_title'),  'size', value = 30 )),
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
                                                        column(width = 12,numericInput(ns('size_axis.title'), 'size', value = 20 )),
                                                        column(width = 12,selectInput(ns('color_axis.title'), "color",choices = colors(),  selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_axis.title'),"hjust",value = 0.5 )),
                                                        column(width = 12,numericInput(ns('vjust_axis.title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_axis.title'), 'angle', value = 0 ))
                                                      )
                                       ) ) ,
                                
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("轴文本"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_axis.text'), 'size', value = 15 )),
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
                                                        column(width = 12,numericInput(ns('size_legend.title'), 'size', value = 15 )),
                                                        column(width = 12,selectInput(ns('color_legend.title'), "color",choices = colors(),  selected = "black" )),
                                                        column(width = 12,numericInput(ns('hjust_legend.title'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.title'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.title'), 'angle', value = 0 ))
                                                      )
                                       ) ),
                                column(width = 2,
                                       dropdownButton(circle=FALSE, label= lang$t("图例文本"), size = "sm",br(),br(),
                                                      fluidRow(
                                                        column(width = 12,numericInput(ns('size_legend.text'), 'size', value = 10 )),
                                                        column(width = 12,selectInput(ns('color_legend.text'), "color",choices = colors(),  selected = "black")),
                                                        column(width = 12,numericInput(ns('hjust_legend.text'),"hjust",value = 0 )),
                                                        column(width = 12,numericInput(ns('vjust_legend.text'), 'vjust', value = 0 )),
                                                        column(width = 12,numericInput(ns('angle_legend.text'), 'angle', value = 0 ))
                                                      )
                                       )
                                ) ) ) ) ,
                     hr(),
                     splitLayout(cellWidths = c("100%"), plotOutput(ns('plot') ) )  
                 ) ,
                 box(width = 3, status="success",
                     selectInput(ns("background"),  lang$t("背景颜色"), colors() , selected = "skyblue" ),
                     selectInput(ns("zero"),        lang$t("竖线颜色"), colors() , selected = "black" ),
                     selectInput(ns("lines"),       lang$t("横线颜色"), colors() , selected = "black" ) 
                     , br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rds0"),  label = "RDS",  size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Plot data',
               fluidRow(
                 box(title=lang$t("Forestplot 数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("contents2"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     downloadButton(ns("downloadtable2"),lang$t("图形数据") )
                 )
               )
               ),
      tabPanel(title = 'Help',logis_helpUI("logis") )
      )
  ) #  tagList
} # function(id)
          
logisServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    observeEvent(input$show, {
      # 数据：读取输入文件或参考数据
      df <- reactive({
        
        file1 <- input$file1
        
        if(is.null(file1)){
          df1 <- readRDS('./www/logistic_df.RDS')
        } 
        else{
          
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          
          if(d=='csv'){
            df1 <- data.frame( read.csv(file1$datapath,fileEncoding = "GB18030") )
          } else{
            df1 <- data.frame( read_excel(file1$datapath,1) )
          } 
          
        } # else
        
        return(df1)
        
      })
      
      output$df <- renderRHandsontable(
        rhandsontable(df(),rowHeaderWidth = 50, height = 370) %>% 
          hot_cols(columnSorting = TRUE)
      ) 
      
      observe({
        df <- df()
        
        updateSelectInput(session, 'factor', choices = colnames(df) ,
                          selected = colnames(df)[1:3] )
        updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                          selected = "" )
       
        updateSelectInput(session,"numeric" , choices = colnames(df) ,
                          selected = "" )
        
        updateSelectInput(session, 'group', choices = colnames(df),
                          selected = colnames(df)[1] )
        updateSelectInput(session, 'var_select', choices = colnames(df),
                          selected = colnames(df)[2:ncol(df)] )
        
      })

      observeEvent(input$submit1, {
        
        myfun <- reactive({
          if(is.null(input$df)){return(NULL)}
          df <- hot_to_r( input$df )

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
          
          df <- df[,c(input$group,input$var_select) ]
          
          logi <- paste0("glm(","data = df,",
                         input$group,"~ .,",
                         "family = binomial(link='logit') ",")")
          fit.full <- eval( parse(text =logi ) )
          
          # fit.full <- glm(group_yn~ . , # 所有变量
          #                 data = df, # 数据集
          #                 family = binomial(link='logit') # 拟合方式
          # )

          if(input$type == 'full'){
            
            plot <- myfun_plot(fit.full,input)
            fit <- as.data.frame(summary( fit.full )$coefficients)
            fp <- myfun_fp( data = fit.full )
            
          }
          else if(input$type == 'step'){
            
            fit.step <- step(object = fit.full,trace = 0)
            
            plot <- myfun_plot(fit.step,input)
            fit <- as.data.frame(summary( fit.step )$coefficients)
            fp <- myfun_fp( data = fit.step )
            
          }

          fit <- cbind( 'Var'=rownames(fit), fit )
          
          results <-list()
          
          results$fp <- fp
          results$fit <- fit 
          results$plot <- plot
          
          return( results )
          
        })
        
        fp <- reactive({
          if(is.null(myfun())){return(NULL)}
          return(myfun()$fp)
        })
        
        fit <- reactive({
          if(is.null(myfun())){return(NULL)}
          return(myfun()$fit )
        })

        plot <- reactive({
          if(is.null(myfun())){return(NULL)}
          
          p <- myfun()$plot
          return(p)
        })
        
        # 森林图图形
        output$plot <- renderPlot({
          return(plot() )
        }  )
        
        # 展示 logistic 结果表格
        output$contents1 <- renderDataTable({
          return(fit() )
        })  
        
        # 展示森林图表格
        output$contents2 <- renderDataTable({
          return(fp() )
        })  
        
        if(T){
          output$pdf0 <- downloadHandler(
            filename="logisfp.pdf",
            content = function(file){
              pdf(file,width=input$w0,height=input$h0)
              print( plot() )
              dev.off()
            }
          )
          output$png0 <- downloadHandler(
            filename="logisfp.png",
            content = function(file){
              png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print( plot() )
              dev.off()
            }
          )
          output$jpeg0 <- downloadHandler(
            filename="logisfp.jpeg",
            content = function(file){
              jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print(plot())
              dev.off()
            }
          )
          output$tiff0 <- downloadHandler( 
            filename="logisfp.tiff",
            content = function(file){
              tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print( plot())
              dev.off()
            } )
          output$rds0 <- downloadHandler( 
            filename="logisfp.RDS",
            content = function(file){
              saveRDS( plot() ,file)
            }  )
        }
        
        
        # 1、下载logistic回归结果
        output$downloadtable1 <- downloadHandler(
          filename = function() {
            paste('logistic.csv')
          },
          content = function(file) {
            write.csv(fit() , file,  row.names = F, fileEncoding = 'GB18030')
          } )
        
        # 2、下载森林图结果
        output$downloadtable2 <- downloadHandler(
          filename = function() {
            paste('plot.csv')
          },
          content = function(file) {
            write.csv(fp() , file,  row.names = F, fileEncoding = 'GB18030')
          } )
        
        
      })
      
      
      
      
    })

       # 0、下载参考数据
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        },
        content = function(file) {
          df <- readRDS('./www/logistic_df.RDS')
          write.csv(df , file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
      
    } # function(input, output, session)
  )  # moduleServer
} # function(id)
          