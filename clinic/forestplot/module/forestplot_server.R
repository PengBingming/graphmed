
# library('readxl') # 读取 Excel
library('ggplot2')
library('ggsci')
# library(showtext) # 解决画图中文乱码
showtext::showtext_auto()

if(T){
  sample <-  gsub('\n','\t','rating	0	0.63	0.52	0.75
religiousness	0	0.72	0.6	0.86
age	0.015	0.96	0.92	0.99
education	0.677	1.02	0.93	1.13
occupation	0.667	1.03	0.9	1.19
yearsmarried	0.003	1.1	1.03	1.17
gendermale	0.241	1.32	0.83	2.12
childrenyes	0.173	1.49	0.85	2.66')

  sample <- as.data.frame(matrix(unlist(strsplit(sample,'\t')),ncol = 5,byrow = T))
  colnames(sample) <- c('Var','Pvalue','OR','Lower','Upper')
  
  sample[,-1] <- apply(sample[,-1],2,as.numeric)
}

forestplotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    observeEvent(input$show, {
      # 数据：读取输入文件或参考数据
      df1 <- reactive({
        
        file1 <- input$file1
        
        if(is.null(file1)){
          df1 <- sample
        } 
        else{
          
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          
          if(d=='csv'){
            df1 <- data.frame( read.csv(file1$datapath,fileEncoding = "GB18030") )
          } else{
            df1 <- data.frame( readxl::read_excel(file1$datapath,1) )
          } 
          
        } # else
        
        return(df1)
        
      })
      
      df <- na.omit(df1() )
      
      output$table <- renderRHandsontable(
        
        df %>% rhandsontable( rowHeaderWidth = 22,height = 300 )%>% 
          hot_cols(columnSorting = TRUE)
      )
      
      observe({
        if( !is.null(input$table) ){
          df <- hot_to_r( input$table )
        
        updateSelectInput(session, 'factor', choices = colnames(df) ,
                          selected = colnames(df)[1] )
        updateSelectInput(session, 'factor_order', choices = colnames(df) ,
                          selected = "" )
        
        updateSelectInput(session,"numeric" , choices = colnames(df) ,
                          selected = colnames(df)[2:5] )

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

          forestplot <- reactive({
            
            mydata <- df # as.data.frame(hot_to_r( input$table ))
            # 森林图画图函数
              library(ggplot2)
              df1 <- na.omit(mydata)
              
              df1$Factor <- ifelse(df1$Lower > 1,'Risk',ifelse(df1$Upper < 1,'Protective','Not sig.'))
              df1$`OR (95% CI)` <- paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')')
              
              df1$Pvalue.sig <- ifelse(df1$Pvalue >= 0.001,df1$Pvalue,'<0.001')
              
              df1 <- df1[order(df1$OR,decreasing = T), ]
              m <- df1$Var
              df1$Var <- factor(df1$Var,levels = m)
              # df1$Var1 <- factor( 1:nrow(df1) )
              
              annotation <- data.frame(matrix("",ncol = 3,nrow = 2*nrow(df1)+3 ))
              colnames(annotation) <- c('x','y','label')
              
              annotation$label <- c('OR (95% CI)','Odds Ratio','P Value',paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')'),df1$Pvalue.sig)
              
              annotation$x <- c( c(-0.3,1,-0.7),rep(-0.3, nrow(df1) ),rep(-0.7,nrow(df1) ) )
              
              annotation$y <- c(rep(nrow(df1)+ 0.4,3),seq(1, nrow(df1), 1),seq(1, nrow(df1), 1)  )
              
              p <-  ggplot(df1, aes(OR, Var)) +
                geom_point(size=3.6, aes(col = Factor)) +
                geom_errorbarh(aes(xmax = Upper, xmin = Lower), col=input$lines, height = 0.4)+
                geom_vline(aes(xintercept=1), col = input$zero)+
                scale_x_continuous(limits=c(-0.7, max( df1$Upper) ), breaks=seq(0, max(df1$Upper) , 0.5))+
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
            
            return( p )
          })
          
          output$plot <- renderPlot({
            
            plot <- forestplot()
            return(plot)
            
          } ) 

          if(T){
            output$pdf <- downloadHandler(
              filename="Forestplot.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print( forestplot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="Forestplot.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="Forestplot.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="Forestplot.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              } )
          
            output$rds <- downloadHandler( 
              filename="Forestplot.RDS",
              content = function(file){
                saveRDS(forestplot(),file)
              } )
            }

          
        } )
        
       
      })
      
        }
        
      })
      
      } )
      # 1、下载 结果
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('Forestplot.csv')
        },
        content = function(file) {
          
          if( is.null(input$table ) ){
            mydata <- sample
          }
          else{
            mydata <- hot_to_r( input$table )
          }
          write.csv(mydata, file,  row.names = F, fileEncoding = 'GB18030')
          
        } )
      
    } # function(input, output, session)
  ) # moduleServer
} # function(id)