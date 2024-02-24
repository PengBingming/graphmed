
# library('readxl')
# library("survival")
# library("survminer")
library('rhandsontable')
library('ggplot2')     # 画图
# library("showtext") #中文问题
showtext::showtext_auto()

survServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data # 读取数据
        data <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            data <- read.csv('./www/surv.csv')
          } 
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              data <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              data <- data.frame( readxl::read_excel(file1$datapath,1) )
            } 
          } # else
          return( data )
        })
        
        # 输入数据
        if(!is.null(data() ) ){
          output$table <- renderRHandsontable(
            rhandsontable(data(),rowHeaderWidth = 22,  height = 400) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
        
        
        observe({
          if(!is.null(input$table ) ){
            data <- as.data.frame(hot_to_r( input$table ) )
            
            # 实验组与对照组情况
            updateSelectInput(session, "time", label = '时间', choices = colnames(data), selected = colnames(data)[1]  )
            updateSelectInput(session, "status"  , label = '状态', choices = colnames(data) , selected = colnames(data)[2]  )
            updateSelectInput(session, "group"  , label = '分组', choices = colnames(data) , selected = colnames(data)[3]  )
          }
        } ) 
        
        observeEvent(input$submit, {
          
          plot <- reactive({
            data <- hot_to_r( input$table)
            expr <- paste0("survival::survfit(survival::Surv(",input$time,",",input$status,") ~ ",input$group,", data = data)" )
            fit <- eval( parse(text = expr ) )
            
            p <- survminer::ggsurvplot(fit, # 创建的拟合对象
                       data = data,  # 指定变量数据来源
                       conf.int = (input$conf.int=='T'), # 显示置信区间
                       pval = (input$pval =='T'), # 添加 P 值
                       surv.median.line = input$median.line,  # 添加中位生存时间线
                       risk.table = (input$risk.table =='T'), # 添加风险表
                       add.all = (input$add.all =='T'), # 添加总患者生存曲线
                       palette = input$palette,
                       xlab = input$xlab, # 指定x轴标签
                       legend.title = input$legend # 设置图例标题
                       ) 
            
            return(p)
          })
          
          output$plot <- renderPlot({
            return(plot() )
          })
          
          if(T){
            output$pdf <- downloadHandler(
              filename="plot.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="plot.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="plot.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="plot.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }  )
            
            output$rdata <- downloadHandler( 
              filename="sur.rdata",
              content = function(file){
                p <- plot()
                save(p,file = file)
              } )
          }
        })
        
        observeEvent(input$submit1, {
          # 累计曲线
          plot1 <- reactive({
            data <- hot_to_r( input$table)
            expr <- paste0("survival::survfit(survival::Surv(",input$time,",",input$status,") ~ ",input$group,", data = data)" )
            fit <- eval( parse(text = expr ) )
            
            p <-  survminer::ggsurvplot(fit, # 创建的拟合对象  
                             data = data,  # 指定变量数据来源
                             conf.int = (input$conf.int1=='T'), # 显示置信区间
                             pval = (input$pval1 =='T'), # 添加 P 值
                             risk.table = (input$risk.table1 =='T'), # 添加风险表
                             add.all = (input$add.all1 =='T'), # 添加总患者生存曲线
                             palette = input$palette1,
                             xlab = input$xlab1, # 指定x轴标签
                             legend.title = input$legend1, # 设置图例标题
                             fun = "cumhaz") 
              
            return(p)
          })

          output$plot1 <- renderPlot({
            return(plot1() )
          })
          
          if(T){
            output$pdf1 <- downloadHandler(
              filename="plot1.pdf",
              content = function(file){
                pdf(file,width=input$w1,height=input$h1)
                print(plot1() )
                dev.off()
              }
            )
            output$png1 <- downloadHandler(
              filename="plot1.png",
              content = function(file){
                png(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }
            )
            output$jpeg1 <- downloadHandler(
              filename="plot1.jpeg",
              content = function(file){
                jpeg(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }
            )
            output$tiff1 <- downloadHandler( 
              filename="plot1.tiff",
              content = function(file){
                tiff(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                print(plot1() )
                dev.off()
              }  )
            
            output$rdata1 <- downloadHandler( 
              filename="roc1.rdata",
              content = function(file){
                p <- plot1()
                save(p,file = file)
              } )
            
          }
          })
      }) # show
      
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('surv.csv')
        } ,
        content = function(file) {
          data <- read.csv('./www/surv.csv')
          write.csv(data , file, row.names = F, fileEncoding = "GB18030")
        }  ) 
      
    } )
}
