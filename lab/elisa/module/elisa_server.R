
library('readxl')
library('drc')
library('ggplot2')
library('patchwork') # 合并图片
# library('showtext')
showtext::showtext_auto() 

elisaServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, {
        # 读取 ELISA 数据
        df1 <- reactive({
          
          file1 <- input$file1
          if( is.null(file1) ){
            if(input$rep == T){ df1 <- readRDS('www/with.RDS') }
            else{ df1 <- readRDS('www/without.RDS') }
          }
          else if(!is.null(file1)){
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if(d=='csv'){
              df1 <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
            } else{
              df1 <- data.frame( read_excel(file1$datapath,1) ) 
            } 
          } 
          colnames(df1) <-  tolower( colnames(df1) )
          return(df1)
        })
        
        # 输入数据
        output$table <- renderRHandsontable(
          rhandsontable(df1(),rowHeaderWidth = 22, height = 300 ) %>% hot_col(colnames(df1() ),type = 'numeric')
        )
        
        observeEvent(input$submit1, {
          
          # 分析处理 ELISA数据
          elisa <- reactive({
            # 1、读取数据
            if(is.null(input$table ) ){return(NULL)} 
            
            df1 <- hot_to_r( input$table )
            df1 <- as.data.frame( df1 )
            colnames(df1) <- tolower(colnames(df1))
            
            # 2、判断有无副孔
            if(input$rep == T){
              
              nm2 <- vector() 
              for (i in 1:( ncol(df1)/2-1) ) {
                nm1 <- c(paste0('y',i) , paste0('y',i,'copy') )
                nm2 <-c(nm2,nm1)
              }
              
              colnames(df1)[3:ncol(df1)] <- nm2
              
              k1 <- 2*(2:(dim(df1)[2]/2))-1 ; k2 <- 2*(2:(dim(df1)[2]/2))
              df1 <- cbind(df1[,1:2],(df1[,k1] + df1[,k2])/2)
              df2 <- df1
              
            } else  (df2 <- df1 )
            
            # 3、直线回归
            if(input$type == "line"){
              
              fit1 <-lm(y ~ x, data=df1) 
              a <- fit1$coefficients[2]; b <- fit1$coefficients[1] # 参数赋值
              
              f <- paste0('y  ≈  ',round(a, 6),'x +  ',round(b, 6) ) # 函数
              r <- paste0('Adjusted R-squared ≈ ', round(as.numeric(summary(fit1)[9]), 3) ) # R方
              
              p1 <- ggplot( data = df1, aes(x = x, y = y) ) +
                geom_point(shape = 1 ,size=0.1) +
                geom_smooth(data = df1,  aes(x = x, y = y), color= 'gray60', method = "gam", formula = y ~ poly(x,1) ) +
                annotate("text", x = (max(df1$x)-min(df1$x))/2 , y = max(df1$y)*0.98, label = paste0(f , '\n' , r) , colour="black") +
                labs(title = 'ELISA : binomial',
                     x = "x : conc", 
                     y = "y : OD") 
              p1 <- p1 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p1 <- p1 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )
              
              # 构建由 y 求 x 函数
              myfun <- function(y,a,b){
                x <- round((y-b)/a , input$round) # 求 k 值
                return(x) # 输出计算值
              }
              
              x1 <- myfun(y=df1$y,a=a,b=b)
              
              y0 <- as.matrix(df1[,3:dim(df1)[2] ]) # 取96孔板其余列 OD值（y）
              x0 <- myfun(y=y0,a=a,b=b) # 求浓度（x）,如果报错可能为OD值超过函数界值。
              
              df2[, 3:dim(df2)[2] ]<-matrix(x0, ncol = dim(df2)[2]-2, nrow = 8,byrow = F)
              df2[, dim(df2)[2]+1 ]<-x1 ; colnames(df2)[(dim(df2)[2])]<-'y→fitting→x'
              
              df2 <- as.data.frame(df2)
              
              df_f <- data.frame('od' = runif(1000, min = min(df1$y), max = max(df1$y) )) 
              df_f$conc <- as.numeric(myfun(y= df_f$od,a=a,b=b))
              
              p2 <- ggplot(data = df_f , aes(x = od , y = conc) ) + 
                geom_line( color= 'pink3' ) +
                geom_point( aes(x= y, y = x ) , shape= 1 ,data = df1 ,size=0.1) +
                ggtitle("ELISA : binomial") + xlab("OD") + ylab("Conc")
              p2 <- p2 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p2 <- p2 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )
              standard_plot <- p1 + p2
            }
            # 4、二项式回归
            else if(input$type == "binomial"){
              
              fit2<-lm(y ~ x+ I(x^2), data=df1) # 拟合曲线，x为浓度，y为OD值
              
              a <- fit2$coefficients[3]; b <- fit2$coefficients[2]; c <- fit2$coefficients[1] # 参数赋值
              
              f <- paste0('y  ≈  ',round(a,8),'x^2  +  ',round(b, 4),'x +  ',round(c, 4) ) # 函数
              r <- paste0('Adjusted R-squared ≈ ', round(as.numeric(summary(fit2)[9]), 3) ) # R方
              
              p1 <- ggplot( data = df1, aes(x = x, y = y) ) +
                geom_point(shape = 1,size=0.1 ) +
                geom_smooth(data = df1,  aes(x = x, y = y), color= 'gray60', method = "gam", formula = y ~ poly(x,2) ) +
                annotate("text", x = (max(df1$x)-min(df1$x))/2 , y = max(df1$y)*0.98, label = paste0(f , '\n' , r) , colour="black") +
                labs(title = 'ELISA : binomial',
                     x = "x : conc", 
                     y = "y : OD") 
              p1 <- p1 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p1 <- p1 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )
              
              # 构建由 y 求 x 函数
              myfun <- function(y,a,b,c){
                k<- 1/a # 求 k 值
                n<- -b/(2*a) # 求 n 值
                m<-(n)^2-(c/a) # 求 m 值
                
                # sqrt() 为开方，前面符号为±，按实际情况(是否大于0)选择 +（x1） 或 -（x2）
                if(a<0){
                  x <- round(-sqrt(k*y+m)+n, input$round) # a大于0时的解
                }
                else if(a>0){
                  x <- round(sqrt(k*y+m)+n, input$round) # a小于0时的解
                }
                return( x ) # 输出计算值
              }
              
              x1<-myfun(y=df1$y,a=a,b=b,c=c)
              
              y0 <- as.matrix(df1[,3:dim(df1)[2] ]) # 取96孔板其余列 OD值（y）
              x0 <- myfun(y=y0,a=a,b=b,c=c) # 求浓度（x）,如果报错可能为OD值超过函数界值。
              
              df2[, 3:dim(df2)[2] ]<-matrix(x0, ncol = dim(df2)[2]-2, nrow = 8,byrow = F)
              df2[, dim(df2)[2]+1 ]<-x1 ; colnames(df2)[(dim(df2)[2])]<-'y→fitting→x'
              
              df_f <- data.frame('od' = runif(1000, min = min(df1$y), max = max(df1$y) )) 
              df_f$conc <- as.numeric(myfun(y= df_f$od,a=a,b=b,c=c))
              
              p2 <- ggplot(data = df_f , aes(x = od , y = conc) ) + 
                geom_line( color= 'pink3' ) +
                geom_point( aes(x= y, y = x ) , shape= 1 ,data = df1 ,size=0.1) +
                # xlim( min(df1$y) , max(df1$y) )+
                # ylim( min(df1$x) , max(df1$x) )+
                ggtitle("ELISA : binomial") + xlab("OD") + ylab("Conc")
              p2 <- p2 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p2 <- p2 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )
              standard_plot <- p1 + p2
            }
            # 5、logistic 回归
            else if(input$type == "logistic"){
              
              library(drc)
              
              # 拟合
              pl4 <- drm( y~x , fct=LL.4( names=c("Slope", "Lower", "Upper", "ED50") ), data= df1)
              RSD <- abs(sqrt(summary(pl4)$"resVar") / mean(fitted(pl4))) # 相对标准偏差RSD
              
              # 提取参数
              var <- as.numeric(pl4[["coefficients"]])
              
              UpperLimit <- var[3] ; EC50 <- var[4] 
              Slope <- -var[1] ; LowerLimit <- var[2] 
              
              # 设置函数
              myfun<- function(y,a,b,c,d){
                x <- ((a-d)/(y-d)-1)^(1/b)*c
                return( round(x, input$round) )
              }
              
              # 查看标曲拟合度
              x1 <- myfun( y=df1$y, a=LowerLimit , b= Slope, c=EC50, d=UpperLimit)
              
              # 计算后赋值给df2
              df_r <- dim(df1)[1]; df_n <- dim(df1)[2]
              
              y0 <- as.matrix(df1[,3:df_n]) # 取96孔板其余列 OD值（y）
              
              x0 <- myfun(y0, a = LowerLimit , b = Slope , c = EC50, d = UpperLimit) # 求浓度（x）,如果报错可能为OD值超过函数界值。
              
              df2[,3:df_n] <- matrix(x0,ncol = df_n-2,nrow = df_r,byrow = F)
              
              df2[,df_n+1] <- x1 ; colnames(df2)[df_n+1] <- "y→fitting→x"
              
              # 四参数logistic拟合图
              
              df_f <- data.frame('conc' = runif(1000, min = min(df1$x), max = max(df1$x) ) )
              
              pm <- predict(pl4, newdata=expand.grid(df_f$conc), interval="confidence")
              
              df_f$od <- pm[,1]
              df_f$pmin <- pm[,2]
              df_f$pmax <- pm[,3]
              
              f <- paste0("y==", UpperLimit , "+", "frac(", LowerLimit ,"-",UpperLimit,",",
                          "1+(","frac(","x",",",EC50,")" ,")^",Slope," )")
              
              rsd <- paste0('RSD ≈ ', round(as.numeric(RSD ), input$round) ) 
              
              p1 <- ggplot(data = df_f , aes(x = conc , y = od) ) +
                geom_line( color ='black' ) +
                geom_point( aes(x, y ) , shape= 1 ,data = df1 ,size=0.1) +
                geom_ribbon(data=df_f, aes(x= conc, y= od, ymin=pmin, ymax=pmax), alpha=0.2)+
                annotate("text", x = (max(df1$x)-min(df1$x))/2 , y = max(df1$y)*0.95, parse=T, 
                         label= f, size = 4)+
                annotate("text", x = (max(df1$x)-min(df1$x))/2 , y = max(df1$y)*0.85, 
                         label= rsd, size = 4)
              p1 <- p1 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p1 <- p1 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )+
                ggtitle("ELISA : 4pl_logistic") + xlab("x : Conc") + ylab("y : OD")
              
              # 反函数图
              df_f <- data.frame('od' = runif(1000, min = min(df1$y), max = max(df1$y) ) )
              df_f$conc <- myfun( y= df_f$od, a=LowerLimit , b= Slope, c=EC50, d=UpperLimit)
              
              p2 <- ggplot(data = df_f , aes(x = od , y = conc) ) +
                geom_line( color ='pink3' ) +
                geom_point( aes(x= y, y= x) , df1, shape= 1,size=0.1) +
                ggtitle("ELISA : 4pl_logistic") + xlab("OD") + ylab("Conc")
              p2 <- p2 + eval(parse(text = paste0("theme_",input$theme,"()")))
              p2 <- p2 + 
                theme(
                  axis.title = element_text(size = 25),
                  axis.text = element_text(size = 15), 
                )
              standard_plot <- p1 + p2
            }
            elisa <- list()
            elisa$df1  <- df1 
            elisa$df2  <- df2 
            elisa$p    <- standard_plot
            return(elisa)
          })
          
          # 3、分析情况展示
          output$results <- renderRHandsontable(
            
            rhandsontable( elisa()$df2 ) 
          )
          
          # 4、画图ELISA的标曲图像，并呈现在网页上
          output$plot <-  renderPlot({
            
            if ( is.null(elisa() )) { return() }
            return(elisa()$p)
            
          })
          
          # 依据函数计算结果，并设置可下载
          output$downloadData <- downloadHandler(
            
            filename = function() {
              paste("计算结果.csv")
            },
            content = function(file) {
              write.csv( elisa()$df2 , file , row.names = F, fileEncoding = 'GB18030') 
            } )
          
          # download figure  
          if(T){
            output$pdf <- downloadHandler(
              filename="elisa.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(elisa()$p)
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="elisa.png",
              content = function(file){
                ggsave(plot = elisa()$p,filename = file,width=input$w,height = input$h,units = 'in',dpi =input$ppi )
              }
            )
            output$jpeg <- downloadHandler(
              filename="elisa.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(elisa()$p)
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="elisa.tiff",
              content = function(file){
                ggsave(plot = elisa()$p,filename = file,width=input$w,height = input$h,units = 'in',dpi =input$ppi )
              }
            )
            
            output$rdata <- downloadHandler( 
              filename="elisa.rdata",
              content = function(file){
                p <- elisa()$p
                save(p,file = file)
              }
            )
          }
        } )
      })
      
      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        
        filename = function() {
          paste('elisa.csv')
        },
        content = function(file) {
          if(is.null(input$table) ){
            # 读取 ELISA 数据
            if(input$rep == T){ df1 <- readRDS('www/with.RDS') }
            else{ df1 <- readRDS('www/without.RDS') }
          }
          else{ df1  <- hot_to_r( input$table ) }
          write.csv( df1 , file , row.names = F, fileEncoding = 'GB18030') 
        } )  
    }
  ) # moduleServer
} # function(id)

