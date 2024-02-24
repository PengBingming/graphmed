
# 安装并加载所需的R包
#install.packages("survivalROC")
library(survivalROC)

# 查看内置数据集
data(mayo)
head(mayo)

# 计算数据的行数
nobs <- NROW(mayo)
nobs

# 自定义阈值
cutoff <- 365

# 使用MAYOSCORE 4作为marker, 并用NNE（Nearest Neighbor Estimation）法计算ROC值
Mayo4.1 = survivalROC(Stime=mayo$time,  
                      status=mayo$censor,      
                      marker = mayo$mayoscore4,     
                      predict.time = cutoff,
                      span = 0.25*nobs^(-0.20) )
Mayo4.1

# 绘制ROC曲线
plot(Mayo4.1$FP, Mayo4.1$TP, type="l", 
     xlim=c(0,1), ylim=c(0,1), col="red",  
     xlab=paste( "FP", "\n", "AUC = ",round(Mayo4.1$AUC,3)), 
     ylab="TP",main="Mayoscore 4, Method = NNE \n  Year = 1")
# 添加对角线
abline(0,1)

# 使用KM（Kaplan-Meier）法计算ROC值
## MAYOSCORE 4, METHOD = KM
Mayo4.2= survivalROC(Stime=mayo$time,  
                     status=mayo$censor,      
                     marker = mayo$mayoscore4,     
                     predict.time =  cutoff, method="KM")
Mayo4.2

plot(Mayo4.2$FP, Mayo4.2$TP, type="l", 
     xlim=c(0,1), ylim=c(0,1), col="blue",
     xlab=paste( "FP", "\n", "AUC = ",round(Mayo4.2$AUC,3)), 
     ylab="TP",main="Mayoscore 4, Method = KM \n Year = 1")
abline(0,1,lty=2,col="gray")

# 将两种方法的结果绘制到同一个图里
## 绘制NNE法计算的ROC曲线
plot(Mayo4.1$FP, Mayo4.1$TP,
     type="l",col="red",
     xlim=c(0,1), ylim=c(0,1),   
     xlab="FP", 
     ylab="TP",
     main="Time dependent ROC")
# 添加对角线
abline(0,1,col="gray",lty=2)

## 添加KM法计算的ROC曲线
lines(Mayo4.2$FP, Mayo4.2$TP, 
      type="l",col="blue",
      xlim=c(0,1), ylim=c(0,1))
# 添加图例
legend("bottomright",legend = c(paste("AUC of NNE =",round(Mayo4.1$AUC,3)),
                                paste("AUC of KM =",round(Mayo4.2$AUC,3))),
       col=c("red","blue"),
       lty= 1 ,lwd= 2)
