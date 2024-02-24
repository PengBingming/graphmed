
# install.packages("pROC")
library(pROC)
library(ggplot2)
library(ggsci)
library(tidyverse)
# 查看内置数据集
data("aSAH")
head(aSAH)

# This is equivalent to using roc.formula:
roc.list <- roc(outcome ~ s100b+ndka+wfns, 
                ci=TRUE ,aur=TRUE, percent=F,
                levels=c("Good","Poor"),direction="<",
                data = aSAH)


# coords(roc.list[[1]], "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))


# text <- paste0("roc(","outcome","~","s100b","+","ndka","+","wfns",", data = aSAH)")
# 
# roc.list <- eval(parse(text = text  ))
# 分面展示
# OR faceting
data.labels <- do.call(rbind, lapply(roc.list, ci) )
data.labels <- round(data.labels,3)
data.labels <- data.frame(data.labels)
colnames(data.labels) <- c("min","auc","max")
data.labels$name <- rownames(data.labels)


best.point <- lapply(roc.list, function(x){coords(x, "best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))})
best.point <- do.call(rbind, best.point)
best.point <- round(best.point,3)

best.point <- data.frame(best.point)
colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
best.point$name <- rownames(best.point)

ggroc(roc.list, legacy.axes = T,
      aes=c("linetype","color") ,
      show.legend= F) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),show.legend= F)+
  facet_grid(.~name) +
  theme_bw() +
  labs(color="good",linetype='good') +
  scale_color_aaas()+
  geom_text(data.labels, hjust = 0.5,show.legend= F,
            mapping= aes(0.6, 0.3, 
              label =paste("AUC=",auc,"\n","95% CI: ",min,"~",max)) )+
  geom_point(data = best.point,show.legend = F,
             mapping = aes(x = `1-specificity`,y = sensitivity) )+
  geom_text(data = best.point,show.legend = F,
             mapping = aes(x = `1-specificity`+0.1,y = sensitivity,
                    label=paste(threshold,"\n","(",sensitivity,",",`1-specificity`,")")) )+
  ggtitle("ROC")

# one var.
roc <- roc(outcome ~ s100b, 
                ci=TRUE ,aur=TRUE, percent=F,
                levels=c("Good","Poor"),direction="<",
                data = aSAH)

data.labels <- as.data.frame(t(data.frame(ci(roc)) ))
data.labels <- round(data.labels ,3)
colnames(data.labels) <- c("min","auc","max")
data.labels$name <- "s100b"

data.labels <- as.data.frame(t(data.frame(ci(roc)) ))
data.labels <- round(data.labels ,3)
colnames(data.labels) <- c("min","auc","max")
data.labels$name <- "s100b"

best.point <- coords(roc, "best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))
best.point <- round(best.point,3)
best.point <- data.frame(best.point)
colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
best.point$name <- "s100b"

ggroc(roc, legacy.axes = T,
      show.legend= F) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),show.legend= F)+
  theme_bw() +
  labs(color="good",linetype='good') +
  scale_color_aaas()+
  geom_text(data.labels, hjust = 1,show.legend= F,
            mapping= aes(0.75, 0.4, 
                         label =paste("AUC=",auc,"\n","95% CI: ",min,"~",max)) )+
  geom_point(data = best.point,show.legend = F,
             mapping = aes(x = `1-specificity`,y = sensitivity) )+
  geom_text(data = best.point,show.legend = F, hjust = 0.5,
            mapping = aes(x = `1-specificity`+0.01,y = sensitivity+0.05,
                          label=paste(threshold,"(",sensitivity,",",`1-specificity`,")")) )+
  ggtitle("ROC")


model_1<-glm(outcome~s100b+ndka,data = aSAH, family = binomial(link ="logit"))
summary(model_1)
fitted.prob<-predict(model_1, newdata = aSAH, type = "response")
aSAH$pred<-model_1$fitted.values
roc_multivar_1<-roc(aSAH$outcome,aSAH[,"pred"])
plot.roc(roc_multivar_1,col="red",legacy.axes = T,print.auc = T,ci =!is.null(roc_multivar_1$ci )) 


ggroc(roc_multivar_1, legacy.axes = T,
      show.legend= F) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),show.legend= F)+
  theme_bw()


