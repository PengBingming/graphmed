
df0 <- rbind( c("100000","中华人民共和国"),readRDS(('www/map.RDS'))[,1:2] )

myfun <- function(x){
  ifelse(x=="710000" | x=="100000",
         site <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",x,".json")
         ,
         site <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",x,"_full.json")
         )
  df1 <- read_sf(site)[,c('adcode','name','center','geometry')]
  df1$province <-  df0[which(df0$adcode==x),"name"]
  return(df1)
}

df <- lapply( df0[,1] , myfun)

df1 <- do.call(rbind, df )

df1 <- df1[,c(5,2,3,4)]
df1[1,2] <- "国界线"
saveRDS(df1,file = './www/map_province.RDS')

