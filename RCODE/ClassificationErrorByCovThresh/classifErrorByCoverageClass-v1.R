

library(raster)
library(fasterize)
library(dplyr)
library(tidyr)
library(sf)
library(units)
library(randomForest)
library(randomForestSRC)
library(BBmisc)
library(ggplot2)
library(patchwork)

rstTrainDF <- readRDS(file="./OUT/LC_rstTrainDF-v1.RData")


# lowInt <- seq(0,200/3,by=100/3)
# highInt <- seq(100/3,100,by=100/3)

lowInt <- seq(0, 60, by = 5)
highInt <- c(seq(5, 60, by = 5), 100)
cv <- round(lowInt + ((highInt-lowInt)/2), 2)

print(length(lowInt))
print(length(highInt))


nRounds <- 100
NR <- nRounds*length(lowInt)

res <- as.data.frame(matrix(NA,nrow=NR,ncol=6))
colnames(res) <- c("thresh","round","nsize","TNErr","TPErr","Acc")

pb<-txtProgressBar(min=1,max=NR, style=3)
k<-0

for(i in 1:length(lowInt)){
  
  for(j in 1:nRounds){
    k<-k+1
  
    posDF <- rstTrainDF %>% 
      na.omit() %>% 
      filter(percCov > lowInt[i], percCov <= highInt[i]) %>% 
      sample_n(size = 20) %>% 
      mutate(pCS = 1)
    
    negDF <- rstTrainDF %>% 
      na.omit() %>% 
      filter(percCov == 0) %>% 
      sample_n(size = 20) %>% 
      mutate(pCS = 0)
    
    tmpTrainDF <- bind_rows(posDF,negDF) %>% 
      mutate(pCS = as.factor(pCS)) %>% 
      as.data.frame
    
    rf <- randomForest(x = tmpTrainDF %>% select(-layer, -ID, -percCov, -pCS),
                        y =  tmpTrainDF %>% select(pCS) %>% pull)
    
    res[k,"thresh"] <- cv[i]
    res[k,"round"] <- j
    res[k,"nsize"] <- nrow(tmpTrainDF)
    
    nt <- sum(rf$confusion[1:2,1:2])
    acc <- sum(diag(rf$confusion)) / nt
    
    res[k,"TNErr"] <- 1 - rf$confusion[1,3]
    res[k,"TPErr"] <- 1 - rf$confusion[2,3]
    res[k,"Acc"] <- acc
    
    setTxtProgressBar(pb, k)
    
  }
}



#tmpRes <- res %>% group_by(thresh) %>% summarise(medAcc=median(Acc))

g1 <- ggplot(res,aes(x=as.factor(thresh), y=Acc)) + 
  geom_boxplot(fill="orange2",color="black", alpha=0.4) + 
  #geom_line(data = tmpRes, mapping = aes(x=thresh, y=medAcc)) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=Acc,group=1),
               fun=median,geom="line",size=1,color="blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=Acc,group=1),
               fun=median,geom="point",size=2.5,color="brown3") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("Accuracy") 

plot(g1)

g2 <- ggplot(res,aes(x=as.factor(thresh), y=TPErr)) + 
  geom_boxplot(fill="orange2",color="black", alpha=0.4) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=TPErr,group=1),
               fun=median,geom="line",size=1,color="blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=TPErr,group=1),
               fun=median,geom="point",size=2.5,color="brown3") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("True Positive Rate") 

plot(g2)

g3 <- ggplot(res,aes(x=as.factor(thresh), y=TNErr)) + 
  geom_boxplot(fill="orange2",color="black", alpha=0.4) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=TNErr,group=1),
               fun=median,geom="line",size=1,color="blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(thresh),y=TNErr,group=1),
               fun=median,geom="point",size=2.5,color="brown3") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("True Negative Rate") 

plot(g3)

gg <- g1+g2+g3 + 
  plot_annotation(
    title = "Accuracy vs. % coverage of Cortaderia in Sentinel-2 pixels",
    subtitle="Accuracy values distribution for 100 random forest classifiers (with n=40)")

ggsave(filename = "./OUT/AccVsCoverageClass-v1.png", plot = gg, width = 14, height = 5)
