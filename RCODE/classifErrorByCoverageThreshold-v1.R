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


threshValues <- seq(from=0.0001,to=80, by=2.5)

nRounds <- 30
NR <- nRounds*length(threshValues)

res <- as.data.frame(matrix(NA,nrow=NR,ncol=6))
colnames(res) <- c("thresh","round","nsize","TNErr","TPErr","Acc")

pb<-txtProgressBar(min=1,max=NR, style=3)
k<-0
for(i in 1:length(threshValues)){
  
  for(j in 1:nRounds){
    k<-k+1
    
    posTrainDF <- rstTrainDF %>% 
      filter(percCov >= threshValues[i]) %>% 
      mutate(pr=1)
    
    negTrainDF <- rstTrainDF %>% 
      filter(percCov == 0) %>% 
      sample_n(size = nrow(posTrainDF)) %>% 
      mutate(pr=0)
    
    blrstTrainDF <- rbind(posTrainDF, negTrainDF) %>% 
      mutate(pr=as.factor(pr)) %>% 
      as.data.frame
    
    rf4 <- randomForest(x = blrstTrainDF %>% select(-layer, -ID, -percCov, -pr),
                        y =  blrstTrainDF %>% select(pr) %>% pull)
    
    res[k,"thresh"] <- threshValues[i]
    res[k,"round"] <- j
    res[k,"nsize"] <- nrow(blrstTrainDF)
    
    nt <- sum(rf4$confusion[1:2,1:2])
    acc <- sum(diag(rf4$confusion)) / nt
    res[k,"TNErr"] <- 1-rf4$confusion[1,3]
    res[k,"TPErr"] <- 1-rf4$confusion[2,3]
    res[k,"Acc"] <- acc
    
    setTxtProgressBar(pb, k)
  }
}


res <- res %>% mutate(threshValue = as.factor(round(thresh,2)))


g1<-ggplot(res %>% filter(thresh<=55),aes(x=as.factor(thresh), y=Acc)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("Accuracy") + 
  ylim(0.725,0.95)


g2<-ggplot(res %>% filter(thresh<=55),aes(x=as.factor(thresh), y=TPErr)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("True Positive Rate") + 
  ylim(0.725,0.95)


g3<-ggplot(res %>% filter(thresh<=55),aes(x=as.factor(thresh), y=TNErr)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage Threshold") + 
  ylab("True Negative Rate")  + 
  ylim(0.725,0.95)


gg <- g1+g2+g3 + 
  plot_annotation(
    title = "Accuracy vs. % Coverage of Cortaderia in Sentinel-2 pixels",
    subtitle="Accuracy values distribution for 30 random forest classifiers")

ggsave("./OUT/Accuracy_vs_CovThresh-v1.png",width = 13,height = 6)



