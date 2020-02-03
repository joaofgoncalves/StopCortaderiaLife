
library(caret)
library(randomForest)
library(SegOptim)
library(stringr)
library(dplyr)
library(tidyr)

source("./RCODE/_INPUT_PARAMS.R")


TPR_Sensitivity <- function(x) x[2,2] / sum(x[2,])
TNR_Specificity <- function(x) x[1,1] / sum(x[1,])


## ------------------------------------------------------------------- #

nR <- 100

nFolds <- 10

negSampSizeMult <- 1


## ------------------------------------------------------------------- #

pb <- txtProgressBar(min=1, max=nFolds*nR, style=3)
z<-0
 
posTrainDF <- rstTrainDF %>% filter(percCov >= 30) %>% mutate(pr=1)
#rstTrainDF %>% filter(percCov >= 25) %>% nrow()
#rstTrainDF %>% filter(percCov == 0) %>% nrow()

negTrainDF <- rstTrainDF %>% filter(percCov == 0)
blrstTrainDF <- rbind(posTrainDF, negTrainDF)
blrstTrainDF <- blrstTrainDF %>% mutate(pr=as.factor(pr))



for(j in 1:nR){
  
  
  negTrainDF <- rstTrainDF %>% filter(percCov == 0) %>% 
    sample_n(size = negSampSizeMult*nrow(posTrainDF))
  
  blrstTrainDF <- rbind(posTrainDF, negTrainDF)
  blrstTrainDF <- blrstTrainDF %>% mutate(pr=as.factor(pr))
  
  yvar <- blrstTrainDF$pr
  
  folds <- createFolds(yvar, k = nFolds, list = TRUE, returnTrain = TRUE)
  
  perfMetrics <- as.data.frame(matrix(NA,nrow = length(folds), ncol = 10))
  colnames(perfMetrics)<-c("AUC","AUC.thresh",
                           "Kappa","Kappa.thresh",
                           "Accuracy.AUCthresh",
                           "TPR.AUCthresh",
                           "TNR.AUCthresh",
                           "Accuracy.KappaThresh",
                           "TPR.KappaThresh",
                           "TNR.KappaThresh")
  
  for(k in 1:length(folds)){
    z<-z+1
    blrstTrainDF.train <- blrstTrainDF[folds[[k]],]
    blrstTrainDF.test <- blrstTrainDF[-folds[[k]],]
    
    rf <- randomForest(x = blrstTrainDF.train %>% select(-layer, -ID, -percCov, -pr),
                       y =  blrstTrainDF.train %>% select(pr) %>% pull, ntree = 500)
    
    pred <- predict(rf, newdata=blrstTrainDF.test, type="prob")[,2]
    
    obs <- blrstTrainDF.test %>% select(pr) %>% pull
    obs <- as.integer(levels(obs)[obs])
    
    kappaVal <- kappaSingleClass(obs,pred)
    aucVal <-   aucSingleClass(obs, pred)
    
    predFact <- as.integer(pred > aucVal[2])
    acc1 <- evaluatePerformance(obs = obs, pred = predFact)
    
    predFact <- as.integer(pred > kappaVal$maxKappa[1,1])
    acc2 <- evaluatePerformance(obs = obs, pred = predFact)
    
    ## Performance metrics for the test set (using k-folds CV)
    ##
    perfMetrics[k,c("AUC","AUC.thresh")] <- aucVal
    perfMetrics[k,c("Kappa","Kappa.thresh")] <- as.numeric(kappaVal$maxKappa[,c(2,1)])

    perfMetrics[k,"Accuracy.AUCthresh"] <- acc1$Metrics$Accuracy[1]
    perfMetrics[k,"Accuracy.KappaThresh"] <- acc2$Metrics$Accuracy[1]
    
    perfMetrics[k,"TPR.AUCthresh"] <- TPR_Sensitivity(acc1$ConfusionMatrix)
    perfMetrics[k,"TNR.AUCthresh"] <- TNR_Specificity(acc1$ConfusionMatrix)
    
    perfMetrics[k,"TPR.KappaThresh"] <- TPR_Sensitivity(acc2$ConfusionMatrix)
    perfMetrics[k,"TNR.KappaThresh"] <- TNR_Specificity(acc2$ConfusionMatrix)
    
    ## Save randomForest object
    outFn <- paste(outFolderRFobjects,"/RF_Classifier_idx_",
                   str_pad(z,4,side="left",pad=0),".RData",sep="")
    saveRDS(rf,file = outFn)
    
    setTxtProgressBar(pb, z)
    
  }
  
  avgPerf <- perfMetrics %>% summarise_all(.funs = list(avg=mean))
  
  if(j==1){
    avgPerfMetrics <- avgPerf
    perfMetricsAll <- data.frame(round = j, perfMetrics)
  }else{
    avgPerfMetrics <- rbind(avgPerfMetrics,avgPerf)
    perfMetricsAll <- rbind(perfMetricsAll, data.frame(round = j, perfMetrics))
  }
}


perfMetricsAll<-perfMetricsAll %>% 
  mutate(idx=1:nrow(.)) %>% 
  select(ncol(.), 1:(ncol(.)-1))


avgPerfMetrics %>% 
  summarise_all(.funs = list(avg = mean))

avgPerfMetrics %>% 
  summarise_all(.funs = list(stderr = function(x) sd(x)/sqrt(length(x))))


perfMetricsAll %>% group_by(round) %>% 
  summarise_all(.funs = list(stderr = function(x) sd(x)/sqrt(length(x))))

perfMetricsAll %>% group_by(round) %>% 
  summarise_all(.funs = list(std=sd))

