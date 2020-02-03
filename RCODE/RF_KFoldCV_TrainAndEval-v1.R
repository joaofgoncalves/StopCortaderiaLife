

library(caret)
library(randomForest)
library(SegOptim)
library(stringr)
library(dplyr)
library(tidyr)

source("./RCODE/_INPUT_PARAMS.R")


TPR_Sensitivity <- function(x) x[2,2] / sum(x[2,])
TNR_Specificity <- function(x) x[1,1] / sum(x[1,])



posTrainDF   <- rstTrainDF %>% filter(percCov >= 30) %>% mutate(pr=1)
negTrainDF   <- rstTrainDF %>% filter(percCov == 0)
blrstTrainDF <- rbind(posTrainDF, negTrainDF)
blrstTrainDF <- blrstTrainDF %>% mutate(pr=as.factor(pr))

compIds <- complete.cases(blrstTrainDF %>% dplyr::select(-layer, -ID, -percCov, -pr))
blrstTrainDF <- blrstTrainDF[compIds, ]


nFolds <- 10

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

pb <- txtProgressBar(min=1, max=nFolds, style=3)


for(k in 1:length(folds)){

  # Train data subset
  blrstTrainDF.train <- blrstTrainDF[folds[[k]],]
  # Test data subset
  blrstTrainDF.test <- blrstTrainDF[-folds[[k]],] %>% 
    dplyr::select(-layer, -ID, -percCov)
  
  # Train random forest classifier
  rf <- randomForest(x = blrstTrainDF.train %>% dplyr::select(-layer, -ID, -percCov, -pr),
                     y =  blrstTrainDF.train %>% dplyr::select(pr) %>% pull, ntree = 500)
  
  # Predict for the test fraction
  pred <- predict(rf, newdata=blrstTrainDF.test, type="prob")[,2]
  
  obs <- blrstTrainDF.test %>% dplyr::select(pr) %>% pull
  obs <- as.integer(levels(obs)[obs])
  
  kappaVal <- kappaSingleClass(obs,pred)
  aucVal <-   aucSingleClass(obs, pred)
  
  predFact.AUC <- as.integer(pred > aucVal[2])
  acc1 <- evaluatePerformance(obs = obs, pred = predFact.AUC)
  
  predFact.kappa <- as.integer(pred > kappaVal$maxKappa[1,1])
  acc2 <- evaluatePerformance(obs = obs, pred = predFact.kappa)
  
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
  outFn <- paste(outFolderRFobjects,"/RF_Classifier_KFoldCV_idx_",
                 str_pad(k,2,side="left",pad=0),".RData",sep="")
  saveRDS(rf,file = outFn)
  
  setTxtProgressBar(pb, k)
  
}


avgPerf <- perfMetrics %>% summarise_all(.funs = list(avg=mean))

print(avgPerf)

write.csv(perfMetrics,"./OUT/PIXCLASSIFY/PerformanceMetrics_RF_10FCV-v1.csv",row.names = FALSE)



