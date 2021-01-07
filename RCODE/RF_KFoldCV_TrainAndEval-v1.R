

library(caret)
library(randomForest)
library(SegOptim)
library(stringr)
library(dplyr)
library(tidyr)
library(imbalance)
library(ranger)
library(xgboost)


source("./RCODE/_INPUT_PARAMS.R")
#source("./RCODE/_INPUT_PARAMS_L8.R")

source("./RCODE/_AUX_FUNS.R")

TPR_Sensitivity <- function(x) x[2,2] / sum(x[2,])
TNR_Specificity <- function(x) x[1,1] / sum(x[1,])

rstTrainDF <- readRDS("./OUT/LC_rstTrainDF-v4-20201223_205228.RData")
#rstTrainDF <- readRDS("./OUT/LC_Landsat8_rstTrainDF-v1-20201230_195323.RData")

print(colnames(rstTrainDF))

## --------------------------------------------------------------------------- ##

# MWMOTE options
propMWMOTE    <- 1

# Random forest options
mtry          <- 5
num.trees     <- 1000
num.threads   <- 6
probability   <- TRUE
class.weights <- NULL #c(1,10)
verbose       <- FALSE

# Cross-validation options
nFolds        <- 10

# version
vn <- "v1"


## --------------------------------------------------------------------------- ##

#posTrainDF   <- rstTrainDF %>% filter(percCov >= 30) %>% mutate(pr=1)

posTrainDF   <- rstTrainDF %>% filter(percCov >= threshTrainPos) %>% mutate(pr=1)
negTrainDF   <- rstTrainDF %>% filter(percCov == 0)
blrstTrainDF <- rbind(posTrainDF, negTrainDF)
blrstTrainDF <- blrstTrainDF %>% mutate(pr=as.factor(pr))

compIds <- complete.cases(blrstTrainDF %>% 
                            dplyr::select(-layer, -ID, -percCov, -pr, 
                                          -MOSAIC_ID, -DATA_TYPE, -SCN_INDEX, -SCN_CODE))
blrstTrainDF <- blrstTrainDF[compIds, ]

blrstTrainDF_init <- blrstTrainDF

if((propMWMOTE > 0) & (propMWMOTE <= 1)){
  
  nCasesMWMOTE <- round(blrstTrainDF %>% filter(pr == 0) %>% nrow * propMWMOTE) - 
    (blrstTrainDF %>% filter(pr == 1) %>% nrow)
  print(nCasesMWMOTE)
  
  
  (blrstTrainDF %>% filter(DATA_TYPE == "FIELD") %>% nrow) / 
    (blrstTrainDF %>% filter(DATA_TYPE == "PSEUDO_ABS") %>% nrow)
  
  wts <- vector(mode = "numeric", length = nrow(blrstTrainDF))
  
  wts[blrstTrainDF$DATA_TYPE == "FIELD"] <- 20
  wts[blrstTrainDF$DATA_TYPE == "PSEUDO_ABS"] <- 5
  wts <- c(wts, rep(1, nCasesMWMOTE))
  
  blrstTrainDF_mwmote <- mwmote(blrstTrainDF %>% select(-layer, -percCov, -ID, -MOSAIC_ID, 
                                                        -DATA_TYPE, -SCN_INDEX, -SCN_CODE), 
                                numInstances = nCasesMWMOTE, 
                                classAttr = "pr")
  
  
  blrstTrainDF <- bind_rows(blrstTrainDF %>% select(-layer, -percCov, -ID, -MOSAIC_ID, 
                                                    -SCN_INDEX, -SCN_CODE),
                            data.frame(DATA_TYPE="MWMOTE",blrstTrainDF_mwmote))
}else if(propMWMOTE == 0){
  
  wts <- vector(mode = "numeric", length = nrow(blrstTrainDF))
  
  wts[blrstTrainDF$DATA_TYPE == "FIELD"] <- 20
  wts[blrstTrainDF$DATA_TYPE == "PSEUDO_ABS"] <- 5

}else{
  stop("Invalid value for propMWMOTE!!")
}



nrow(blrstTrainDF)

tb <- table(blrstTrainDF$pr)
print(tb / sum(tb))



## ------------------------------------------------------------------------ ##




folds <- createFolds(blrstTrainDF$pr, k = nFolds, list = TRUE, returnTrain = TRUE)

perfMatrix <- as.data.frame(matrix(NA,nrow = nFolds+1, ncol = 15))
colnames(perfMatrix)<-c( "AUC","AUC.thresh",
                         "Kappa","Kappa.thresh",
                         "F1","F1.thresh",
                         
                         "Accuracy.AUCthresh",
                         "TPR.AUCthresh",
                         "TNR.AUCthresh",
                         
                         "Accuracy.KappaThresh",
                         "TPR.KappaThresh",
                         "TNR.KappaThresh",
                         
                         "Accuracy.F1Thresh",
                         "TPR.F1Thresh",
                         "TNR.F1Thresh")

perfMetrics <- list()
perfMetrics[["ALL"]] <- perfMatrix
perfMetrics[["FIELD"]] <- perfMatrix

pb <- txtProgressBar(min=1, max=nFolds, style=3)
cn <- colnames(blrstTrainDF)[-c(1,ncol(blrstTrainDF))]
form <- as.formula(paste("pr~",paste(cn,collapse="+")))


for(k in 1:length(folds)){

  kIdx <- folds[[k]]
  
  # Train data subset
  blrstTrainDF.train <- blrstTrainDF[kIdx,]
  # Test data subset
  blrstTrainDF.test <- blrstTrainDF[-kIdx,] #%>% 
    #dplyr::select(-layer, -ID, -percCov)

  wtsTrain <- wts[kIdx]
  

  rf <- ranger(
    formula       = form,
    data          = blrstTrainDF.train,
    mtry          = mtry,
    num.trees     = num.trees,
    case.weights  = wtsTrain,
    num.threads   = num.threads,
    probability   = probability,
    class.weights = class.weights,
    verbose       = verbose)
  
  
  # Predict for the test fraction

  pred <- predict(rf, data=blrstTrainDF.test, type="response")$predictions[,2] # ranger

  obs <- blrstTrainDF.test %>% dplyr::select(pr) %>% pull
  obs <- as.integer(levels(obs)[obs])

  for(dataSubset in c("ALL","FIELD")){
    
    if(dataSubset == "FIELD"){
      pred <- pred[blrstTrainDF.test$DATA_TYPE=="FIELD"]
      obs <- obs[blrstTrainDF.test$DATA_TYPE=="FIELD"]
    }
    
    kappaObj <- kappaSingleClass(obs,pred)
    aucObj   <-   aucSingleClass(obs, pred)
    f1Obj    <-   F1max_SingleClass(obs, pred)
    
    predFact.AUC <- as.integer(pred > aucObj[2])
    acc1 <- evaluatePerformance(obs = obs, pred = predFact.AUC)
    
    predFact.kappa <- as.integer(pred > kappaObj$maxKappa[1,1])
    acc2 <- evaluatePerformance(obs = obs, pred = predFact.kappa)
    
    predFact.F1 <- as.integer(pred > f1Obj$maxF1[1,1])
    acc3 <- evaluatePerformance(obs = obs, pred = predFact.F1)
    
    ## Performance metrics for the test set (using k-folds CV)
    ##
    perfMetrics[[dataSubset]][k,c("AUC","AUC.thresh")] <- aucObj
    perfMetrics[[dataSubset]][k,c("Kappa","Kappa.thresh")] <- as.numeric(kappaObj$maxKappa[,c(2,1)])
    perfMetrics[[dataSubset]][k,c("F1","F1.thresh")] <- as.numeric(f1Obj$maxF1[,c(2,1)])
    
    perfMetrics[[dataSubset]][k,"Accuracy.AUCthresh"] <- acc1$Metrics$Accuracy[1]
    perfMetrics[[dataSubset]][k,"Accuracy.KappaThresh"] <- acc2$Metrics$Accuracy[1]
    perfMetrics[[dataSubset]][k,"Accuracy.F1Thresh"] <- acc3$Metrics$Accuracy[1]
    
    perfMetrics[[dataSubset]][k,"TPR.AUCthresh"] <- TPR_Sensitivity(acc1$ConfusionMatrix)
    perfMetrics[[dataSubset]][k,"TNR.AUCthresh"] <- TNR_Specificity(acc1$ConfusionMatrix)
    
    perfMetrics[[dataSubset]][k,"TPR.KappaThresh"] <- TPR_Sensitivity(acc2$ConfusionMatrix)
    perfMetrics[[dataSubset]][k,"TNR.KappaThresh"] <- TNR_Specificity(acc2$ConfusionMatrix)
    
    perfMetrics[[dataSubset]][k,"TPR.F1Thresh"] <- TPR_Sensitivity(acc3$ConfusionMatrix)
    perfMetrics[[dataSubset]][k,"TNR.F1Thresh"] <- TNR_Specificity(acc3$ConfusionMatrix)

  }
  
  ## Save randomForest object
  outFn <- paste(outFolderRFobjects,"/RF_rangerObj_10FCV_MWMOTE_caseWts_NoClassWts_idx_",
                 str_pad(k,2,side="left",pad=0),"_",vn,".RData",sep="")
  saveRDS(rf,file = outFn)
  
  # Final round with full data i.e. no train/test partition
  
  if(k == nFolds){
    
    rf <- ranger(
      formula       = form,
      data          = blrstTrainDF,
      mtry          = mtry,
      num.trees     = num.trees,
      case.weights  = wts,
      num.threads   = num.threads,
      probability   = probability,
      class.weights = class.weights,
      verbose       = verbose #,
      #importance    = "impurity_corrected"
      
      )
    
    rf_imp <- ranger(
      formula       = form,
      data          = blrstTrainDF,
      mtry          = mtry,
      num.trees     = num.trees,
      case.weights  = wts,
      num.threads   = num.threads,
      probability   = probability,
      class.weights = class.weights,
      verbose       = verbose,
      importance    = "impurity_corrected"
    )
    
    outFn <- paste(outFolderRFobjects,"/RF_rangerObj_MWMOTE_caseWts_NoClassWts_FULL_",vn,".RData",sep="")
    saveRDS(rf, file = outFn)
    
    
    ## EVALUATE FULL ROUND
    
    for(dataSubset in c("ALL","FIELD")){
      
      #pred <- predict(rf, data = blrstTrainDF, type="response")$predictions[,2] # ranger
      
      pred <- rf$predictions[,2]
      
      obs <- blrstTrainDF %>% dplyr::select(pr) %>% pull
      obs <- as.integer(levels(obs)[obs])
      
      
      if(dataSubset == "FIELD"){
        pred <- pred[blrstTrainDF$DATA_TYPE=="FIELD"]
        obs <- obs[blrstTrainDF$DATA_TYPE=="FIELD"]
      }
    

      kappaObj <- kappaSingleClass(obs,pred)
      aucObj   <-   aucSingleClass(obs, pred)
      f1Obj    <-   F1max_SingleClass(obs, pred)
      
      predFact.AUC <- as.integer(pred > aucObj[2])
      acc1 <- evaluatePerformance(obs = obs, pred = predFact.AUC)
      
      predFact.kappa <- as.integer(pred > kappaObj$maxKappa[1,1])
      acc2 <- evaluatePerformance(obs = obs, pred = predFact.kappa)
      
      predFact.F1 <- as.integer(pred > f1Obj$maxF1[1,1])
      acc3 <- evaluatePerformance(obs = obs, pred = predFact.F1)
      
      ## Performance metrics for the test set (using k-folds CV)
      ##
      perfMetrics[[dataSubset]][k+1,c("AUC","AUC.thresh")] <- aucObj
      perfMetrics[[dataSubset]][k+1,c("Kappa","Kappa.thresh")] <- as.numeric(kappaObj$maxKappa[,c(2,1)])
      perfMetrics[[dataSubset]][k+1,c("F1","F1.thresh")] <- as.numeric(f1Obj$maxF1[,c(2,1)])
      
      perfMetrics[[dataSubset]][k+1,"Accuracy.AUCthresh"] <- acc1$Metrics$Accuracy[1]
      perfMetrics[[dataSubset]][k+1,"Accuracy.KappaThresh"] <- acc2$Metrics$Accuracy[1]
      perfMetrics[[dataSubset]][k+1,"Accuracy.F1Thresh"] <- acc3$Metrics$Accuracy[1]
      
      perfMetrics[[dataSubset]][k+1,"TPR.AUCthresh"] <- TPR_Sensitivity(acc1$ConfusionMatrix)
      perfMetrics[[dataSubset]][k+1,"TNR.AUCthresh"] <- TNR_Specificity(acc1$ConfusionMatrix)
      
      perfMetrics[[dataSubset]][k+1,"TPR.KappaThresh"] <- TPR_Sensitivity(acc2$ConfusionMatrix)
      perfMetrics[[dataSubset]][k+1,"TNR.KappaThresh"] <- TNR_Specificity(acc2$ConfusionMatrix)
      
      perfMetrics[[dataSubset]][k+1,"TPR.F1Thresh"] <- TPR_Sensitivity(acc3$ConfusionMatrix)
      perfMetrics[[dataSubset]][k+1,"TNR.F1Thresh"] <- TNR_Specificity(acc3$ConfusionMatrix)
    }
  }
  
  setTxtProgressBar(pb, k)
  
}

#varImpPlot(rf)

avgPerf_ALL <- perfMetrics[[1]][-11,] %>% summarise_all(.funs = list(avg=mean))
print(avgPerf_ALL)
avgPerf_FIELD <- perfMetrics[[2]][-11,] %>% summarise_all(.funs = list(avg=mean))
print(avgPerf_FIELD)

imp <- importance(rf_imp)
vimp <- data.frame(vnames = names(imp), imp = imp) %>% arrange(desc(imp))


# write.csv(perfMetrics[["ALL"]],
#           "./OUT/PIXCLASSIFY_2/CLF_PERF_METRICS/PerformanceMetrics_ALL-data_RFranger_10FCV_MWMOTE_caseWts_NoClassWts-v6.csv",
#           row.names = FALSE)
# write.csv(perfMetrics[["FIELD"]],
#           "./OUT/PIXCLASSIFY_2/CLF_PERF_METRICS/PerformanceMetrics_FIELD-data_RFranger_10FCV_MWMOTE_caseWts_NoClassWts-v6.csv",
#           row.names = FALSE)
# write.csv(vimp,
#           "./OUT/PIXCLASSIFY_2/CLF_PERF_METRICS/VarImp_RFranger_10FCV_MWMOTE_caseWts_NoClassWts-v6.csv",
#           row.names = FALSE)
write.csv(perfMetrics[["ALL"]],
          paste(outFolder,"/CLF_PERF_METRICS/PerfMetrics_ALL-data_RFranger_10FCV_MWMOTE_caseWts_NoClassWts-",vn,".csv",sep=""),
          row.names = FALSE)
write.csv(perfMetrics[["FIELD"]],
          paste(outFolder,"/CLF_PERF_METRICS/PerfMetrics_FIELD-data_RFranger_10FCV_MWMOTE_caseWts_NoClassWts-",vn,".csv",sep=""),
          row.names = FALSE)
write.csv(vimp,
          paste(outFolder,"/CLF_PERF_METRICS/VarImp_RFranger_FULL_MWMOTE_caseWts_NoClassWts-",vn,".csv",sep=""),
          row.names = FALSE)





