

library(dplyr)
library(caret)
#library(imbalance)
library(parallel)
library(doParallel)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

fl <- list.files("C:/MyFiles/R-dev/StopCortaderiaLife/DATA/TABLES/ReflDataBySensor", pattern=".csv$",
                 full.names = TRUE)

fl_CIBIO <- fl[1:13]
fl_IHCant <- fl[14:26]

tb_CIBIO <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesField_Master-v2.csv")
tb_IHCant <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesIHCantb_Master-v2.csv")

trainObjs <- list()

for(i in 1:13){
  
  # Read average data from speclibs
  DF1 <- read.csv(fl_CIBIO[i])
  DF2 <- read.csv(fl_IHCant[i])
  NC1 = ncol(DF1)
  NC2 = ncol(DF2)
  vnames = colnames(DF1)
  
  # Check if the number of columns is the same
  if(NC1 != NC2){
    cat("Found differences for the following tables:\n->",
        basename(fl_CIBIO[i]),"\n->",basename(fl_IHCant[i]),"\n\n")
    #next
    
    vnames <- base::intersect(colnames(DF1), colnames(DF2)) 
    DF1 <- DF1[,vnames]
    DF2 <- DF2[,vnames]
  }
  
  # Assemble data
  trainDF <-
  rbind(data.frame(Class = tb_CIBIO$Binary, DF1),
        data.frame(Class = tb_IHCant$Binary, DF2)
  )
  trainDF[,"Class"] <- as.factor(trainDF[,"Class"])
  
  ctrlObj <- trainControl(method = "LGOCV", 
                          number = 50, 
                          p = 0.50, 
                          verboseIter = TRUE)

  trainObj <- train(trainDF[,-1], 
                    y = trainDF[,1], 
                    # method = "rf",       # Random Forest
                    # method = "C5.0Tree", # Single C5.0 Tree
                    # method = "Linda",    # Robust Linear Discriminant Analysis
                    # method = "qda",      # Quadratic Discriminant Analysis
                    # method = "lda",
                    # method = "kknn",
                    # method = "gam",         # Generalized Additive Model using Splines !!SLOW!!
                    # NOT WORKING: method="fda",
                    # method = "xgbLinear",   # eXtreme Gradient Boosting !!SLOW!!
                    # method = "adaboost",    # AdaBoost Classification Trees
                    # NOT WORKING: method = "bagFDA",
                    # NOT WORKING: method = "bagEarth",
                    # NOT WORKING method = "bayesglm", # Bayesian Generalized Linear Model
                    # NOT WORKING: method = "binda",
                    # NOT WORKING: method = "blackboost",
                    # NOT WORKING: method = "bstTree",
                    # NOT WORKING: method = "randomGLM",
                    # NOT WORKING: method = "SLAVE",
                    # method = "gaussprLinear", # !!!VERY SLOW!!!
                    # NOT WORKING: method = "glm",
                    # NOT WORKING: method = "glmnet",
                    # NOT WORKING: method = "lvq",
                    # method = "lssvmRadial",
                    # method = "avNNet", # Model Averaged Neural Network !! Unstable??
                    # NOT WORKING: method = "gcvEarth",
                    method = "snn",
                    trControl = ctrlObj, 
                    allowParallel = TRUE)
  
  tmpPerfData <- data.frame(
    Sensor = gsub("CIBIOdata_|.csv","",basename(fl_CIBIO[i])),
    NC1 = NC1,
    NC2 = NC2,
    mxKappa = max(trainObj$results$Kappa),
    mxAccuracy = max(trainObj$results$Accuracy),
    varNames = paste(vnames,collapse = " | "),
             stringsAsFactors = FALSE)
  
  if(i==1){
    perfScores <- tmpPerfData
  }else{
    perfScores <- rbind(perfScores, tmpPerfData)
  }
  
  trainObjs[[i]] <- trainObj
  
  cat("\n\n-------------------------------------------------------------------------\n")
  cat("->",basename(fl_CIBIO[i]),"\n->",basename(fl_IHCant[i]),"\n\n")
  print(trainObj)
  cat("-------------------------------------------------------------------------\n\n")
  
}

arrange(perfScores,desc(mxKappa)) %>% 
  mutate(mxKappa=round(mxKappa,2), mxAccuracy=round(mxAccuracy,2))


