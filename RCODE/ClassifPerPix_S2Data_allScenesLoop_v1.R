
library(raster)
#library(randomForest)
library(ranger)
library(crayon)
library(BBmisc)
library(parallel)
library(doParallel)
library(foreach)
library(magrittr)
library(dplyr)


# if(!exists("cl")){
#   cl <- parallel::makeCluster(4)
#   doParallel::registerDoParallel(cl)
# }

setwd("C:/MyFiles/R-dev/StopCortaderiaLife")
#source("./RCODE/_INPUT_PARAMS.R")
source("./RCODE/_INPUT_PARAMS_L8.R")

#rf <- readRDS("./OUT/PIXCLASSIFY_2/RFobjects/RF_Classifier_KFoldCV_idx_03.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/RFobjects/RF_Classifier_KFoldCV_mwmote_idx_01.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/RFobjects/v3_MWMOTE_wts/RF_Classifier_KFoldCV_mwmote_wts_FULL.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/RFobjects/XGBoost_Classifier_KFoldCV_mwmote_wts_idx_07.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/CLF_OBJECTS/RF_Classifier_mwmote_wts_FULL_v4.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/CLF_OBJECTS/RF_ranger_Classifier_mwmote_wts_FULL_v5.RData")
#rf <- readRDS("./OUT/PIXCLASSIFY_2/CLF_OBJECTS/RF_ranger_Classifier_MWMOTE_caseWts_NoClassWts_FULL_v6.RData")
rf <- readRDS("./OUT/PIXCLASSIFY_L8/CLF_OBJECTS/RF_rangerObj_MWMOTE_caseWts_NoClassWts_FULL_v1.RData")

pkgs <- c("raster","ranger","crayon","BBmisc",
          "utils", "dplyr", "magrittr")

rowsPerChunk <- 300

# version
vn <- "2013_L8_v1"
sceneList <- sceneList_2013

#vn <- "2018_L8_v1"


## ------------------------------------------------------------------------- ##


#foreach(scnIdx = 1:length(sceneList), .packages = pkgs) %dopar% {
for(scnIdx in 1:length(sceneList)){
  
  # setwd("C:/MyFiles/R-dev/StopCortaderiaLife")
  # source("./RCODE/_INPUT_PARAMS.R")
            
  cat(green("\n\nProcessing S2 scene:",scnCodes[scnIdx],".....\n\n"))
  
  if(checkL8ExtentDifferences){
    
    cat(red("\n_ Checking Landsat-8 image data differences......."))
    
    s1 <- raster::stack(sceneList[[scnIdx]][1:10])
    s2 <- raster::stack(sceneList[[scnIdx]][11:20])
    
    compRst <- compareRaster(s1, s2, stopiffalse=FALSE, 
                             showwarning=FALSE)
    
    if(!compRst){
      s1 <- crop(s1, s2)
      s2 <- crop(s2, s1)
    }
    
    s2imgScene <- raster::stack(s1, s2)
    
    cat(red("done!\n\n"))
    
  }else{
    
    # Load and stack each S2 scene
    s2imgScene <- raster::stack(sceneList[[scnIdx]])
    
  }
  
  # Attribute names for each layer in the scene stack
  names(s2imgScene) <- sceneBandNames[[scnIdx]]
  
  cks <- chunk(1:nrow(s2imgScene), rowsPerChunk)
  out <- vector(mode="numeric", length=ncell(s2imgScene))
  nc  <- ncol(s2imgScene)
  pb  <- txtProgressBar(min=0, max=length(cks), style=3)
  
  iv <- 0
  for(i in 1:length(cks)){
    
    l          <- length(cks[[i]]) # Number of read rows
    ncellsRead <- l * nc # Number of read cells
    idx        <- (iv+1):(iv+ncellsRead) # index of read cells in row-wise reading order
    iv         <- idx[length(idx)] # Update index of last cell
    
    rrow   <- cks[[i]][1]
    predDF <- getValues(s2imgScene, row = rrow, nrows = l)
    
    preds  <- vector(mode="numeric",length=nrow(predDF))
    cidx   <- complete.cases(predDF)
    predDF <- predDF[cidx, ]
    predDF <- predDF %>% as.data.frame()
    
    if(nrow(predDF) > 0){
      preds[!cidx] <- NA
      #preds[cidx]  <- predict(rf, newdata = predDF, type = "prob")[,2] # randomForest package
      preds[cidx]  <- predict(rf, 
                              data        = predDF, 
                              verbose     = FALSE, 
                              num.threads = 2)$predictions[,2] # ranger package
      #preds[cidx]  <- predict(rf, newdata = as.matrix(predDF))
      out[idx]     <- preds
    }else{
      out[idx]     <- NA
    }

    setTxtProgressBar(pb,i)
  }
  
  outFn <- paste(outFolder,"/CLF_IMGS/Cselloana_RFranger_FULL_MWMOTE_csWts_NoClWts_",
                 scnCodes[scnIdx],"_",vn,".tif", sep="")
  
  predRst <- s2imgScene[[1]]
  values(predRst) <- out
  writeRaster(predRst, outFn, overwrite = TRUE)
  
}


