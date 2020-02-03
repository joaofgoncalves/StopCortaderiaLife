
library(raster)
library(dplyr)
library(tidyr)
library(randomForest)
library(BBmisc)
library(stringr)

source("./RCODE/_INPUT_PARAMS.R")

majorityClass <- function(x){
  if(all(is.na(x))){
    return(NA)
  }else{
    as.integer(names(which.max(table(x))))
  }
} 

# bestMods <- perfMetricsAll %>% 
#   group_by(round) %>% 
#   top_n(n = 1, wt = AUC) %>% 
#   as.data.frame() %>% 
#   sample_n(5)

# bestMods <- perfMetricsAll %>% 
#   arrange(desc(AUC)) %>% 
#   `[`(1:20,)
# 



perfMetrics <- perfMetrics %>% 
  mutate(idx=1:nrow(.)) %>% 
  dplyr::select(ncol(.), 1:(ncol(.)-1))

bestMods <- perfMetrics

scnIndex <- 2

# Load and stack each S2 scene
s2imgScene <- raster::stack(sceneList[[scnIndex]])
# Attribute names for each layer in the scene stack
names(s2imgScene) <- sceneBandNames[[scnIndex]]


rowsPerChunk <- 250

chunks <- chunk(1:nrow(s2imgScene), rowsPerChunk)
out <- vector(mode="numeric", length=ncell(s2imgScene))
outClassProb <- vector(mode="numeric", length=ncell(s2imgScene))
nc <- ncol(s2imgScene)
NT <- length(chunks)*nrow(bestMods)
#pb <- txtProgressBar(min=1, max=NT, style=3)


idxLastCell <- 0
itCountAll <- 0
for(i in 1:length(chunks)){
  
  l <- length(chunks[[i]]) # Number of read rows
  ncellsRead <- l * nc # Number of read cells
  idx <- (idxLastCell+1):(idxLastCell+ncellsRead) # index of read cells in row-wise reading order
  idxLastCell <- idx[length(idx)] # Update index of last cell
  
  # Starting row
  rrow <- chunks[[i]][1]
  
  # Load raster data from Sentinel-2 raster files
  predDF <- getValues(s2imgScene, row = rrow, nrows = l)
  nrows <- nrow(predDF)
  
  # Separate complete cases to avoid NA's
  cidx <- complete.cases(predDF)
  predDF <- predDF[cidx, ] # remove NA's from newdata
  predDF <- predDF %>% as.data.frame()
  
  # Final matrix holding binary predictions for majority voting
  ensBinDF <- matrix(0,nrow=nrows,ncol=nrow(bestMods))
  ensClassProbDF <- matrix(0,nrow=nrows,ncol=nrow(bestMods))
  ##
  ## Iterate through all pre-trained classifiers
  ##
  ##
  for(j in 1:nrow(bestMods)){
    preds <- vector(mode="numeric",length=nrows)
    idxBestMod <- bestMods[j,"idx"]
    
    # Filename of the randomForest object
    inputRFfname <- paste(outFolderRFobjects,"/RF_Classifier_KFoldCV_idx_",
                   str_pad(idxBestMod,2,side="left",pad=0),".RData",sep="")
    
    threshVal <- bestMods$AUC.thresh[j]
    
    # Load the pre-trained classifier
    rf <- readRDS(inputRFfname)
    
    preds[!cidx] <- NA
    preds[cidx] <- predict(rf, newdata = predDF, type = "prob")[,2]
    
    ensClassProbDF[,j] <- preds
    ensBinDF[,j] <- as.integer(preds >= threshVal)
    
    itCountAll <- itCountAll + 1
    #setTxtProgressBar(pb,i)
    cat("->",round((itCountAll/NT)*100,1),"% done! Chunk:",i,"/",length(chunks),
        "| Classifier index:",j,"/",nrow(bestMods),"\n\n")
  }
  
  # Majority vote 
  majPred <- apply(ensBinDF, 1, majorityClass)
  avgPred <- apply(ensClassProbDF, 1, mean)
  out[idx] <- majPred
  outClassProb[idx] <- avgPred
}




predRst <- s2imgScene[[scnIndex]]
values(predRst) <- out
writeRaster(predRst, paste("./OUT/PIXCLASSIFY/predRst_RF4b_scn",scnIndex,"_10kcv_v5.tif",sep=""))

predRst <- s2imgScene[[scnIndex]]
values(predRst) <- outClassProb
writeRaster(predRst, paste("./OUT/PIXCLASSIFY/predRstAvgClassProbs_RF4b_scn",scnIndex,"_10kcv_v5.tif",sep=""))
