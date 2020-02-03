
library(raster)
library(fasterize)
library(dplyr)
library(tidyr)
library(sf)
library(units)
library(randomForest)
library(randomForestSRC)
library(BBmisc)

source("./RCODE/_INPUT_PARAMS.R")

#rstTrainDF <- rstTrainDF %>% filter(!(percCov>0 & percCov<threshTrainPos))


#rstTrainDF %>% filter(percCov > 50) %>% nrow
#posTrainDF <- rstTrainDF %>% filter(percCov > 0)
posTrainDF <- rstTrainDF %>% filter(percCov >= 30) %>% mutate(pr=1)
negTrainDF <- rstTrainDF %>% filter(percCov == 0) %>% sample_n(size = nrow(posTrainDF))
blrstTrainDF <- rbind(posTrainDF, negTrainDF)

blrstTrainDF <- blrstTrainDF %>% mutate(pr=as.factor(pr))

## Continuous prediction (% cover) ----------------------------------- ##
rstTrainDF1 <- rstTrainDF %>% filter(percCov > 0) 

rf1 <- randomForest(x = rstTrainDF1 %>% dplyr::select(-layer, -ID, -percCov, -pr),
                    y =  rstTrainDF1 %>% dplyr::select(percCov) %>% pull)
print(rf1)
varImpPlot(rf1)

# Balanced data
rf2 <- randomForest(x = blrstTrainDF %>% select(-layer, -ID, -percCov, -pr),
                    y =  blrstTrainDF %>% select(percCov) %>% pull)
print(rf2)
varImpPlot(rf2)

## Binary prediction ------------------------------------------------ ##

rf3 <- randomForest(x = rstTrainDF %>% select(-layer, -ID, -percCov, -pr),
                    y =  rstTrainDF %>% select(pr) %>% pull)
print(rf3)
varImpPlot(rf3)

# Balanced data


for(threshVal in seq(10,50,by=5)){
  
  threshTrainPos <- threshVal
  
  posTrainDF <- rstTrainDF %>% filter(percCov>=threshTrainPos)
  negTrainDF <- rstTrainDF %>% filter(percCov==0) %>% sample_n(size = nrow(posTrainDF))
  blrstTrainDF <- rbind(posTrainDF, negTrainDF)
  
  
  rf4 <- randomForest(x = blrstTrainDF %>% select(-layer, -ID, -percCov, -pr),
                      y =  blrstTrainDF %>% select(pr) %>% pull)
  
  cat("\n\nThreshold value =",threshVal,"% ------ ##\n\n")
  print(rf4)

}



rf4 <- randomForest(x = blrstTrainDF %>% select(-layer, -ID, -percCov, -pr),
                    y =  blrstTrainDF %>% select(pr) %>% pull, ntree = 1000)
                    #classwt = c(10,1))
print(rf4)
varImpPlot(rf4)

#s2pred <- predict(s2, rf4, type="prob")
#writeRaster(s2pred, filename="./OUT/S2pred_rf4_balanced_v2.tif")
#dim(s2imgScene)


# Load and stack each S2 scene
s2imgScene <- raster::stack(sceneList[[1]])
# Attribute names for each layer in the scene stack
names(s2imgScene) <- sceneBandNames[[1]]


rowsPerChunk <- 250

cks <- chunk(1:nrow(s2imgScene), rowsPerChunk)
out <- vector(mode="numeric", length=ncell(s2imgScene))
nc <- ncol(s2imgScene)
pb <- txtProgressBar(min=1, max=length(cks), style=3)


iv = 0
for(i in 1:length(cks)){
  
  l <- length(cks[[i]]) # Number of read rows
  ncellsRead <- l * nc # Number of read cells
  idx <- (iv+1):(iv+ncellsRead) # index of read cells in row-wise reading order
  iv <- idx[length(idx)] # Update index of last cell
  
  rrow <- cks[[i]][1]
  predDF <- getValues(s2imgScene, row = rrow, nrows = l)
  
  preds <- vector(mode="numeric",length=nrow(predDF))
  cidx <- complete.cases(predDF)
  predDF <- predDF[cidx, ]
  
  predDF <- predDF %>% as.data.frame()
  
  preds[!cidx] <- NA
  #preds[cidx] <- predict(rf4, newdata = predDF, type = "prob")[,2]
  preds[cidx] <- predict(rfi, newdata=predDF, 
                         importance=FALSE,
                         proximity=FALSE,
                         distance=FALSE,
                         forest.wt=FALSE,
                         split.depth=FALSE,
                         do.trace=FALSE,
                         membership=FALSE,
                         statistics=FALSE)
    
  out[idx] <- preds

  setTxtProgressBar(pb,i)
}




predRst <- s2imgScene[[1]]
values(predRst) <- out
writeRaster(predRst, "./OUT/PIXCLASSIFY/predRst_RF4b_scn1_v2.tif")
  


## ------------------------------------------------------------------------------- ##
## ------------------------------------------------------------------------------- ##

form <- as.formula(paste("pr~",paste(colnames(blrstTrainDF)[4:13],collapse="+")))

rfi <- imbalanced(formula = form, 
                  data = rstTrainDF %>% select(pr,4:13), 
                  ntree = 500, 
                  method = "rfq",
                  perf.type = "g.mean",
                  fast = TRUE,
                  ratio = NULL,
                  optimize = TRUE,
                  ngrid = 1e4, 
                  importance = FALSE,
                  forest = TRUE)

print(rfi)

preds <- predict(rfi, newdata=rstTrainDF, 
                 importance=FALSE,
                 proximity=FALSE,
                 distance=FALSE,
                 forest.wt=FALSE,
                 split.depth=FALSE,
                 do.trace=FALSE,
                 membership=FALSE,
                 statistics=FALSE)

#predRst <- predict(s2,rfi)

preds$predicted



