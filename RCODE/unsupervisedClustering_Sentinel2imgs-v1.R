
library(caret)
library(randomForest)
library(SegOptim)
library(stringr)
library(dplyr)
library(tidyr)
library(RStoolbox)
library(raster)
library(crayon)
#source("./RCODE/_INPUT_PARAMS.R")


dirsToProc <- list.dirs("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL",
                        recursive = FALSE)

N <- length(dirsToProc)
pb <- txtProgressBar(min = 1, max = N, style = 3)


for(i in 1:N){
  
  dirProc <- dirsToProc[i]
  tileName <- basename(dirProc)
  
  fl <- list.files(dirProc, pattern=".tif$", full.names = TRUE)
  
  flStack <- fl[grepl("_201908",fl)]
  rStack <- stack(flStack)
  
  unC <- unsuperClass(rStack, nSamples = 1E6, nClasses = 20, nStarts = 20)
  fout <- paste("D:/DATA/LifeCortaderia/Kmeans_20c_S2L2A_201908_",tileName,"_v1.tif",sep="")
  writeRaster(unC$map, fout)
  
  cat(green("\n\n-> Finished processing tile: [",tileName,"]\n\n"))
  setTxtProgressBar(pb, i)
}



