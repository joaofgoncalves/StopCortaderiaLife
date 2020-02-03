
library(caret)
library(randomForest)
library(SegOptim)
library(stringr)
library(dplyr)
library(tidyr)
library(RStoolbox)
library(raster)

source("./RCODE/_INPUT_PARAMS.R")



r <- stack(sceneList[[3]])
names(r) <- sceneBandNames[[3]]

unC <- unsuperClass(r, nSamples = 1E6, nClasses = 20, nStarts = 20)

writeRaster(unC$map, "D:/DATA/LifeCortaderia/Km_20c_scn3.tif")



