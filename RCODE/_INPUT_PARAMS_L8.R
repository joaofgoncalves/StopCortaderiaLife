
source("./RCODE/_AUX_FUNS.R")

# Folder where image mosaics for sampling areas is located
#orthoFolder <- "./DATA_/RASTER/Orthomosaics"
orthoFolder <- "D:/DATA/LifeCortaderia/Orthomosaics"

checkOrtho <- TRUE


## LANDSAT 8 image data ----------------------------------------------------------------------------

bandFileIdxs <- c(3:9,11:13)

sceneList_2013 <- list(
  
  sc1 = c(list.files("D:/DATA/LifeCortaderia/L8/2013/204-031/LC08_L2SP_204031_20130706_20200912_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2013/204-031/LC08_L2SP_204031_20131010_20200912_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs]),
  
  sc2 = c(list.files("D:/DATA/LifeCortaderia/L8/2013/204-032/LC08_L2SP_204032_20130620_20200912_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2013/204-032/LC08_L2SP_204032_20131010_20200913_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs]),
  
  sc3 = c(list.files("D:/DATA/LifeCortaderia/L8/2013/204-033/LC08_L2SP_204033_20130706_20200912_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2013/204-033/LC08_L2SP_204033_20131010_20200912_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs])
  
)

sceneList <- list(
  
  sc1 = c(list.files("D:/DATA/LifeCortaderia/L8/2018/204-031/LC08_L2SP_204031_20180618_20200831_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2018/204-031/LC08_L2SP_204031_20181008_20200830_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs]),
  
  sc2 = c(list.files("D:/DATA/LifeCortaderia/L8/2018/204-032/LC08_L2SP_204032_20180618_20200831_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2018/204-032/LC08_L2SP_204032_20181008_20200830_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs]),
  
  sc3 = c(list.files("D:/DATA/LifeCortaderia/L8/2018/204-033/LC08_L2SP_204033_20180517_20200901_02_T1", 
                     pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs],
          list.files("D:/DATA/LifeCortaderia/L8/2018/204-033/LC08_L2SP_204033_20181008_20200830_02_T1", 
                   pattern=".TIF$|.tif$", full.names = TRUE)[bandFileIdxs])
  
)


clusterStrataList <- list(
  sc1 = "D:/DATA/LifeCortaderia/L8/2018/KMeans/L8_204031_201810_KMclust_20c_v1.tif",
  sc2 = "D:/DATA/LifeCortaderia/L8/2018/KMeans/L8_204032_201810_KMclust_20c_v1.tif",
  sc3 = "D:/DATA/LifeCortaderia/L8/2018/KMeans/L8_204033_201810_KMclust_20c_v1.tif"
)


checkL8ExtentDifferences <- TRUE

nBands <- 7

scnCodes <- c("PR204031","PR204032","PR204033")

bandsEMparts <- c("COAST","BLUE","GREEN","RED","NIR","SWIR1","SWIR2")

bandNames <- c(paste("B",1:7,"_",bandsEMparts,sep=""),
               "ARVI","MCARI","MSI")

bandNames <- c(
  paste(bandNames,"S1",sep="_"),
  paste(bandNames,"S2",sep="_")
)

sceneBandNames <- list(sc1 = bandNames,
                       sc2 = bandNames,
                       sc3 = bandNames)

threshTrainPos <- 20

removeBelowThresh <- FALSE

extractPseudoAbsences <- TRUE
doSRS <- FALSE
doStRS <- TRUE

# In StRS this is the amount per stratum
randSampSize <- 200 

outFolder <- "./OUT/PIXCLASSIFY_L8"

outFolderRFobjects <- "./OUT/PIXCLASSIFY_L8/CLF_OBJECTS"

viewData <- TRUE

saveData <- TRUE
outFile <- paste("./OUT/LC_Landsat8_rstTrainDF-v1-",
                 format(Sys.time(),"%Y%m%d_%H%M%S"),
                 ".RData",sep="")
