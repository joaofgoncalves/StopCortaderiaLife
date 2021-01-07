

source("./RCODE/_AUX_FUNS.R")

# Folder where image mosaics for sampling areas is located
#orthoFolder <- "./DATA_/RASTER/Orthomosaics"
orthoFolder <- "D:/DATA/LifeCortaderia/Orthomosaics"


## SENTINEL-2 image data ----------------------------------------------------------------------------
# Each element of the list corresponds to a raster stack
sceneList <- list(
  sc1 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SMC",
                   pattern = ".tif$", full.names = TRUE),
  sc2 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SMD",
                   pattern = ".tif$", full.names = TRUE),
  sc3 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SNC",
                   pattern = ".tif$", full.names = TRUE),
  sc4 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SND",
                   pattern = ".tif$", full.names = TRUE),
  sc5 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29TME",
                   pattern = ".tif$", full.names = TRUE),
  sc6 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29TNE",
                   pattern = ".tif$", full.names = TRUE),
  sc7 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29TNF",
                   pattern = ".tif$", full.names = TRUE),
  sc8 = list.files("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29TNG",
                   pattern = ".tif$", full.names = TRUE)
  
)

# Sort the file list by date of acquisition instead of name
sortSceneListByDates(sceneList)

# clusterStrataList <- list(
#   sc1 = c("D:/DATA/LifeCortaderia/Km_20c_scn1.tif"),
#   sc2 = c("D:/DATA/LifeCortaderia/Km_20c_scn2.tif"),
#   sc3 = c("D:/DATA/LifeCortaderia/Km_20c_scn3.tif")
# )

scnCodes <- c("T29SMC","T29SMD","T29SNC","T29SND",
              "T29TME","T29TNE","T29TNF","T29TNG")

clusterStrataList <- list(
  sc1 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29SMC_201908_v1.tif",
  sc2 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29SMD_201908_v1.tif",
  sc3 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29SNC_201908_v1.tif",
  sc4 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29SND_201908_v1.tif",
  sc5 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29TME_201908_v1.tif",
  sc6 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29TNE_201908_v1.tif",
  sc7 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29TNF_201908_v1.tif",
  sc8 = "D:/DATA/LifeCortaderia/S2_IMAGES/km_20c_T29TNG_201908_v1.tif"
)

# Band names for each image scene
# An entry should exist for each band in the scene raster stack
# bandNames <- c( "B4_665_nm",
#                 "B3_560_nm",
#                 "B2_490_nm",
#                 "B8_842_nm",
#                 "SRB5_705_nm",
#                 "SRB6_740_nm",
#                 "SRB7_783_nm",
#                 "SRB8A_865_nm",
#                 "SRB11_1610_nm",
#                 "SRB12_2190_nm")
# 
# bandNames <- c(paste(bandNames,"MAY",sep="_"),
#                paste(bandNames,"OCT",sep="_"))
# 
# sceneBandNames <- list(sc1 = bandNames,
#                        sc2 = bandNames,
#                        sc3 = bandNames)


# ARVI, S2 Bands, MCARI, MSI
bandNames <- c( "B4_665_nm",
                "B3_560_nm",
                "B2_490_nm",
                "B8_842_nm",
                "SRB5_705_nm",
                "SRB6_740_nm",
                "SRB7_783_nm",
                "SRB8A_865_nm",
                "SRB11_1610_nm",
                "SRB12_2190_nm",
                "ARVI",
                "MCARI",
                "MSI")

bandNames <- c(paste(bandNames,"MAY",sep="_"),
               paste(bandNames,"JUL",sep="_"),
               paste(bandNames,"AUG",sep="_"),
               paste(bandNames,"SEP",sep="_"),
               paste(bandNames,"OCT",sep="_"))

sceneBandNames <- list(sc1 = bandNames,
                       sc2 = bandNames,
                       sc3 = bandNames,
                       sc4 = bandNames,
                       sc5 = bandNames,
                       sc6 = bandNames,
                       sc7 = bandNames,
                       sc8 = bandNames)


checkOrtho <- TRUE

checkL8ExtentDifferences <- FALSE

threshTrainPos <- 30

removeBelowThresh <- FALSE

extractPseudoAbsences <- TRUE
doSRS <- FALSE
doStRS <- TRUE

randSampSize <- 200 # In StRS this is the amount per stratum

outFolder <- "./OUT/PIXCLASSIFY_2"

outFolderRFobjects <- "./OUT/PIXCLASSIFY_2/CLF_OBJECTS"

viewData <- TRUE

saveData <- TRUE
outFile <- paste("./OUT/LC_rstTrainDF-v4-",
                 format(Sys.time(),"%Y%m%d_%H%M%S"),
                 ".RData",sep="")


