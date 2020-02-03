
# Folder where image mosaics for sampling areas is located
orthoFolder <- "./DATA_/RASTER/Orthomosaics"

# List of Sentinel-2 image scenes
# Each element of the list corresponds to a raster stack
sceneList <- list(
  sc1 = c(
    "D:/DATA/LifeCortaderia/SRB/L2A/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNG_20190530T132835_crop_v2.tif",
    "D:/DATA/LifeCortaderia/SRB/L2A/S2B_MSIL2A_20191010T113319_N0213_R080_T29TNG_20191010T141358_crop_v2.tif"
    ),
  
  sc2 = c(
    "D:/DATA/LifeCortaderia/SRB/L2A/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNF_20190530T132835_crop_v2.tif",
    "D:/DATA/LifeCortaderia/SRB/L2A/S2B_MSIL2A_20191010T113319_N0213_R080_T29TNF_20191010T141358_crop_v2.tif"),
  
  sc3 = c(
    "D:/DATA/LifeCortaderia/SRB/L2A/S2B_MSIL2A_20190530T112119_N0212_R037_T29TNE_20190530T132835_crop_v2.tif",
    "D:/DATA/LifeCortaderia/SRB/L2A/S2A_MSIL2A_20191022T112121_N0213_R037_T29TNE_20191022T123705_crop_v2.tif")
)

# 
clusterStrataList <- list(
  sc1 = c("D:/DATA/LifeCortaderia/Km_20c_scn1.tif"),
  sc2 = c("D:/DATA/LifeCortaderia/Km_20c_scn2.tif"),
  sc3 = c("D:/DATA/LifeCortaderia/Km_20c_scn3.tif")
)

# Band names for each image scene
# An entry should exist for each band in the scene raster stack
bandNames <- c( "B4_665_nm",
                "B3_560_nm",
                "B2_490_nm",
                "B8_842_nm",
                "SRB5_705_nm",
                "SRB6_740_nm",
                "SRB7_783_nm",
                "SRB8A_865_nm",
                "SRB11_1610_nm",
                "SRB12_2190_nm")

bandNames <- c(paste(bandNames,"MAY",sep="_"),
               paste(bandNames,"OCT",sep="_"))

sceneBandNames <- list(sc1 = bandNames,
                       sc2 = bandNames,
                       sc3 = bandNames)

threshTrainPos <- 30

removeBelowThresh <- FALSE

extractPseudoAbsences <- TRUE
doSRS <- FALSE
doStRS <- TRUE

randSampSize <- 100 # In StRS this is the amount per stratum

outFolder <- "./OUT/PIXCLASSIFY"

outFolderRFobjects <- "./OUT/PIXCLASSIFY/RFobjects"

