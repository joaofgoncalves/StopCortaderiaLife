
orthoFolder <- "./DATA_/RASTER/Orthomosaics"

sceneList <- list(
  sc1 = "C:/MyFiles/temp/S2/S2B_MSIL1C_20191010T113319_N0208_R080_T29TNG_20191010T132921_v2.tif",
  sc2 = "C:/MyFiles/temp/S2/S2B_MSIL1C_20191010T113319_N0208_R080_T29TNF_20191010T132921_v2.tif"
)

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

sceneBandNames <- list(sc1 = bandNames,
                       sc2 = bandNames)
threshTrainPos <- 30
removeBelowThresh <- FALSE