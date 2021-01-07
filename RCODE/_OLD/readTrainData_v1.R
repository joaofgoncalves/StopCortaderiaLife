
library(raster)
library(fasterize)
library(dplyr)
library(tidyr)
library(sf)
library(units)

rm(list=ls())

## ------------------------------------------------------------------------------------ ##
## Inputs

#source("./RCODE/_INPUT_PARAMS.R")
source("./RCODE/_INPUT_PARAMS_L8.R")

## End of inputs
## ------------------------------------------------------------------------------------ ##



foldersList <- list.dirs(orthoFolder, recursive=FALSE, full.names = TRUE)
procMosIdxs <- c()

for(scIdx in 1:length(sceneList)){
  
  cat("\n\n## -------------------------------------------------------------------\n")
  cat("## Analyzing scene index:",scIdx,"\n")
  cat("## -------------------------------------------------------------------\n")
  
  # Load and stack each S2 scene
  s2imgScene <- raster::stack(sceneList[[scIdx]])
  # Attribute names for each layer in the scene stack
  names(s2imgScene) <- sceneBandNames[[scIdx]]
  
  i<-0
  ortoMosImgList <- c()
  digitShpList <- c()
  
  # List data in orthomosaics folder
  # -- Folder_Ortho1:
  #  -- GeoTIFF file with mosaic
  #  -- Shapefile with train data
  #
  for(fn in foldersList){
    i<-i+1
    ortoMosImgList[i] <- list.files(fn, pattern=".tif$", full.names = TRUE)[1]
    digitShpList[i] <- list.files(fn, pattern=".shp$", full.names = TRUE)[1]
  }
  
  # Bounding box for the reference S2 image/scene
  e1 <- st_as_sfc(st_bbox(s2imgScene))
  plot(e1)
  i <- 0
  
  
  # Diagnostics matrix with four columns:
  #
  # - [CRS_Equal] - is CRS between s2 scene and ortho equal?
  # - [IsMoContained] - is the orthomosaic contained in the S2 scene?
  # - [HasDigitData] - Does the sample area has digitized data for cortaderia plants?
  # - [DataType] - Geometry data type
  
  cat("\n-> Performing data diagnostics.......\n")
  
  diagMat <- as.data.frame(
    matrix(FALSE,nrow=length(digitShpList),ncol=4))
  colnames(diagMat) <- c("CRS_Equal","IsMoContained",
                         "HasDigitData","DataType")
  
  # Iterate through all mosaics
  #
  for(imgPath in ortoMosImgList){
    
    i <- i+1
    
    ## Check orthomosaic
    if(checkOrtho){
      
      # Load orthomosaic metadata for data tests
      r <- raster(imgPath)
      
      if(compareCRS(s2imgScene,r)){
        
        e2 <- st_as_sfc(st_bbox(r))
        suppressWarnings(st_crs(e2) <- st_crs(e1))
        plot(e2, add=TRUE)
        containTest <- st_contains(e1, e2, sparse = FALSE)[1,1]
        
        diagMat[i,"CRS_Equal"] <- TRUE
        
        if(!containTest){
          
          diagMat[i,"IsMoContained"] <- FALSE
          if(is.na(digitShpList[i])){
            diagMat[i,"HasDigitData"] <- FALSE
          }else{
            diagMat[i,"HasDigitData"] <- TRUE
            diagMat[i,"DataType"] <- as.character(st_geometry_type(read_sf(digitShpList[i]))[1])
          }
        }else{
          diagMat[i,"IsMoContained"] <- TRUE
          
          if(is.na(digitShpList[i])){
            diagMat[i,"HasDigitData"] <- FALSE
          }else{
            diagMat[i,"HasDigitData"] <- TRUE
            diagMat[i,"DataType"] <- as.character(st_geometry_type(read_sf(digitShpList[i]))[1])
          }
        }
      }else{
        diagMat[i,"CRS_Equal"] <- FALSE
        
        if(is.na(digitShpList[i])){
          diagMat[i,"HasDigitData"] <- FALSE
        }else{
          diagMat[i,"HasDigitData"] <- TRUE
          diagMat[i,"DataType"] <- as.character(st_geometry_type(read_sf(digitShpList[i]))[1])
        }
      }
    }
    
    ## --------------------------
    ## Don't check orthomosaics!!
    else{
      
      if(is.na(digitShpList[i])){
        diagMat[i,"HasDigitData"] <- FALSE
      }else{
        diagMat[i,"HasDigitData"] <- TRUE
        diagMat[i,"DataType"] <- as.character(st_geometry_type(read_sf(digitShpList[i]))[1])
      }
      
    }
  }
  
  if(checkOrtho){
    numTests <- 3
    
    # Fill in diagnostics matrix
    diagMat <- cbind(idx   = 1:nrow(diagMat),
                     fn    = basename(ortoMosImgList),
                     tests = apply(diagMat[,-4],1,sum),
                     diagMat)
  }else{
    numTests <- 1
    
    # Fill in diagnostics matrix
    diagMat <- cbind(idx   = 1:nrow(diagMat),
                     fn    = basename(ortoMosImgList),
                     tests = diagMat$HasDigitData,
                     diagMat)
  }
  
  # Write csv diagnostics table
  write.csv(diagMat, paste(outFolder, "/InputDataDiagnostic_scn",scIdx,".csv",sep=""), 
            row.names = FALSE)
  
  
  # Select data that passes all three tests
  selDiagMat <- diagMat %>% filter(tests == numTests)
  
  # Remove already processed samples as to avoid duplications
  # This happens for samples located between scene overlap regions
  #
  if(scIdx>1){
    selDiagMat <- selDiagMat %>% filter(!(idx %in% procMosIdxs))
  }
  procMosIdxs <- c(procMosIdxs, selDiagMat$idx) # Accumulate new idxs after filtering...
  
  # Create a raster grid with pixel indices of the the reference s2imgScene scene
  s2Ind <- raster(s2imgScene[[1]])
  values(s2Ind) <- 1:ncell(s2imgScene)
  
  
  # ---------------------------------------------------------------------------- #
  # Iterate through all sample areas selected (meeting all criteria)
  # ---------------------------------------------------------------------------- #
  #
  cat("\n-> Calculating percentage cover by pixel.......\n\n")
  nr <- nrow(selDiagMat)
  pb <- txtProgressBar(min=1, max=nr, style=3)
  bboxes <- list()
  
  for(i in 1:nr){
    
    # Index for the selected mosaic/sample/train area
    idx <- selDiagMat$idx[i]
    
    # get the orthomosaic path and image metadata
    imgPath <- ortoMosImgList[idx]
    orthoRst <- raster(imgPath)
    # extract the bounding box for the scene
    bb <- st_as_sf(st_as_sfc(st_bbox(orthoRst)))
    
    # get/read the digitized data and dissolve it (to avoid 
    # spatially overlapping 'plant' circles which would 
    # potentially cause coverage > 100%)
    shp <- read_sf(digitShpList[idx])
    suppressWarnings(shp_un <- st_union(shp))
    
    if(i==1){
      sampInds <- select(shp)
    }else{
      sampInds <- rbind(sampInds,select(shp))
    }
    
    
    if(i==1) crs_to_set <- st_crs(bb)
    suppressWarnings(st_crs(bb) <- crs_to_set)
    bboxes[[i]] <- bb
    # get the reference sentinel-2 scene indices
    s2IndMask <- mask(crop(s2Ind, bb), bb)
    
    # convert pixel/indices to polygons (faster processing this)
    rstGrid <- st_as_sf(rasterToPolygons(s2IndMask)) %>% 
      mutate(init_area = drop_units(st_area(.)))
    
    # intersect the vectorized raster grid and extract the % cover
    # (only applies to intersect cells...?!)
    rstInt <- st_intersection(rstGrid, shp_un) %>% 
      mutate(ints_area = drop_units(st_area(.))) %>%
      mutate(percCov = (ints_area/init_area)*100) %>% 
      st_drop_geometry()
    
    # joint the data for intersected cells to all the vectorized grid
    tmpRstGrid <- rstGrid %>% 
      st_drop_geometry() %>% 
      left_join(rstInt %>% select(-init_area), by = "layer") %>% 
      mutate(ints_area = coalesce(ints_area, 0)) %>% 
      mutate(percCov   = coalesce(percCov, 0)) %>% 
      drop_units() %>% 
      group_by(layer) %>% 
      # aggregate possible disjunct areas for same pixel
      summarize(percCov = sum(percCov), .groups="drop") %>%  
      as.data.frame()
    
    # append data for all selected train areas
    # the 'layer' field in rstGrid shows the pixel indices 
    # from the s2 reference scene
    #
    if(i==1){
      rstGridDF <- tmpRstGrid
    }else{
      rstGridDF <- rbind(rstGridDF, tmpRstGrid)
    }
    
    setTxtProgressBar(pb, value = i)
  }
  
  cat("\n\n-> Exporting bounding boxes and percentage cover training data.......")
  # Export bounding boxes of mosaics and digitized samples of cortaderia individuals
  bbs <- do.call(what = sf:::rbind.sf, args = bboxes)
  write_sf(bbs,paste(outFolder, "/bboxes_sample_areas_scn_",scIdx,".shp"))
  write_sf(sampInds,paste(outFolder, "/sample_digit_inds_scn",scIdx,".shp",sep=""))
  # Export coverage percentage using the S2 reference scene as raster grid
  s2TrainPercRst <- s2Ind
  values(s2TrainPercRst) <- 0
  values(s2TrainPercRst)[rstGridDF$layer] <- rstGridDF$percCov
  outPercTrainRstPath <- paste(outFolder, "/s2TrainPercRst_scn",scIdx,"_v1.tif",sep="")
  writeRaster(s2TrainPercRst, outPercTrainRstPath, overwrite=TRUE)
  
  
  # Extract spectral data from s2 scene ------------------------- #
  cat("\n\n-> Extracting data for S2 scene index [",scIdx,"].......")
  
  # Get pixel coordinates from cell number IDs as a spatial object 
  xyPixels <- xyFromCell(s2imgScene, rstGridDF$layer, spatial=TRUE)
  rstS2DF <- raster::extract(s2imgScene, xyPixels, df=TRUE, cellnumbers=TRUE)
  
  # Compile training data --------------------------------------- #
  rstTrainDFtmp <- rstGridDF %>% 
    left_join(rstS2DF, by=c("layer"="cells")) %>% 
    mutate(pr = ifelse(percCov >= threshTrainPos, 1, 0)) %>% # Add a binary column
    mutate(pr = as.factor(pr))
  
  # Remove cases with low % cover (above 0 and below the threshold)
  if(removeBelowThresh){
    rstTrainDFtmp <- rstTrainDFtmp %>% filter(!(percCov>0 & percCov<threshTrainPos))
  }
  
  # Append train data across multiple S2 scenes
  if(scIdx==1){
    rstTrainDF <- rstTrainDFtmp
  }else{
    rstTrainDF <- rbind(rstTrainDF, rstTrainDFtmp)
  }
}



