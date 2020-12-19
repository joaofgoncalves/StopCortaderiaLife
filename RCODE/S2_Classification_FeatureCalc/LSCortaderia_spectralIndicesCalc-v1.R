
library(raster)
library(crayon)

S2_sr20m_ARVI <- function(r){
  red  <- r[[1]] # red
  blue <- r[[3]] # blue
  nir  <- r[[4]] # NIR
  return(
    (nir - red - (0.106) * (red - blue)) / 
      (nir + red - (0.106) * (red - blue))
  )
}

S2_sr20m_MCARI <- function(r){
  red       <- r[[1]] # red
  red_edge1 <- r[[5]] # red edge #1
  green     <- r[[2]] # green
  return(
    (((red_edge1 - red) - (0.2) * 
        (red_edge1 - green)) * (red_edge1 / red))
  )
}

S2_sr20m_MSI <- function(r){
  
  nir  <- r[[4]] # NIR
  swir <- r[[9]] # swir
  return(swir / nir)
}


## ---------------------------------------------------------------------------- ##


#baseFolder <- "D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL/05_MAIO_2019"
baseFolder <- "D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SNC"

fullList <- list.files(baseFolder, pattern=".tif$",full.names = TRUE)

#baseS2Files <- fullList[!grepl("_ARVI.tif|_MCARI.tif|_MSI.tif",fullList)]
baseS2Files <- fullList[!grepl("_ARVI_|_MCARI_|_MSI_",fullList)]

## ---------------------------------------------------------------------------- ##


for(i in 1:length(baseS2Files)){
  
  fn <- baseS2Files[i]
  cat(green("\n\nChecking file:",basename(fn),".....\n"))
  
  fn_sans_ext <- tools::file_path_sans_ext(fn)
  
  fn_sans_ext <- gsub("_crop_int32","", fn_sans_ext)
  
  fl <- fullList[grepl(fn_sans_ext, fullList)]
  r <- try(raster::stack(fn))
  
  if(inherits(r,"try-error")){
    cat(red("Failed to read file:\n",fn,"\nProceeding to next...\n\n"))
    next
  }
  
  # checkMCARI <- grepl("_MCARI.tif",fl)
  # checkARVI  <- grepl("_ARVI.tif",fl)
  # checkMSI   <- grepl("_MSI.tif",fl)
  # 
  checkMCARI <- grepl("_MCARI_",fl)
  checkARVI  <- grepl("_ARVI_",fl)
  checkMSI   <- grepl("_MSI_",fl)
  
  if(!any(checkMCARI)){
    
    cat(yellow("  -> Calculating MCARI index..."))
    
    #fout <- paste(fn_sans_ext,"_MCARI.tif",sep="")
    fout <- paste(fn_sans_ext,"_MCARI_crop_int32.tif",sep="")
    rout <- S2_sr20m_MCARI(r)
    writeRaster(rout, fout)
    
    cat(yellow("done!\n"))
  }
  
  if(!any(checkARVI)){
    
    cat(yellow("  -> Calculating ARVI index..."))
    
    #fout <- paste(fn_sans_ext,"_ARVI.tif",sep="")
    fout <- paste(fn_sans_ext,"_ARVI_crop_int32.tif",sep="")
    rout <- S2_sr20m_ARVI(r)
    writeRaster(rout, fout)
    
    cat(yellow("done!\n"))
  }
  
  if(!any(checkMSI)){
    
    cat(yellow("  -> Calculating MSI index..."))
    
    #fout <- paste(fn_sans_ext,"_MSI.tif",sep="")
    fout <- paste(fn_sans_ext,"_MSI_crop_int32.tif",sep="")
    rout <- S2_sr20m_MSI(r)
    writeRaster(rout, fout)
    
    cat(yellow("done!\n"))
  }
  
  cat(green("\ndone!"))
}


