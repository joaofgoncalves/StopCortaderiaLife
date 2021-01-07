
library(raster)
library(utils)
library(crayon)

source("./RCODE/_AUX_FUNS.R")
source("./RCODE/_INPUT_PARAMS_L8.R")

scnList <- c(sceneList, sceneList_2013)



for(scnIdx in 1:length(scnList)){
  
  for(i in 1:(length(scnList[[scnIdx]]) / nBands) ){
    
    imgBandsIdx <- (1:nBands) + ((i-1)*nBands)
    fnames <- scnList[[scnIdx]][imgBandsIdx]
    activeDir <- dirname(scnList[[scnIdx]][imgBandsIdx[1]])
    allFnames <- list.files(activeDir, full.names = TRUE)
    
    
    cat("\n\n---------------------------------------------------------------------\n")
    cat("Active dir:\n",blue(activeDir),"\n\n")
    cat("Image bands:\n",blue(paste("_",basename(scnList[[scnIdx]][imgBandsIdx])),
                              collapse="\n",sep=""),"\n\n",sep="")
    
    cat("Calculating spectral indices:\n")
    
    outFn_ARVI <- gsub("_B1.TIF","_SPI_ARVI.tif",fnames[1])
    outFn_MCARI <- gsub("_B1.TIF","_SPI_MCARI.tif",fnames[1])
    outFn_MSI <- gsub("_B1.TIF","_SPI_MSI.tif",fnames[1])
    
    checkMCARI <- grepl("_MCARI.tif",allFnames)
    checkARVI  <- grepl("_ARVI.tif",allFnames)
    checkMSI   <- grepl("_MSI.tif",allFnames)
    
    r <- try(raster::stack(fnames))
    
    
    if(!any(checkMCARI)){
      
      cat(yellow("  -> Calculating MCARI index for [",scnIdx,i,"]..."))
      
      rout <- L8_C2_MCARI(r)
      rout <- round(rout * 10000)
      rout[(rout > 30000) | (rout < -30000)] <- NA
      writeRaster(rout, outFn_MCARI, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }else{
      cat(yellow("  -> Skipping MCARI SPI file - already calculated.....\n"))
    }
    
    if(!any(checkARVI)){
      
      cat(yellow("  -> Calculating ARVI index for [",scnIdx,i,"]..."))
      
      rout <- L8_C2_ARVI(r)
      rout <- round(rout * 10000)
      rout[(rout > 30000) | (rout < -30000)] <- NA
      writeRaster(rout, outFn_ARVI, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }else{
      cat(yellow("  -> Skipping ARVI SPI file - already calculated.....\n"))
    }
    
    if(!any(checkMSI)){
      
      cat(yellow("  -> Calculating MSI index for [",scnIdx,i,"]..."))
      
      rout <- L8_C2_MSI(r)
      rout <- round(rout * 10000)
      rout[(rout > 30000) | (rout < -30000)] <- NA
      writeRaster(rout, outFn_MSI, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }else{
      cat(yellow("  -> Skipping MSI SPI file - already calculated.....\n"))
    }
    
    cat(green("\nDONE!"))
    cat("\n\n---------------------------------------------------------------------\n")
  }
  
}
