
library(raster)
library(crayon)

source("../_AUX_FUNS.R")


## ---------------------------------------------------------------------------- ##


#baseFolder <- "D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL/05_MAIO_2019"
#baseFolder <- "D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP/T29SNC"

dirList <- list.dirs("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP",
                     recursive = FALSE)

idx <- 0


for(baseFolder in dirList){
  
  idx <- idx +1
  
  dirBase <- basename(baseFolder)
  
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
      
      cat(yellow("  -> Calculating MCARI index for [",idx,dirBase,"]..."))
      
      #fout <- paste(fn_sans_ext,"_MCARI.tif",sep="")
      fout <- paste(fn_sans_ext,"_crop_int32_MCARI.tif",sep="")
      rout <- S2_sr20m_MCARI(r)
      rout <- round(rout)
      writeRaster(rout, fout, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }
    
    if(!any(checkARVI)){
      
      cat(yellow("  -> Calculating ARVI index for [",idx,dirBase,"]..."))
      
      #fout <- paste(fn_sans_ext,"_ARVI.tif",sep="")
      fout <- paste(fn_sans_ext,"_crop_int32_ARVI.tif",sep="")
      rout <- S2_sr20m_ARVI(r)
      rout <- round(rout * 10000)
      writeRaster(rout, fout, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }
    
    if(!any(checkMSI)){
      
      cat(yellow("  -> Calculating MSI index for [",idx,dirBase,"]..."))
      
      #fout <- paste(fn_sans_ext,"_MSI.tif",sep="")
      fout <- paste(fn_sans_ext,"_crop_int32_MSI.tif",sep="")
      rout <- S2_sr20m_MSI(r)
      rout <- round(rout * 10000)
      writeRaster(rout, fout, datatype="INT4S")
      
      cat(yellow("done!\n"))
    }
    
    cat(green("\ndone!"))
  }
}



