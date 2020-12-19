
library(sf)
library(raster)
library(tools)
library(fasterize)
library(crayon)

sa <- read_sf("./DATA/VECTOR/StudyArea/Buffer50Km_SA_LifeCortaderia_WGS84_UTM29N.shp")

outDir <- "D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL_CROP"

dirsToProc<- list.dirs("D:/DATA/LifeCortaderia/S2_IMAGES/SENTINEL_2019_ALL", recursive=FALSE)


for(i in 1:length(dirsToProc)){
  
  procDir <- dirsToProc[i]
  procDirName <- basename(procDir)
  
  dir.create(paste(outDir,"/",procDirName,sep=""))
  
  fileList <- list.files(procDir, pattern=".tif$", full.names = TRUE)
  
  for(j in 1:length(fileList)){
    
    fn <- fileList[j]
    r <- stack(fn)
    
    fout <- file_path_sans_ext(basename(fn))
    fout <- gsub("_v2","",fout)
    fout <- paste(outDir,"/",procDirName,"/",fout,"_crop_int32.tif",sep="")
    
    if(file.exists(fout)){
      cat(green("\n\nSkipping file:\n"), 
          yellow("  ->",basename(fout),"\n"))
      next
    }
    
    #print(fout)
    
    # if(j==1){
    #   saMask <- fasterize(sa, fn[[1]], field = NULL)
    # }
    
    r_crop <- crop(r, sa)
    r_mask <- mask(r_crop, sa)

    if(grepl("_ARVI.tif$|_MSI.tif$|_MCARI.tif$", fn)){
      print("Found index!")
      r_mask <- round(r_mask * 10000)
    }
    
    writeRaster(r_mask, fout, datatype="INT4S", overwrite=TRUE)
    cat(green("\n\nProcessed file:\n"),yellow("  ->",basename(fout),"\n"))
  }
}


