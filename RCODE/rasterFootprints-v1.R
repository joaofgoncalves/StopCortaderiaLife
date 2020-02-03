
library(raster)
library(sf)
library(dplyr)

inputFolder <- "input/folder"

fileList <- list.files(inputFolder, pattern=".tif$", full.names = TRUE)

for(fn in fileList){
  
  r <- raster(fileList)
  
  bb <- st_as_sfc(st_bbox(r))
  bb <- bbox %>% mutate(fn = basename(fn))
  
  if(i==1){
    bbs <- bb
  }else{
    bbs <- rbind(bbs,bb)
  }
  
}

write_sf(bbs,"out_footprints.shp")
