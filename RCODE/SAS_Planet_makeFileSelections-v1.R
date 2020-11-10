
library(sf)
library(raster)
library(dplyr)

shpPath <- "C:/MyFiles/R-dev/StopCortaderiaLife/DATA/VECTOR/Grids/Grid30k_WGS84GCS_intBuffer_v4.shp"

shp <- read_sf(shpPath)

coords <- st_coordinates(shp) %>% as.data.frame()

#View(coords)

outPath <- "C:/Users/JG/Desktop/Selections"

for(i in 1:nrow(shp)){
  
  UID <- as.character(st_drop_geometry(shp)[i,"UID"])
  
  sink(file = paste(outPath,"/Selection_UID_",UID,"_seqNr_",i,".hlg",sep=""))
  
  coords <- st_coordinates(shp[i,])
  
  
  cat("[HIGHLIGHTING]\nZoom=21\n",sep="")
  
  for(j in 1:nrow(coords)){
    
    cat("PointLon_",j,"=",coords[j,"X"],"\n",sep="")
    cat("PointLat_",j,"=",coords[j,"Y"],"\n",sep="")
    
  }
  #cat("\n",sep="")
  sink()
}



