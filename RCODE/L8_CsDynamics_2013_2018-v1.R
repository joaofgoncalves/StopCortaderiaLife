
library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(fasterize)

L8_cs_2013_mos <- raster("./OUT/PIXCLASSIFY_L8/CLF_IMGS/Mosaics/Cselloana_L8_RF_FULL_2013_R204_P031_033_mos_msk_v1.tif")

L8_cs_2018_mos <- raster("./OUT/PIXCLASSIFY_L8/CLF_IMGS/Mosaics/Cselloana_L8_RF_FULL_2018_R204_P031_033_mos_msk_v1.tif")

sa <- read_sf("./DATA/VECTOR/StudyArea/Buffer50Km_SA_LifeCortaderia_WGS84_UTM29N.shp")

LC_2013_bin <- L8_cs_2013_mos >= 0.67
LC_2018_bin <- L8_cs_2018_mos >= 0.67


#
# distCentroid = sqrt((2  * Area) / (sqrt(3)))

suffix <- "10km2"

areaHex <- 1E7
cellSize <- sqrt((2  * areaHex) / (sqrt(3)))

sa_hex <- st_make_grid(sa, 
                          cellsize = cellSize,
                          square = FALSE, 
                          what = "polygons")

outFn <- paste("./DATA/VECTOR/Grids/HexGrid_",suffix,".shp",sep="")
write_sf(sa_hex, outFn)

sa_hex <- read_sf(outFn) %>% mutate(ID = FID + 1)

sa_hex_r1 <- fasterize(sa_hex, L8_cs_2013_mos, field = "ID")
sa_hex_r2 <- fasterize(sa_hex, L8_cs_2018_mos, field = "ID")

zonal2013 <- zonal(LC_2013_bin, sa_hex_r1, fun="sum")
zonal2018 <- zonal(LC_2018_bin, sa_hex_r2, fun="sum")

zonal2013 <- zonal2013 %>% 
  as.data.frame %>% 
  `colnames<-`(c("ID","s2013"))

zonal2018 <- zonal2018 %>% 
  as.data.frame %>% 
  `colnames<-`(c("ID","s2018"))

sa_hex <- sa_hex %>% 
  left_join(zonal2013, by="ID") %>% 
  left_join(zonal2018, by="ID")

sa_hex <- sa_hex %>%
  mutate( sDiff = ((s2018 - s2013) / s2013)*100 )
  
plot(sa_hex %>% select(sDiff))


outFnDiff <- paste("./OUT/PIXCLASSIFY_L8/STATS/HexGrid_",suffix,"_Stats13_18_v1.shp",sep="")
write_sf(sa_hex, outFnDiff)



