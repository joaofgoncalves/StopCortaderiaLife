

library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(fasterize)


cs <- raster("./OUT/PIXCLASSIFY_2/CLF_IMGS/v6/mosaic/Cselloana_S2_RFranger_FULL_mos_mask_v2.tif")

csBin <- cs > 0.6

hexGrid10k <- read_sf("./DATA/VECTOR/Grids/HexGrid_10km2.shp") %>% mutate(ID = FID + 1)

sa_hex_r1 <- fasterize(hexGrid10k, cs, field = "ID")

zonalS2 <- zonal(csBin, sa_hex_r1, fun="sum")

zonalS2 <- zonalS2 %>% 
  as.data.frame %>% 
  `colnames<-`(c("ID","cs_area"))

sa_hex <- hexGrid10k %>% 
  left_join(zonalS2, by="ID")


write_sf(sa_hex,"./OUT/PIXCLASSIFY_2/STATS/HexGrid10K_S2_cs_stats_19_20_v1.shp")
