
library(dplyr)
library(sf)

## ----------------------------------------------------------------------------- ## 
## Count the number of photos per survey
## ----------------------------------------------------------------------------- ## 

dirSurveys <- "D:/DATA/LifeCortaderia/UAV_Surveys/___NEW_SURVEYS___2020"

fl <- list.files(dirSurveys, pattern=".jpg$|.JPG$", recursive = TRUE, full.names = FALSE)

baseDir <- fl %>% sapply(FUN = function(x) unlist(strsplit(x,"/"),use.names = FALSE)[1])


DF <- data.frame(bd = as.character(baseDir), stringsAsFactors = FALSE) %>% 
  group_by(bd) %>% 
  summarize(nc = n())

View(DF)

write.csv(DF,"./OUT/NumPicsPerSurvey_Season#2.csv",row.names = FALSE)



## ----------------------------------------------------------------------------- ## 
## Plant sizes
## ----------------------------------------------------------------------------- ## 

dirSurveys <- c("D:/DATA/LifeCortaderia/Orthomosaics",
                "D:/DATA/LifeCortaderia/Orthomosaics/__NEW_MOSAICS__2020")

dirList <- list.dirs(dirSurveys, recursive = FALSE, full.names = TRUE)
dirList <- dirList[-1]
dirList <- dirList[c(31:63,1:30)]

i <- 0

for(dirName in dirList){
  
  i <- i + 1
  
  shpFile <- list.files(dirName, pattern=".shp$", full.names = TRUE)
  
  shp <- read_sf(shpFile) %>% 
    mutate(area_m2 = st_area(.)) %>% 
    mutate(raio = sqrt(area_m2/ pi))
  
  tmp <- shp %>% 
    select(area_m2, raio) %>% 
    bind_cols(data.frame(OID = i, fn = basename(shpFile))) %>% 
    select(OID,fn,area_m2,raio)
  
  if(i==1){
    plantSizes <- tmp
  }else{
    plantSizes <- bind_rows(plantSizes, tmp)
  }
  
}

mean(plantSizes$raio* 2)
sd(plantSizes$raio * 2)
sd(plantSizes$raio * 2)/sqrt(nrow(plantSizes))

quantile(plantSizes$raio* 2)

aggPlantDataBySurvey <- plantSizes %>% 
  st_centroid() %>% 
  group_by(OID) %>%
  summarize(
    med_dim = mean(raio*2),
    num = n())

aggPlantDataBySurvey <-  aggPlantDataBySurvey %>% 
  st_centroid(.)


plot(aggPlantDataBySurvey)

write_sf(aggPlantDataBySurvey,"./OUT/allSurveysPlantAvgSize_v1.shp")



