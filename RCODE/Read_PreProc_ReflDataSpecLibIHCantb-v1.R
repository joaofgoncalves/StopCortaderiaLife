


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# Input folder where the data is sitting
#setwd("D:/MyDocs/Projects/LifeCortaderia")

# List all txt converted from the original .asd file format
fl <- list.files("./DATA/TABLES/SpecLib_IHCantabria", pattern=".csv$", full.names = TRUE)


## ---------------------------------------------------------------- ##
## Read al files
pb <- txtProgressBar(1,length(fl),style=3)

for(i in 1:length(fl)){
  
  # Read data from each CSV file in the list
  tb <- read.csv(fl[i], header = TRUE)
  
  # Remove empty column nr 5
  tb <- tb %>% select(-5)
  
  # Get from the metadata the info on the species and habitat type code
  LatinName <- as.character(tb[tb[,5]=="Latin name 1",6])
  HabitatCode <- as.character(tb[tb[,5]=="Habitat Directive Code",6])
  HabitatCode <- ifelse(length(HabitatCode)==0,"",HabitatCode)
  HabitatCode <- gsub("\\ +","",HabitatCode)
  FinalCode <- ifelse(HabitatCode != "",paste(LatinName, HabitatCode, sep=" / "),LatinName)
  PhenoState <- as.character(tb[tb[,5]=="Phenological stage 1",6])
  
  tmp <- data.frame(pointID   = paste(as.character(unique(tb[,1])),"SID",i,sep="_"),
             Type             = FinalCode,
             PhenoState       = PhenoState, 
             stringsAsFactors = FALSE)
  
  if(i==1){
    metaTable <- tmp
  }else{
    metaTable <- rbind(metaTable, tmp)
  }
  
  tmpDF <- tb[,1:3] %>% 
    `colnames<-`(c("pointID","wl","refl")) %>% 
    mutate(pointID = paste(as.character(tb[,1]),"SID",i,sep="_"))
  
  if(i==1){
    reflDF <- tmpDF
  }else{
    reflDF <- rbind(reflDF, tmpDF)
  }
  
  setTxtProgressBar(pb, i)
}


# Convert the dataset into wide format
# Wavelengths are put by column from 325 nm to 1025 nm
reflDF_wide <- spread(reflDF, key = wl, value = refl) %>% 
  left_join(metaTable, by="pointID") 
nc <- ncol(reflDF_wide)
reflDF_wide <- reflDF_wide[,c(1, nc-1, nc, 2:(nc-2))]

write.csv(reflDF_wide, file = "./DATA/TABLES/Cortaderia_ReflProfilesIHCantb_Master-v1.csv",
          row.names = FALSE)

