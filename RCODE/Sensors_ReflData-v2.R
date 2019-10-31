library(dplyr)
library(readxl)

setwd("C:/Users/Utilizador/Documents/Repos/StopCortaderiaLife")



test <- read_excel("./DATA/TABLES/BandsSensors_2019_24_10_v1.xlsx", sheet = 2) %>% as.data.frame

#test <- filter(table_w,Costl_Arsol_min:TIRS2_max>=0.4,Costl_Arsol_min:TIRS2_max<=10.75)

print(test)

reflData <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesField_Master-v2.csv")
#reflData <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesIHCantb_Master-v2.csv")
# Valid wavelength columns
wlValidValues <- colnames(reflData)[-c(1:4)]

# Names of the bands in metadata
bandNames <- colnames(test)[-c(1:2)]
# Number of bands in total (for)
numberBands <- length(bandNames) / 2
# Names of the sensors
sensorNames <- as.character(test[,1])

# Define which columns have reflectance data
vr <- 3:ncol(test)
# Prefix for column names for reflectance data
prefix <- "wl_"

outputPrefix <- "CIBIOdata"
#outputPrefix <- "IHCantabriaData"


for(i in 1:nrow(test)){
  
  # Extract band intervals
  bandIntervals <- test[i, vr, drop=FALSE]
  # Remove no data values for columns
  bandIntervals <- bandIntervals[, !is.na(as.numeric(bandIntervals))]
  # Extract valid names for bands
  bnames <- unique(gsub("_min|_max", "", colnames(bandIntervals)))
  # Converto to nanometers
  bandIntervals <- as.numeric(bandIntervals) * 1000
  
  # Sensor name
  sensorName <- test[i,1]
  
  # Control varibales
  bnn <- 0
  bNamesToUse <- c()
  
  for(bn in 0:(numberBands-2)){
    
    j <- 2*bn+1
    k <- 2*bn+2
    
    if((j > length(bandIntervals)) || 
       (k > length(bandIntervals))){
      print("j or k overflow!")
      next
    }
    
    # Band sequence that equals column names
    bInts <- paste(prefix,bandIntervals[j]:bandIntervals[k],sep="")
    # Remove column names in the sequence that do not exsit
    bInts <- bInts[bInts %in% wlValidValues]
    
    if(length(bInts)==0){
      next
    }
    
    bnn <- bnn +1
    bNamesToUse <- c(bNamesToUse, bnames[bn + 1])
    # Calculate average per line
    bandData <- apply(reflData[,bInts], MARGIN = 1, FUN = mean)
    # Accumulate results
    if(bnn==1){
      selReflData <- bandData
    }else{
      selReflData <- cbind(selReflData, bandData)
    }
  }
  
  colnames(selReflData) <- bNamesToUse
  
  fn <- paste("./DATA/TABLES/ReflDataBySensor/",outputPrefix,"_",gsub("\\ +","_",sensorName),".csv",sep="")
  write.csv(selReflData, fn, row.names = FALSE)
  cat(sensorName,".....done!\n\n")
}

