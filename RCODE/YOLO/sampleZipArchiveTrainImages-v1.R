



library(dplyr)


trainDataPath <- "D:/DATA/LifeCortaderia/YOLOv3_TrainData"

setwd(trainDataPath)

fl <- list.files()[-1]

getOID_ <- function(x) unlist(strsplit(x, split="_"), use.names = FALSE)[2]
getTID_ <- function(x) unlist(strsplit(x, split="_"), use.names = FALSE)[3]

getOID <- function(x) as.integer(sapply(x, getOID_))
getTID <- function(x) as.integer(substr(as.character(sapply(x, getTID_)),1,4))

trainImgData <- data.frame(OID = getOID(fl),
                           TID = getTID(fl), 
                           UID = paste(getOID(fl), "_", getTID(fl),sep=""),
                           fname = fl)

sampleFrac <- 0.5

i <- 0
for(id in unique(trainImgData$OID)){
  
  i<- i + 1
  subsetDF <- trainImgData %>% filter(OID == id)
  
  uniqueIDs <- unique(subsetDF$UID)

  Nsamp <- round(sampleFrac * length(uniqueIDs))
  
  sampledUIDs <- sample(uniqueIDs, Nsamp)
  
  trainFiles_tmp <- subsetDF %>% filter(UID %in% sampledUIDs)
  testFiles_tmp <- subsetDF%>% filter(!(UID %in% sampledUIDs))
  
  if(i == 1){
    trainFiles <- trainFiles_tmp
    testFiles <- testFiles_tmp
  }else{
    trainFiles <- bind_rows(trainFiles, trainFiles_tmp)
    testFiles <- bind_rows(testFiles, testFiles_tmp)
  }
  
}

length(unique(trainFiles$UID))
length(unique(testFiles$UID))

zip("images.zip", files =  trainFiles$fname)

write.csv(trainFiles, "_trainFiles.csv", row.names = FALSE)
write.csv(testFiles, "_testFiles.csv", row.names = FALSE)

trainFiles <- read.csv("_trainFiles.csv")
testFiles <- read.csv("_testFiles.csv")

dir.create("_testData")

filesCopyFrom <- testFiles$fname[grepl(".jpg$",testFiles$fname)]
filesCopyTo <- paste("./_testData/",filesCopyFrom,sep="")

filesRenamed <- gsub("train_","test_",filesCopyTo)

file.copy(filesCopyFrom, filesCopyTo)
file.rename(filesCopyTo,filesRenamed)



