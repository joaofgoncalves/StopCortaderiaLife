

library(stringr)
library(dplyr)
library(tidyr)

source("./RCODE/_INPUT_PARAMS.R")
#source("./RCODE/_INPUT_PARAMS_L8.R")

source("./RCODE/_AUX_FUNS.R")

rstTrainDF <- readRDS("./OUT/LC_rstTrainDF-v4-20201223_205228.RData")


## ------------------------------------------------------------------------------------ ##


tb <- cut(rstTrainDF$percCov,c(-1,0,1,5,10,20,30,50,70,90,100),
    labels=c("0%","]0 - 1%]","]1 - 5%]","]5 - 10%]","]10 - 20%]",
             "]20 - 30%]","]30 - 50%]","]50 - 70%]","]70 - 90%]",
             "]90 - 100%]")) %>% table

freqTb <- data.frame(cl=names(tb),perc=as.numeric(tb))



((freqTb[1,2] / sum(freqTb[,2])) * 100) %>% round(2)

sum(freqTb[2:10,2])
sum(freqTb[8:10,2])
((sum(freqTb[8:10,2]) / sum(freqTb[-1,2])) * 100) %>% round(2)


freqTb <- freqTb %>% 
  mutate(percPresenceOnly = c(NA, ((freqTb[-1,2] / sum(freqTb[-1,2])) * 100) %>% round(2)),
         percAll = ((freqTb[,2] / sum(freqTb[,2])) * 100) %>% round(2)
)

write.csv(freqTb,"./OUT/FrequencyTable_TrainData_S2pixs-v1.csv", row.names = FALSE)


## ------------------------------------------------------------------------------------ ##


pixCount <- c(
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov == 0) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 0, percCov <= 1) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 1, percCov <= 5) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 5, percCov <= 10) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 10, percCov <= 20) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 20, percCov <= 30) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 30, percCov <= 50) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 50, percCov <= 70) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 70, percCov <= 90) %>% nrow,
  rstTrainDF %>% filter(DATA_TYPE == "FIELD", percCov > 90, percCov <= 100) %>% nrow
)

pixStats <- data.frame(np = pixCount) %>% 
  mutate(percTotal = (np/sum(np))*100) %>% 
  mutate(perc2 = (np/sum(pixCount[-1]))*100) %>% 
  mutate(perc2 = replace(perc2,1,NA))

write.csv(pixStats, "./OUT/pixStats-v1.csv")


