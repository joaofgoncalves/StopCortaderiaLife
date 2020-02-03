

library(dplyr)


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
