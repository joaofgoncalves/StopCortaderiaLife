
library(sf)
library(sp)
library(raster)
library(dplyr)
library(tidyr)

csMod_path <- "./DATA/VECTOR/Models/CS_model/CS_model_StudyAreaBuff50Km.shp"

csMod <- read_sf(csMod_path)

print(csMod)

envStb <- csMod %>% pull(CorSel_t_1)

sum(envStb==0)

brks <-  quantile(envStb[envStb>0],probs=c(0.25,0.5,0.75,1))
# brks[1] <- 0 
# brks[2] <- 0.0001
brks <- c(c(-1,0.0001),brks)
csMod <- csMod %>% 
  mutate(ens_cl = cut(envStb, brks, labels=c("Q0","Q1","Q2","Q3","Q4")))

agg <- csMod %>% 
  st_drop_geometry() %>% 
  group_by(ens_cl) %>% 
  summarize(avgHS = mean(CorSel_t_1), 
            medHS = median(CorSel_t_1),
            nc = n())
print(agg)



calcAlloc <- function(x,x1,x2,y1,y2,b){
  m <- (y2-y1)/(x2-x1)
  #print(m)
  out <- (m*x)+b
  return(round(out,0))
}

calcAllocLog <- function(x,m,b){
  out <- m*log(x) + b
  return(round(out,0))
}
 
calcAllocExp <- function(x,m,b){
  out <- m*exp(x*b)
  return(round(out,0))
}

#ns <- sapply(agg$avgHS, calcAlloc, x1=0,x2=733,y1=3,y2=30,b=3)
#ns <- sapply(agg$avgHS, calcAllocLog, m=3.7895, b=5)
ns <- sapply(agg$avgHS, calcAllocExp, m=3, b=0.00225)

print(ns)
sum(ns)


for(i in 1:length(ns)){
  
  TMP <- csMod %>% 
    select(ID, CorSel_t_1, ens_cl) %>% 
    filter(ens_cl == paste("Q",i-1,sep="")) %>% 
    sample_n(size = ns[i])
  
  if(i==1){
    selUnits <- TMP
  }else{
    selUnits <- rbind(selUnits, TMP)
  }
  
}

write_sf(selUnits,"./OUT/SEL_UNITS/selected1km2Units_v1.shp")

plot(selUnits %>% select(ID))







