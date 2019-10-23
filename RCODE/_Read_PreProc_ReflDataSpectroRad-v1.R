
library(dplyr)
library(ggplot2)
library(readxl)

# Input folder where the data is sitting
setwd("D:/MyDocs/Projects/LifeCortaderia")

# List all txt converted from the original .asd file format
fl <- list.files("./DATA/TABLES/July2019_JG", pattern=".txt$", full.names = TRUE)

nLinesToIgnore<- 34 # Number of lines in the txt file header to ignore
fs <- 0:209 # File sequence ID
nMeasuresPerPoint <- 10 # number of measurements by point
nPoints <- 21

PIDs <- rep(1:nPoints, each=nMeasuresPerPoint) # Point ID generator


## ---------------------------------------------------------------- ##
## Read al files
pb <- txtProgressBar(1,length(fl),style=3)

for(i in 1:length(fl)){
  
  tb <- read.table(fl[i], header = TRUE, skip = nLinesToIgnore)
  tb <- data.frame(pointID = PIDs[i], seqID = fs[i], tb)
  colnames(tb)[3:4] <- c("wl","refl")
  
  if(i==1){
    reflDF <- tb
  }else{
    reflDF <- rbind(reflDF, tb)
  }
  
  setTxtProgressBar(pb, i)
  
}


## ---------------------------------------------------------------- ##
## Check the data for potential errors
reflCheck <- reflDF %>% 
  group_by(pointID,seqID) %>% 
  summarize(min_wl = min(wl), max_wl = max(wl), nwl=n())

# Identify lines with errors (if any ...)
filter(reflCheck, min_wl != 325)  # check min wavelength value?
filter(reflCheck, max_wl != 1075) # check max wavelength value?
filter(reflCheck, nwl != 751)     # check number of bands


## ---------------------------------------------------------------- ##
## Average data per point

stdErr <- function(x) sd(x)/sqrt(length(x))


reflAvg <- reflDF %>% 
  group_by(pointID, wl) %>% 
  summarize(refl_avg = mean(refl), refl_std=sd(refl), refl_ste = stdErr(refl))

reflAvgCortaderia <- reflAvg %>% 
  filter(pointID %in% c(1:6,15,19)) %>% # Filter sampling points
  group_by(wl) %>% 
  summarize(refl_AV = mean(refl_avg), refl_SD=sd(refl_avg), refl_SE = stdErr(refl_avg))

fieldDF <- read_excel("./DATA/TABLES/LifeCortaderia_filedwork_02.07.2019-v1.xlsx")

reflByLCtype <- reflAvg %>% left_join(fieldDF %>% select(1,2),by = "pointID") %>% 
  group_by(vegetation_plant, wl) %>% 
  summarize(refl_AV = mean(refl_avg), refl_SD=sd(refl_avg), refl_SE = stdErr(refl_avg))


## -------------- ##


g1 <- ggplot(reflAvg %>% filter(pointID %in% c(1:6,15,19), wl >= 350, wl <= 1000), 
            aes(x=wl,y=refl_avg)) + 
  geom_line(aes(color=factor(pointID)), size=1) + 
  #geom_errorbar(aes(ymin=refl_avg-refl_std, ymax=refl_avg+refl_std), width=.2) +
  #scale_color_brewer(palette="BrBG") +
  ylab("Reflectance") + xlab("Wavelength (nm)") +
  theme_bw() + 
  labs(title="Cortaderia selloana reflectance profile (all measurements)",
       subtitle = "Date: 02/07/2019 (14:00-15:00) | LatLon: 41.178349, -8.634895")
  
plot(g1)


## -------------- ##


g2 <- ggplot(reflAvgCortaderia %>% filter(wl >= 350, wl <= 1000), aes(x=wl,y=refl_AV)) + 
  geom_ribbon(aes(ymin = refl_AV - 0.5*refl_SD, ymax = refl_AV + 0.5*refl_SD), fill="dark green",alpha=0.5) +
  geom_line(size=1) + 
  #scale_color_brewer(palette="BrBG") +
  ylab("Reflectance") + xlab("Wavelength (nm)") +
  theme_bw() + 
  labs(title="Cortaderia selloana reflectance profile",
       subtitle = "Date: 02/07/2019 (14:00-15:00) | LatLon: 41.178349, -8.634895")

plot(g2)


## -------------- ##


g3 <- ggplot(reflByLCtype %>% filter(wl >= 350, wl <= 1000, 
                                     !(vegetation_plant %in% c("Daucus sp. (dry)","Mentha sp. / Holcus lanatus",
                                                               "Mix therophytes", "Mentha sp. (fl.)"))), 
             aes(x=wl,y=refl_AV,color=vegetation_plant)) + 
  #geom_ribbon(aes(ymin = refl_AV - 0.5*refl_SD, ymax = refl_AV + 0.5*refl_SD),alpha=0.5) +
  geom_line(size=1) + 
  #scale_color_brewer(palette="BrBG") +
  ylab("Reflectance") + xlab("Wavelength (nm)") +
  labs(title="Comparison of reflectance profiles",
       subtitle = "Date: 02/07/2019 (14:00-15:00) | LatLon: 41.178349, -8.634895") + 
  scale_color_brewer(palette = "Set1",name="") + 
  theme_bw() + 
  theme(legend.position="bottom")

plot(g3)

ggsave(plot = g3, filename = "C:/Users/JG/Desktop/ReflectanceProfile_LifeCortaderia-v1.png", 
       width = 10, height = 8.5)

