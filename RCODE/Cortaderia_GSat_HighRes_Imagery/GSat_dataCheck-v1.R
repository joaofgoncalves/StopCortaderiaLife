
library(raster)
library(tools)
library(crayon)

#img <- "D:/DATA/Google_satellite/C_39_75_75/Google_satellite_C_39_1-4.jpg"

baseDirs <- list.dirs("D:/DATA/Google_satellite", recursive = FALSE)
baseDirs <- baseDirs[-c(1,length(baseDirs))]

imgListFull <- list.files(baseDirs, full.names = TRUE, pattern = ".jpg$", recursive = TRUE)

torm <- 1:(1:length(imgListFull))[grepl("C_47_12-22",imgListFull)]
imgListFull <- imgListFull[-torm]

tot <- length(imgListFull)
#pb <- txtProgressBar(min=1, max=tot, style = 3)
i <- 0
n <- 0

# for(baseDir in baseDirs){
#   
#   imgList <- list.files(baseDir, full.names = TRUE, pattern = ".jpg$")
#     
for(img in imgListFull){
  
  i <- i + 1
  r <- stack(img)
  r_ <- values(r)
  nr <- nrow(r_)
  
  c1 <- sum(r_[,1] == 192) == nr
  c2 <- sum(r_[,2] == 192) == nr
  c3 <- sum(r_[,3] == 192) == nr
  
  if(c1 & c2 & c3){
    n <- n + 1
    fn <- file_path_sans_ext(basename(img))
    
    filesToErase <- paste(fn,c(".dat",".jpg",".jpg.aux.xml",".jpgw",
                               ".kml",".map",".prj",".tab"),sep="")
    
    try(file.remove(filesToErase))
    cat("\n-> Erasing filename:",yellow(fn),"\n")
    cat("[",red(n),"] files removed in total....\n\n")
    
  }
  if((i %% 100)==0){
    percDone <- round(i / length(imgListFull), 3)*100
    cat(green(percDone,"% completed .....\n", sep=""))
  }
  #setTxtProgressBar(pb, i)
  
}

#}

