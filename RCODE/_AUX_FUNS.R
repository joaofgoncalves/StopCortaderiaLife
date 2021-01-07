

get_S2_dates <- function(x){
  getS2Date <- function(x) substr(unlist(strsplit(basename(x),"_"),
                                         use.names = FALSE)[3],1,8)
  out <- sapply(X = x, FUN = getS2Date)
  names(out) <- NULL
  return(out)
}

sortSceneListByDates <- function(x){
  outList <- list()
  for(i in 1:length(x)){
    dts <- get_S2_dates(x[[i]])
    out <- x[[i]][order(dts)]
    outList[[paste("sc",i,sep="")]] <- out
  }
  return(outList)
}

F1max_SingleClass <- function(obs, pred){
  
  i<-0
  k<-vector(mode="numeric", length = 101)
  thresh.vals<-vector(mode="numeric", length = 101)
  
  for(thresh in seq(0,1,0.01)){
    
    i<-i+1	
    k[i]<-evaluatePerformance(obs,as.integer(pred > thresh))$Metrics$F1[1]
    thresh.vals[i]<-thresh
  }
  
  DF<-data.frame(thresh=thresh.vals,F1=k)
  return(list(values=DF, maxF1=DF[which.max(DF[,2]),]))
}


## --------------------------------------------------- ##


S2_sr20m_ARVI <- function(r){
  red  <- r[[1]] # red
  blue <- r[[3]] # blue
  nir  <- r[[4]] # NIR
  return(
    (nir - red - (0.106) * (red - blue)) / 
      (nir + red - (0.106) * (red - blue))
  )
}

S2_sr20m_MCARI <- function(r){
  red       <- r[[1]] # red
  red_edge1 <- r[[5]] # red edge #1
  green     <- r[[2]] # green
  return(
    (((red_edge1 - red) - (0.2) * 
        (red_edge1 - green)) * (red_edge1 / red))
  )
}

S2_sr20m_MSI <- function(r){
  
  nir  <- r[[4]] # NIR
  swir <- r[[9]] # swir
  return(swir / nir)
}


## --------------------------------------------------- ##


L8_C2_ARVI <- function(r){
  red  <- r[[4]] * 2.75e-05 - 0.2 # red
  blue <- r[[2]] * 2.75e-05 - 0.2 # blue
  nir  <- r[[5]] * 2.75e-05 - 0.2 # NIR
  return(
    (nir - red - (0.106) * (red - blue)) / 
      (nir + red - (0.106) * (red - blue))
  )
}

L8_C2_MCARI <- function(r){
  red   <- r[[4]] * 2.75e-05 - 0.2 # red
  nir   <- r[[5]] * 2.75e-05 - 0.2 # red edge #1 -> NIR
  green <- r[[3]] * 2.75e-05 - 0.2 # green
  return(
    (((nir - red) - (0.2) * 
        (nir - green)) * (nir / red))
  )
}

L8_C2_MSI <- function(r){
  
  nir  <- r[[5]] * 2.75e-05 - 0.2 # NIR
  swir <- r[[6]] * 2.75e-05 - 0.2 # swir
  return(swir / nir)
}
