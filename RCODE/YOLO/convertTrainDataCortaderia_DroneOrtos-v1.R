
library(raster)
library(SegOptim)
library(TileManager)
library(sf)
library(rgdal)
library(dplyr)
library(stringr)
library(crayon)

# --------------------------------------------------------------------- #
# Set inputs for the script ----
# --------------------------------------------------------------------- #

setwd("D:/DATA/LifeCortaderia/Orthomosaics/__NEW_MOSAICS__2020")

outputDir <- "D:/DATA/LifeCortaderia/YOLOv3_TrainData"

tileDimSize <- c(1664, 1664)

plotData <- FALSE

filterByMin <- FALSE
minNum <- 3

filterByNAs <- TRUE
naPerc <- 0.01 # Proportion of image with NA's



# --------------------------------------------------------------------- #

naNum <- round(tileDimSize[1]*tileDimSize[2]*naPerc)

dirsToProc <- list.dirs(recursive = FALSE)
nDirs <- length(dirsToProc)
id <- 0

pb <- txtProgressBar(1,nDirs,style=3)

for(dirToProc in dirsToProc){
  
  # Generate Sequential ID code for each orthomosaic
  id <- id + 1
  idCode <- str_pad(id, width = 3, side="left", pad=0)
  
  cat(green("\n# ---------- Processing orthoimage ID:",id,"---------- #\n\n"))
  
  # Read drone orthomosaic
  rstPath <- list.files(dirToProc,pattern=".tif$", full.names = TRUE)
  rst <- stack(rstPath)
  
  # Read train data and calculate centroids using sf
  trainDataPath <- list.files(dirToProc, pattern=".shp$", full.names = TRUE)
  trainData <- read_sf(trainDataPath)
  trainData <- trainData %>% mutate(Id = 1:nrow(trainData))
  trainDataCentrs <- st_centroid(trainData) # training area centroids
  
  # Generate a tiling scheme for the input orthomosaic
  tsObj <- tileScheme(input       = rst[[1]], 
                      tiledim     = tileDimSize, 
                      cells       = TRUE, 
                      buffer      = 0, 
                      bufferspill = FALSE,
                      round       = NA,
                      roundDir    = "out",
                      crs         = "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                      origin      = NULL,
                      removeEmpty = FALSE)
  
  tileSchemeDir <- paste(dirToProc,"/_tileScheme",sep="")
  tileSchemePath <- paste(tileSchemeDir,"/tileScheme_v1.shp",sep="")
  dir.create(tileSchemeDir)

  # Save tile scheme object to shapefile
  tileSave(tsObj, tileSchemePath, overwrite = TRUE)
  
  # Number of unique tiles
  n <- length(tsObj@tiles)
  
  # Read tiles and set CRS equal to train data
  tiles <- read_sf(tileSchemePath)
  st_crs(tiles) <- st_crs(trainData)
  
  # Subset tiles for the first band only
  tiles <- tiles[1:n,]
    
  # Subset tiles which intersect train data cortaderia centroids!! (changed it ...)
  tilesInt <- st_intersects(tiles, trainDataCentrs, sparse = FALSE) %>% 
    apply(MARGIN = 1, sum) > 0

  tiles <- tiles[tilesInt, ]
  
  ##
  ## Loop through all tiles that do have training samples
  ii <- 0
  for(i in 1:nrow(tiles)){
    
    # Crop orthomosaic by specific tile
    tileRst <- crop(rst, tiles[i,])
    
    # Check NA values
    if(filterByNAs){
      
      # Get raster tile values to check for NA's (one band only)
      v <- raster::values(tileRst) %>% 
        as.data.frame() %>% 
        `colnames<-`(c("b1","b2","b3")) %>% 
        mutate(s = b1 + b2 + b3) %>% 
        pull(s) == (255*3)
      
      # Suppress tile if it has at least naNum NA's
      if(sum(v) > naNum){
        cat(yellow("\nFound NA (",sum(v),") values for Orthoimage ID:",id,
                   "| Tile ID:",i,"... skipping ...\n\n"))
        next
      }else{
        ii <- ii + 1
        idTileCode <- str_pad(ii, width = 4, side="left", pad=0)
      }
    }
    
    # Convert data to SpatialGridDataFrame to enable writing 'spatial' JPEG format 
    # using rgdal package
    spgdf <- as(tileRst,"SpatialGridDataFrame")
    
    # Save raster tile for specific orthomosaic
    outRstTilePath <- paste(outputDir,"/","train_",idCode,"_",idTileCode,".jpg",sep="")
    outTxtTilePath <- paste(outputDir,"/","train_",idCode,"_",idTileCode,".txt",sep="")
    
    writeGDAL(dataset = spgdf,
              fname   = outRstTilePath,
              driver  = "JPEG", 
              options = c("WORLDFILE=YES","QUALITY=80"))
    
    # Check which centroids intersect the image tile
    spInt <- st_intersects(trainDataCentrs, tiles[i,], sparse = FALSE)[,1]
    # Subset centroids based on the ones that intersect data
    trainDataCentrsInt <- trainDataCentrs[spInt,]
    # Subset train data (full)
    trainDataInt <- trainData[spInt,]
    
    
    # Check if we have a minimum number of samples in the image tile
    # Otherwise skip it!
    if(filterByMin){
      if(nrow(trainDataInt) < minNum){
        cat(yellow("\nFound an insufficient number of training samples... skipping ...\n\n"))
        next
      }
    }
    
    # Extract min-max x/y bounding coordinates for the raster data set
    r_xmin <- xmin(tileRst)
    r_xmax <- xmax(tileRst)
    r_ymin <- ymin(tileRst)
    r_ymax <- ymax(tileRst)
    
    # Calculate the length along the x and y axes
    len_x <- r_xmax - r_xmin
    len_y <- r_ymax - r_ymin
    
    # Extract coordinates for the intersected centroids 
    ptCoords <- as.data.frame(st_coordinates(trainDataCentrsInt))
    
    # Plot data to check ?
    if(plotData){
      plotRGB(tileRst, main=paste("Orthoimage ID:",id,"| Tile ID:",i), axes=TRUE,mar=0)
      plot(trainDataInt, col=NA, lwd=2, add=TRUE, border="yellow")
      plot(trainDataCentrsInt, add=TRUE, col="blue")
    }
      
    if(file.exists(outTxtTilePath)){
      file.remove(outTxtTilePath)
    }
    
    ##
    ## Loop through all ground-truth samples / centroids
    
    for(j in 1:nrow(trainDataInt)){
      
      # Get coordinates for the ground-truth sample circles/boxes
      tmp_coords <- st_coordinates(trainDataInt[j,])
      
      # Dimensions of samples along the x and y axes
      dim_x <- max(tmp_coords[,1]) - min(tmp_coords[,1])
      dim_y <- max(tmp_coords[,2]) - min(tmp_coords[,2])
      
      # Calculate relative dimensions to use in YOLO v3 format
      dim_x_rel <- dim_x / len_x
      dim_y_rel <- dim_y / len_y
      
      # Calculate relative coordinates of the centroids
      x_centr_rel <- (ptCoords[j,"X"] - r_xmin) / len_x
      y_centr_rel <- (r_ymax - ptCoords[j,"Y"]) / len_y
      
      # Generate the txt file for training
      cat("0 ",round(x_centr_rel,6) %>% str_pad(width = 8, side = "right",pad = "0")," ",
          round(y_centr_rel,6) %>% str_pad(width = 8, side = "right",pad = "0")," ",
          round(dim_x_rel,6) %>% str_pad(width = 8, side = "right",pad = "0")," ",
          round(dim_y_rel,6) %>% str_pad(width = 8, side = "right",pad = "0"),"\n",
          append = TRUE,
          file = outTxtTilePath,
          sep = "")
      
    }
  } 
  
  setTxtProgressBar(pb, id)
  cat("\n\n")
}








