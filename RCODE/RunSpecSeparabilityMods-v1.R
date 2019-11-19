

library(dplyr)
library(caret)
#library(imbalance)
library(parallel)
library(doParallel)
library(forcats)
library(ggplot2)

# Start a parallel cluster for boosting speed in caret processing
#cl <- makePSOCKcluster(10)
#registerDoParallel(cl)


## --------------------------------------------------------------------------- ##

# List the data
fl <- list.files("C:/MyFiles/R-dev/StopCortaderiaLife/DATA/TABLES/ReflDataBySensor", 
                 pattern=".csv$", full.names = TRUE)
# Files by spec lib
fl_CIBIO <- fl[1:13]
fl_IHCant <- fl[14:26]

# Read master files with calibration data i.e., Cortaderia vs. all the rest
tb_CIBIO <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesField_Master-v2.csv")
tb_IHCant <- read.csv("./DATA/TABLES/Cortaderia_ReflProfilesIHCantb_Master-v2.csv")

# Initialize the list object that will hold all the caret train objects
trainObjs <- list()

# List of ML-classification methods to test and 'tunable' parameters
#
# rf          - Random forest (mtry)
# C5.0Tree    - Single C5.0 Tree (none)
# adaboost    - AdaBoost Classification Trees (nIter, method)
# Linda       - Robust Linear Discriminant Analysis (none)
# gam         - Generalized Additive Models (select, method)
# kknn        - k-nearest neighbors (kmax, distance, kernel)
# lssvmRadial - Least Squares Support Vector Machine with Radial Basis Function Kernel (sigma, tau)
# svmRadial   - Support Vector Machines with Radial Basis Function Kernel (sigma, C)
# nnet        - Neural Network (size, decay)
#
mlMethods <- c("rf", "C5.0Tree", "adaboost", "Linda", "gam", 
               "kknn","lssvmRadial", "svmRadial", "nnet")


## --------------------------------------------------------------------------- ##

# Define the control object for caret using Holdout or Group out 
# cross-validation 50/50 % train/test
#
ctrlObj <- trainControl(method = "LGOCV", 
                        number = 500, 
                        p = 0.50, 
                        verboseIter = FALSE)

k <- 0
for(mlMethod in mlMethods){ # Iterate through all the ML-methods
  
  
  for(i in 1:13){ # Iterate through all sensors/platforms
    
    # Read the averaged data from each speclib
    DF1 <- read.csv(fl_CIBIO[i])
    DF2 <- read.csv(fl_IHCant[i])
    NC1 = ncol(DF1)
    NC2 = ncol(DF2)
    vnames = colnames(DF1)
    
    # Check if the number of columns is the same in each 
    # Spectral Library (CIBIO and IH Cantabria)
    # IF NOT, then subset to the intersection of bands 
    # available from both spectroradiometers
    #
    if(NC1 != NC2){
      cat("Found differences for the following tables:\n->",
          basename(fl_CIBIO[i]),"\n->",basename(fl_IHCant[i]),"\n\n")
      
      # Calculate the intersection between the bands available in each dataset
      vnames <- intersect(colnames(DF1), colnames(DF2)) 
      DF1 <- DF1[,vnames]
      DF2 <- DF2[,vnames]
    }
    
    # Assemble the data from both spec libs
    trainDF <-
      rbind(data.frame(Class = tb_CIBIO$Binary, DF1),
            data.frame(Class = tb_IHCant$Binary, DF2)
      )
    
    # Convert the train column to factor object (named 'Class')
    trainDF[,"Class"] <- as.factor(trainDF[,"Class"])
    
    # Train the classifier using
    trainObj <- train(x = trainDF[,-1], 
                      y = trainDF[,1], 
                      # method = "rf",       # Random Forest
                      # method = "C5.0Tree", # Single C5.0 Tree
                      # method = "Linda",    # Robust Linear Discriminant Analysis
                      # method = "qda",      # Quadratic Discriminant Analysis
                      # method = "lda",
                      # method = "kknn",
                      # method = "gam",         # Generalized Additive Model using Splines !!SLOW!!
                      # NOT WORKING: method="fda",
                      # method = "xgbLinear",   # eXtreme Gradient Boosting !!SLOW!!
                      # method = "adaboost",    # AdaBoost Classification Trees
                      # NOT WORKING: method = "bagFDA",
                      # NOT WORKING: method = "bagEarth",
                      # NOT WORKING method = "bayesglm", # Bayesian Generalized Linear Model
                      # NOT WORKING: method = "binda",
                      # NOT WORKING: method = "blackboost",
                      # NOT WORKING: method = "bstTree",
                      # NOT WORKING: method = "randomGLM",
                      # NOT WORKING: method = "SLAVE",
                      # method = "gaussprLinear", # !!!VERY SLOW!!!
                      # NOT WORKING: method = "glm",
                      # NOT WORKING: method = "glmnet",
                      # NOT WORKING: method = "lvq",
                      # method = "lssvmRadial",
                      # method = "avNNet", # Model Averaged Neural Network !! Unstable??
                      # NOT WORKING: method = "gcvEarth",
                      #method = "snn",
                      method = mlMethod,
                      trControl = ctrlObj, 
                      allowParallel = TRUE)
    
    # Make a temporary data frame to store performance values from caret train object
    tmpPerfData <- data.frame(
      Sensor = gsub("CIBIOdata_|.csv","",basename(fl_CIBIO[i])), # Sensor name
      NC1 = NC1,                                                 # Number of columns/bands in CIBIO data
      NC2 = NC2,                                                 # Number of columns in IHC data
      mxKappa = max(trainObj$results$Kappa),                     # Best Kappa
      mxAccuracy = max(trainObj$results$Accuracy),               # Best Accuracy
      varNames = paste(vnames,collapse = " | "),                 # Band names used (intersection between both datasets)
      stringsAsFactors = FALSE)                  
    
    # Append performance scores across different sensors 
    if(i==1){
      perfScores <- tmpPerfData
    }else{
      perfScores <- rbind(perfScores, tmpPerfData)
    }
    
    # Store train objects from caret
    trainObjs[[i]] <- trainObj
    
    # Print the main results in caret train object
    cat("\n\n-------------------------------------------------------------------------\n")
    cat("->",basename(fl_CIBIO[i]),"\n->",basename(fl_IHCant[i]),"\n\n")
    cat("->",toupper(mlMethod),"\n")
    print(trainObj)
    cat("-------------------------------------------------------------------------\n\n")
    
  }
  
  # Append results by ML-classification method
  # Make temp data frame 
  TMP <- data.frame(mlMethod=mlMethod, perfScores)
  
  k <- k+1
  if(k==1){
    perfScoresByMLmethod <- TMP
  }else{
    perfScoresByMLmethod <- rbind(perfScoresByMLmethod, TMP)
  }
  
}

# Write performance data scores to file
write.csv(perfScoresByMLmethod, "./OUT/perfScoresByMLmethod-v1.csv",row.names = FALSE)



load("./OUT/SeparabilityAnalysis_Cortaderia-v1.RData")

# Arrange/sort the output scores
arrange(perfScoresByMLmethod,mlMethod,desc(mxKappa)) %>% 
  mutate(mxKappa=round(mxKappa,3), mxAccuracy=round(mxAccuracy,3))

percDiff <- function(x) ((max(x) - min(x)) / max(x)) * 100

perfScoresByMLmethod %>% 
  filter(Sensor != "Worldview-2_and_3") %>% 
  group_by(mlMethod) %>% 
  summarize(DiffPerc = percDiff(mxKappa))

# Average performance difference (%) between best and worst sensor
perfScoresByMLmethod %>% 
  filter(Sensor != "Worldview-2_and_3") %>% 
  group_by(mlMethod) %>% 
  summarize(DiffPerc = percDiff(mxKappa)) %>% 
  select(DiffPerc) %>% pull %>% mean
  
# Best sensor across all ML methods
medKappa <- perfScoresByMLmethod %>% 
  filter(Sensor != "Worldview-2_and_3") %>% 
  group_by(Sensor) %>% 
  summarize(AvgKappa = mean(mxKappa),
            MedKappa = median(mxKappa)) %>% 
  arrange(desc(MedKappa)) %>% 
  mutate(Sensor = fct_reorder(Sensor, sort(MedKappa, decreasing = FALSE)))

write.csv(medKappa,"./OUT/MedianKappa-BySensorComp-v1.csv")

g <- ggplot(medKappa,aes(x=Sensor, y=MedKappa)) +
  geom_bar(stat="identity") +
  theme_bw() +
  xlab("Sensor name") +
  ylab("Median Kappa") + 
  scale_y_continuous(breaks = seq(0,0.9,0.05), limits = c(0,0.9)) +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

plot(g)

# Average Kappa across ML algorithms
perfScoresByMLmethod %>% 
  filter(Sensor != "Worldview-2_and_3") %>% 
  group_by(mlMethod) %>% 
  summarize(AvgKappa = mean(mxKappa),
            MedKappa = median(mxKappa)) %>% 
  arrange(desc(MedKappa))





#save.image(file = "./OUT/SeparabilityAnalysis_Cortaderia-v1.RData")



