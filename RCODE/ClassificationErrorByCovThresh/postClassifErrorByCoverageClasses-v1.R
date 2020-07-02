

library(raster)
library(fasterize)
library(dplyr)
library(tidyr)
library(sf)
library(units)
library(randomForest)
library(randomForestSRC)
library(BBmisc)
library(ggplot2)
library(patchwork)
library(gbm)
library(kernlab)
library(earth)
library(mda)
library(caret)
library(nnet)
library(fastAdaboost)


## -------------------------------------------------------- ##
## Generate a balanced train dataset ----------------------
## -------------------------------------------------------- ##

rstTrainDF <- readRDS(file = "./OUT/LC_rstTrainDF-v1.RData")

posTrainDF <- rstTrainDF %>% 
  filter(percCov > 0) %>% 
  mutate(pCS = 1)
negTrainDF <- rstTrainDF %>% 
  filter(percCov == 0) %>% 
  sample_n(nrow(posTrainDF)) %>% 
  mutate(pCS = 0)

trainDF <- bind_rows(posTrainDF, negTrainDF) %>%
  mutate(pCS = as.factor(pCS)) %>% 
  na.omit

## -------------------------------------------------------- ##
## Calibrate/train classifiers ----------------------------
## -------------------------------------------------------- ##

## Random forest
rfc <- randomForest(formula = form, data = trainDF, ntree=300)

## GBM
form<-as.formula(paste("pCS~",paste(colnames(trainDF)[4:23],collapse="+")))
gbmc <- gbm(form, distribution = "bernoulli", 
            data = trainDF %>% mutate(pCS=as.integer(as.character(pCS))),n.trees = 1000)

## SVM
svmRBF <- ksvm(form, type = "C-svc", data = trainDF, kernel = "rbfdot",
                prob.model=TRUE)

## FDA
flDA <- fda(form, data = trainDF)[,2]

## Neural network
nn <- avNNet(form, data = trainDF, size = 5, decay = 1e-4, maxit = 500)

## Adaboost
adab <- adaboost(form, data = trainDF, nIter=10)


## -------------------------------------------------------- ##
## Predict class labels for train data --------------------
## -------------------------------------------------------- ##

predDF_RF <- data.frame(pred = predict(rfc,type="prob")[,2],
                     obs  = as.integer(as.character(trainDF$pCS)),
                     percCov = trainDF$percCov)

predDF_GBM <- data.frame(pred = predict(gbmc,n.trees=1000,type="response"),
                        obs  = as.integer(as.character(trainDF$pCS)),
                        percCov = trainDF$percCov)

predDF_SVM <- data.frame(pred = predict(svmRBF,trainDF,type="probabilities")[,2],
                     obs  = as.integer(as.character(trainDF$pCS)),
                     percCov = trainDF$percCov)

predDF_FDA <- data.frame(pred = predict(flDA, trainDF, type="posterior")[,2],
                         obs  = as.integer(as.character(trainDF$pCS)),
                         percCov = trainDF$percCov)

predDF_NN <- data.frame(pred = predict(nn, type="prob")[,2],
                         obs  = as.integer(as.character(trainDF$pCS)),
                         percCov = trainDF$percCov)

predDF_ADA <- data.frame(pred = predict(adab,trainDF)$prob[,2],
                        obs  = as.integer(as.character(trainDF$pCS)),
                        percCov = trainDF$percCov)


## -------------------------------------------------------- ##
## Calculate error by cover class -------------------------
## -------------------------------------------------------- ##

algos <- c("predDF_RF","predDF_GBM","predDF_SVM",
           "predDF_FDA","predDF_NN","predDF_ADA")

lowInt <- seq(0, 60, by = 5)
highInt <- c(seq(5, 60, by = 5), 100)
cv <- round(lowInt + ((highInt-lowInt)/2), 2)

print(length(lowInt))
print(length(highInt))

nrunEval <- 30

evalRes <- as.data.frame(matrix(nrow = length(lowInt) * nrunEval * length(algos), 
                  ncol = 5))

colnames(evalRes) <- c("percCovMid", "algo", "Kappa", "AUC", "PSS")
k <- 0

pb <- txtProgressBar(min=1,max=length(lowInt) * nrunEval * length(algos), 
                     style=3)

for(i in 1:length(lowInt)){

  for(j in 1:nrunEval){
    
    for(algo in algos){
      
      k <- k + 1
      
      posCases <- get(algo) %>% filter(percCov > lowInt[i], percCov <= highInt[i])
      negCases <- get(algo) %>% filter(percCov == 0) %>% sample_n(nrow(posCases))
      tmpDF <- bind_rows(posCases, negCases)
      
      kappa <- SegOptim::evalPerformanceGeneric(obs        = tmpDF$obs, 
                                                pred       = tmpDF$pred, 
                                                stat       = "Kappa",
                                                nClassType = "single-class")
      auc <- SegOptim::evalPerformanceGeneric(obs        = tmpDF$obs, 
                                                pred       = tmpDF$pred, 
                                                stat       = "AUC",
                                                nClassType = "single-class")
      pss <- SegOptim::evalPerformanceGeneric(obs        = tmpDF$obs, 
                                              pred       = tmpDF$pred, 
                                              stat       = "PSS",
                                              nClassType = "single-class")
      #c("percCovMid", "algo", "Kappa", "AUC", "PSS")
      evalRes[k,"percCovMid"] <- c(cv[i])
      evalRes[k,"algo"]       <- c(algo)
      evalRes[k,"Kappa"]      <- c(kappa)
      evalRes[k,"AUC"]        <- c(auc)
      evalRes[k,"PSS"]        <- c(pss)
      setTxtProgressBar(pb, k)
    }
  }
}

evalRes <- evalRes %>% mutate(algo=gsub("predDF_","",.data$algo))


## -------------------------------------------------------- ##
## Plot results by classification algorithm ---------------
## -------------------------------------------------------- ##

g0 <- ggplot(evalRes,aes(x=as.factor(percCovMid), y=Kappa)) + 
  geom_boxplot(fill="orange2",color="black", alpha=0.4) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(percCovMid),y=Kappa,group=1),
               fun=median,geom="line",size=0.5,color="blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(percCovMid),y=Kappa,group=1),
               fun=median,geom="point",size=2.5,color="brown3") +
  facet_wrap(~algo) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage (mid-class value)") + 
  ylab("Cohen's Kappa [-1,1]") + 
  labs(title = "Kappa statistic by fractional cover of C. selloana")

plot(g0)

## --------------------------- ##
## --------------------------- ##

g1 <- ggplot(evalRes %>% filter(!(algo%in%c("ADA","FDA"))),aes(x = as.factor(percCovMid), y = AUC)) + 
  geom_boxplot(fill = "orange2", color = "black", alpha = 0.4) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(percCovMid), y = AUC, group = 1),
               fun = median, geom = "line", size = 0.5, color = "blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x = as.factor(percCovMid), y = AUC, group = 1),
               fun = median, geom = "point", size = 2.5, color = "brown3") +
  facet_wrap(~algo) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage (mid-class value)") + 
  ylab("Area Under the Curve [0-1]") + 
  labs(title = "AUC statistic by fractional cover of C. selloana")

plot(g1)

## --------------------------- ##
## --------------------------- ##

evalResSumm <- evalRes %>% 
  group_by(percCovMid) %>% 
  summarise_all(.funs = list(med=mean,mad=sd))


g2 <- ggplot(evalRes,aes(x = as.factor(percCovMid), y = PSS)) + 
  geom_boxplot(fill = "orange2", color = "black", alpha = 0.4) + 
  stat_summary(inherit.aes = FALSE,
               aes(x=as.factor(percCovMid), y = PSS, group = 1),
               fun = median, geom = "line", size = 0.5, color = "blue3") +
  stat_summary(inherit.aes = FALSE,
               aes(x = as.factor(percCovMid), y = PSS, group = 1),
               fun = median, geom = "point", size = 2, color = "brown3") +
  facet_wrap(~algo) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
  xlab("% Coverage (mid-class value)") + 
  ylab("Peirce skill score [-1,1]") + 
  labs(title = "PSS statistic by fractional cover of C. selloana")

plot(g2)

