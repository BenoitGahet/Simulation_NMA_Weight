library(gemtc)
library(rjags)
library(doParallel)

load("metadataA5.Weight.Rdata")
load("metadataA5_B6.Fonction.RData")
load("sim.metadataA5_B6.Donnee.250.Rdata")

set.seed(1991)
NS <- 10
n <- 250

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SA5_B6_250 <- NULL
SA5_B6_250 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_B6[[i]], S_weight, weight = 1)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.8

SA5_B6_250_8 <- NULL
SA5_B6_250_8 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_B6[[i]], S_weight, weight = 0.8)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.5
SA5_B6_250_5 <- NULL
SA5_B6_250_5 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_B6[[i]], S_weight, weight = 0.5)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.1
SA5_B6_250_1 <- NULL
SA5_B6_250_1 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_B6[[i]], S_weight, weight = 0.1)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0
SA5_B6_250_0 <- NULL
SA5_B6_250_0 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_B6[[i]], S_weight, weight = 0)
    return(Stemp)
  }


save.image(file=paste("sim","metadataA5_B6",n,"Rdata", sep="."))