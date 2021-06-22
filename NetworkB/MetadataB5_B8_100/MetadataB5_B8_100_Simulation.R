library(gemtc)
library(rjags)
library(doParallel)

load("metadataB5.Weight.Rdata")
load("metadataB5_B8.Fonction.RData")
load("sim.metadataB5_B8.Donnee.100.Rdata")

set.seed(1991)
NS <- 1000
n <- 100

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SB5_B8_100 <- NULL
SB5_B8_100 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB5_B8[[i]], S_weight, weight = 1)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.8

SB5_B8_100_8 <- NULL
SB5_B8_100_8 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB5_B8[[i]], S_weight, weight = 0.8)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.5
SB5_B8_100_5 <- NULL
SB5_B8_100_5 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB5_B8[[i]], S_weight, weight = 0.5)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.1
SB5_B8_100_1 <- NULL
SB5_B8_100_1 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB5_B8[[i]], S_weight, weight = 0.1)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0
SB5_B8_100_0 <- NULL
SB5_B8_100_0 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB5_B8[[i]], S_weight, weight = 0)
    return(Stemp)
  }


save.image(file=paste("sim","metadataB5_B8",n,"Rdata", sep="."))