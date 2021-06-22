library(gemtc)
library(rjags)
library(doParallel)

load("metadataB3.Weight.Rdata")
load("metadataB3_B6.Fonction.RData")
load("sim.metadataB3_B6.Donnee.100.Rdata")

set.seed(1991)
NS <- 1000
n <- 100

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SB3_B6_100 <- NULL
system.time(SB3_B6_100 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_B6[[i]], S_weight, weight = 1)
    return(Stemp)
  })

## vecteur pour  simulations Weight 0.8

SB3_B6_100_8 <- NULL
SB3_B6_100_8 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_B6[[i]], S_weight, weight = 0.8)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.5
SB3_B6_100_5 <- NULL
SB3_B6_100_5 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_B6[[i]], S_weight, weight = 0.5)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0.1
SB3_B6_100_1 <- NULL
SB3_B6_100_1 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_B6[[i]], S_weight, weight = 0.1)
    return(Stemp)
  }

## vecteur pour  simulations Weight 0
SB3_B6_100_0 <- NULL
SB3_B6_100_0 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_B6[[i]], S_weight, weight = 0)
    return(Stemp)
  }

save.image(file=paste("sim","metadataB3_B6",n,"Rdata", sep="."))