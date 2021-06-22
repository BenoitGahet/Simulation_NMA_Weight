library(gemtc)
library(rjags)
library(doParallel)

load("metadataA3.Weight.Rdata")
load("metadataA3_B6.Fonction.RData")
load("sim.metadataA3_B6.Donnee.100.Rdata")

set.seed(1991)
NS <- 1000
n <- 100

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SA3_B6_100 <- NULL
SA3_B6_100 <- foreach (i=1:NS,combine=list) %dopar% 
              {
                library(gemtc)
                library(rjags)
                Stemp=S(metadataA3_B6[[i]], S_weight, weight = 1)
                return(Stemp)
              }

## vecteur pour  simulations Weight 0.8

SA3_B6_100_8 <- NULL
SA3_B6_100_8 <- foreach (i=1:NS,combine=list) %dopar% 
              {
                library(gemtc)
                library(rjags)
                Stemp=S(metadataA3_B6[[i]], S_weight, weight = 0.8)
                return(Stemp)
              }

## vecteur pour  simulations Weight 0.5
SA3_B6_100_5 <- NULL
SA3_B6_100_5 <- foreach (i=1:NS,combine=list) %dopar% 
              {
                library(gemtc)
                library(rjags)
                Stemp=S(metadataA3_B6[[i]], S_weight, weight = 0.5)
                return(Stemp)
              }

## vecteur pour  simulations Weight 0.1
SA3_B6_100_1 <- NULL
SA3_B6_100_1 <- foreach (i=1:NS,combine=list) %dopar% 
              {
                library(gemtc)
                library(rjags)
                Stemp=S(metadataA3_B6[[i]], S_weight, weight = 0.1)
                return(Stemp)
              }

## vecteur pour  simulations Weight 0
SA3_B6_100_0 <- NULL
SA3_B6_100_0 <- foreach (i=1:NS,combine=list) %dopar% 
              {
                library(gemtc)
                library(rjags)
                Stemp=S(metadataA3_B6[[i]], S_weight, weight = 0)
                return(Stemp)
              }


save.image(file=paste("sim","metadataA3_B6",n,"Rdata", sep="."))