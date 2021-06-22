library(gemtc)
library(rjags)
library(doParallel)

load("metadataA3.Weight.Rdata")
load("metadataA3_NB.Fonction.RData")
load("sim.metadataA3_NB.Donnee.250.Rdata")


set.seed(1991)
NS <- 1000
n <- 250

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SA3_NB_250 <- NULL
SA3_NB_250 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA3_NB[[i]], S_weight, weight = 1)
    return(Stemp)
  }


save.image(file=paste("sim","metadataA3_NB",n,"Rdata", sep="."))