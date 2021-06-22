library(gemtc)
library(rjags)
library(doParallel)

load("metadataA5.Weight.Rdata")
load("metadataA5_NB.Fonction.RData")
load("sim.metadataA5_NB.Donnee.100.Rdata")

set.seed(1991)
NS <- 1000
n <- 100

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SA5_NB_100 <- NULL
SA5_NB_100 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=S(metadataA5_NB[[i]], S_weight, weight = 1)
    return(Stemp)
  }


save.image(file=paste("sim","metadataA5_NB",n,"Rdata", sep="."))