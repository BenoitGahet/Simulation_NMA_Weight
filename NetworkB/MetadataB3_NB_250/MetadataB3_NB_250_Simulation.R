library(gemtc)
library(rjags)
library(doParallel)

load("metadataB3.Weight.Rdata")
load("metadataB3_NB.Fonction.RData")
load("sim.metadataB3_NB.Donnee.250.Rdata")


set.seed(1991)
NS <- 1000
n <- 250

cl <- makeCluster(8) #nombre de coeur a mobiliser
registerDoParallel(cl)

#########################################################################################################
############################################# Simulations ############################################### 
#########################################################################################################

SB3_NB_250 <- NULL
SB3_NB_250 <- foreach (i=1:NS,combine=list) %dopar% 
  {
    library(gemtc)
    library(rjags)
    Stemp=SB(metadataB3_NB[[i]], S_weight, weight = 1)
    return(Stemp)
  }


save.image(file=paste("sim","metadataB3_NB",n,"Rdata", sep="."))
