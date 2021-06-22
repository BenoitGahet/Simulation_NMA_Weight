library(tidyverse)

set.seed(1991)
NS <- 1000

#########################################################################################################
####################################### Simulation des données ########################################## 
#########################################################################################################

#########################################################################################################
############################################ Network B ##################################################
#Parameters
n <- 100
PA <- 0.6
PB <- (PA*1.5)/(1-PA*(1-1.5))
#Ni
logOR_ABi <- NULL
logOR_ACi <- NULL
logOR_BCi <- NULL
logOR_ADi <- NULL
PiB <- NULL
NiB <- NULL
PiC <- NULL
NiC <- NULL
PiC2 <- NULL
NiC2 <- NULL
NiA <- NULL
NiB2 <- NULL
PiD <- NULL
NiD <- NULL


############# 5 etudes ##############
## Biais 0.8 ###
metadataB5_B8 <- NULL

for (j in 1:NS) {
  
  for(k in 1:15) {
    logOR_ABi[k] <- rnorm(1,log(1.5),sqrt(0.01))
    PiB[k] <- (PA * exp(logOR_ABi[k]))/(1 - (PA * (1 - exp(logOR_ABi[k]))))
    NiB[k] <- rbinom(1,n,PiB[k])
    
    logOR_ACi[k] <- rnorm(1,log(3),sqrt(0.01))
    PiC[k] <- (PA * exp(logOR_ACi[k]))/(1 - (PA * (1 - exp(logOR_ACi[k]))))
    NiC[k] <- rbinom(1,n,PiC[k])
    
    logOR_BCi[k] <- rnorm(1,log(2*0.8),sqrt(0.01))
    PiC2[k] <- (PB * exp(logOR_BCi[k]))/(1 - (PB * (1 - exp(logOR_BCi[k]))))
    NiC2[k] <- rbinom(1,n,PiC2[k])
    
    logOR_ADi[k] <- rnorm(1,log(4),sqrt(0.01))
    PiD[k] <- (PA * exp(logOR_ADi[k]))/(1 - (PA * (1 - exp(logOR_ADi[k]))))
    NiD[k] <- rbinom(1,n,PiD[k])
    
    NiA[k] <- rbinom(1,n,PA)
    NiB2[k] <-  rbinom(1,n,PB)
    
    metadataB5_B8[[j]] <- tribble(
      ~study, ~responders, ~sampleSize, ~treatment,
      "S1", NiA[1], n, "A",
      "S1", NiB[1], n, "B",
      "S2", NiA[2], n, "A",
      "S2", NiB[2], n, "B",
      "S3", NiA[3], n, "A",
      "S3", NiB[3], n, "B",
      "S4", NiA[4], n, "A",
      "S4", NiB[4], n, "B",
      "S5", NiA[5], n, "A",
      "S5", NiB[5], n, "B",
      "S6", NiA[6], n, "A",
      "S6", NiC[1], n, "C",
      "S7", NiA[7], n, "A",
      "S7", NiC[2], n, "C",
      "S8", NiA[8], n, "A",
      "S8", NiC[3], n, "C",
      "S9", NiA[9], n, "A",
      "S9", NiC[4], n, "C",
      "S10", NiA[10], n, "A",
      "S10", NiC[5], n, "C",
      "S11", NiB2[1], n, "B",
      "S11", NiC2[1], n, "C",
      "S12", NiB2[2], n, "B",
      "S12", NiC2[2], n, "C",
      "S13", NiB2[3], n, "B",
      "S13", NiC2[3], n, "C",
      "S14", NiB2[4], n, "B",
      "S14", NiC2[4], n, "C",
      "S15", NiB2[5], n, "B",
      "S15", NiC2[5], n, "C",
      "S16", NiA[11], n, "A",
      "S16", NiD[1], n, "D",
      "S17", NiA[12], n, "A",
      "S17", NiD[2], n, "D",
      "S18", NiA[13], n, "A",
      "S18", NiD[3], n, "D",
      "S19", NiA[14], n, "A",
      "S19", NiD[4], n, "D",
      "S20", NiA[15], n, "A",
      "S20", NiD[5], n, "D")
  }
}

save(metadataB5_B8, file=paste("sim","metadataB5_B8","Donnee",n,"Rdata", sep="."))