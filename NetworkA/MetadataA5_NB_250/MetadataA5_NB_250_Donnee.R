library(tidyverse)

set.seed(1991)
NS <- 1000
#########################################################################################################
####################################### Simulation des donn�es ########################################## 
#########################################################################################################

#########################################################################################################
############################################ Network A ##################################################
#Parameters
n <- 250
PA <- 0.6
PB <- (PA*1.5)/(1-PA*(1-1.5))
#Ni
logOR_ABi <- NULL
logOR_ACi <- NULL
logOR_BCi <- NULL
PiB <- NULL
NiB <- NULL
PiC <- NULL
NiC <- NULL
PiC2 <- NULL
NiC2 <- NULL
NiA <- NULL
NiB2 <- NULL


##Biais multiple 0.8-1.2 ---------------------------------------------------------
metadataA5_NB <- NULL

for (j in 1:NS) {
  
  for(k in 1:10) {
    logOR_ABi[k] <- rnorm(1,log(1.5),sqrt(0.01))
    PiB[k] <- (PA * exp(logOR_ABi[k]))/(1 - (PA * (1 - exp(logOR_ABi[k]))))
    NiB[k] <- rbinom(1,n,PiB[k])
    
    logOR_ACi[k] <- rnorm(1,log(3),sqrt(0.01))
    PiC[k] <- (PA * exp(logOR_ACi[k]))/(1 - (PA * (1 - exp(logOR_ACi[k]))))
    NiC[k] <- rbinom(1,n,PiC[k])
    
    logOR_BCi[k] <- rnorm(1,log(2),sqrt(0.01))
    PiC2[k] <- (PB * exp(logOR_BCi[k]))/(1 - (PB * (1 - exp(logOR_BCi[k]))))
    NiC2[k] <- rbinom(1,n,PiC2[k])
    
    NiA[k] <- rbinom(1,n,PA)
    NiB2[k] <-  rbinom(1,n,PB)
  }
  
  
  metadataA5_NB[[j]] <- tribble(
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
    "S15", NiC2[5], n, "C")
  
}


### Poids ---------------------------------------------------------------------------------------
S_weight  <- read.table(textConnection('
  study  weight
  S1  1
  S2  1
  S3  1
  S4  1
  S5  1
  S6  1
  S7  1
  S8  1
  S9  1
  S10 1
  S11 1
  S12 1
  S13 1
  S14 1
  S15 1
  '), header=TRUE)


save(metadataA5_NB, file=paste("sim","metadataA5_NB","Donnee",n,"Rdata", sep="."))
save(S_weight, file=paste("metadataA5","Weight","Rdata", sep="."))