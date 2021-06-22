#########################################################################################################
############################################# Fonction ################################################## 
#########################################################################################################
SB <- function(metadata, S_weight, weight = 1) {
  
  metadata <- as.data.frame(metadata)
  # Bias et poids
  S_weight$weight[11] <- weight
  S_weight$weight[12] <- weight
  S_weight$weight[13] <- weight
  S_weight$weight[14] <- weight
  S_weight$weight[15] <- weight
  
  ### Model  ##########
  network <- mtc.network(data.ab=metadata,
                         description="NMA_Random",
                         studies = S_weight)
  
  model_RE <- mtc.model(network, 
                        linearModel='random', 
                        om.scale = 10,
                        re.prior.sd = 1e6,
                        n.chain = 4,
                        powerAdjust = "weight")
  
  mcmc_RE <- mtc.run(model_RE, 
                     n.adapt = 10000, 
                     n.iter = 60000, 
                     thin = 10)
  
  #### Consistency ##################
  node_split <- mtc.nodesplit(network, 
                              comparisons=mtc.nodesplit.comparisons(network),
                              linearModel = "random",
                              om.scale = 10,
                              re.prior.sd = 1e6,
                              n.chain = 4,
                              powerAdjust = "weight",
                              n.adapt = 10000, 
                              n.iter = 60000, 
                              thin = 10)
  summary(node_split)
  
  summary.mtc.nodesplit2 <- function(object, ...) {
    params <- names(object)
    params <- params[params != 'consistency']
    p.value <- do.call(rbind, lapply(params, function(param) {
      samples <- object[[param]][['samples']]
      split <- object[[param]][['model']][['split']]
      samples.dir <- as.matrix(samples[ , 'd.direct', drop=FALSE])
      samples.ind <- as.matrix(samples[ , 'd.indirect', drop=FALSE])
      p <- sum(samples.dir > samples.ind) / length(samples.dir)
      data.frame(t1=split[1], t2=split[2], p=2 * min(p, 1 - p))
    }))
    result <- list(p.value=p.value)
  }
  A <- summary.mtc.nodesplit2(node_split)
  consistency_AB <- A$p.value$p[1]
  consistency_AC <- A$p.value$p[2]
  consistency_BC <- A$p.value$p[3]
  
  #### Convergence ##################
  Conv <- gelman.diag(mcmc_RE)$mpsrf # seuil < 1.1
  
  ### Résultats ####################
  RE <- relative.effect.table(mcmc_RE)
  RE <- summary(relative.effect(mcmc_RE, "A", c("B","C","D")))
  RE2 <- summary(relative.effect(mcmc_RE, "B", c("C","D")))
  RE3 <- summary(relative.effect(mcmc_RE, "C", c("D")))
  
  ### Parameters ###########################################################################################
  LOR_AB <- RE$summaries$statistics[1,1] #log(OR_AB)
  LOR_AC <- RE$summaries$statistics[2,1] #log(OR_AC)
  LOR_AD <- RE$summaries$statistics[3,1] #log(OR_AD)
  LOR_BC <- RE2$summaries$statistics[1,1] #log(OR_BC)
  LOR_BD <- RE2$summaries$statistics[2,1] #log(OR_BD)
  LOR_CD <- RE3$summaries$statistics[1,1] #log(OR_CD)
  #Standard deviation
  SD_AB <- RE$summaries$statistics[1,2]
  SD_AC <- RE$summaries$statistics[2,2]
  SD_AD <- RE$summaries$statistics[3,2] 
  SD_BC <- RE2$summaries$statistics[1,2]
  SD_BD <- RE2$summaries$statistics[2,2] 
  SD_CD <- RE3$summaries$statistics[1,2]
  #IC Log(OR) inf
  IC_AB_inf <- RE$summaries$quantiles[1,1]
  IC_AC_inf <- RE$summaries$quantiles[2,1]
  IC_AD_inf <- RE$summaries$quantiles[3,1]
  IC_BC_inf <- RE2$summaries$quantiles[1,1]
  IC_BD_inf <- RE2$summaries$quantiles[2,1]
  IC_CD_inf <- RE3$summaries$quantiles[1,1]
  #IC Log(OR) sup
  IC_AB_sup <- RE$summaries$quantiles[1,5]
  IC_AC_sup <- RE$summaries$quantiles[2,5]
  IC_AD_sup <- RE$summaries$quantiles[3,5]
  IC_BC_sup <- RE2$summaries$quantiles[1,5]
  IC_BD_sup <- RE2$summaries$quantiles[2,5]
  IC_CD_sup <- RE3$summaries$quantiles[1,5]
  ### Performance ###########################################################################################
  # Coverage probabilty
  CP_AB <- ifelse(log(1.5) > RE$summaries$quantiles[1,1] & log(1.5) < RE$summaries$quantiles[1,5],
                  1,
                  0)
  
  CP_AC <- ifelse(log(3) > RE$summaries$quantiles[2,1] & log(3) < RE$summaries$quantiles[2,5],
                  1,
                  0)
  CP_AD <- ifelse(log(4) > RE$summaries$quantiles[3,1] & log(4) < RE$summaries$quantiles[3,5],
                  1,
                  0)
  CP_BC <- ifelse(log(2) > RE2$summaries$quantiles[1,1] & log(2) < RE2$summaries$quantiles[1,5],
                  1,
                  0)
  CP_BD <- ifelse((log(4) - log(1.5)) > RE2$summaries$quantiles[2,1] & (log(4) - log(1.5)) < RE2$summaries$quantiles[2,5],
                  1,
                  0)
  CP_CD <- ifelse((log(4) - log(3)) > RE3$summaries$quantiles[1,1] & (log(4) - log(3)) < RE3$summaries$quantiles[1,5],
                  1,
                  0)
  #Square error ####
  MSE_AB <- (log(1.5) - RE$summaries$statistics[1,1])^2
  MSE_AC <- (log(3) - RE$summaries$statistics[2,1])^2
  MSE_AD <- (log(4) - RE$summaries$statistics[3,1])^2
  MSE_BC <- (log(2) - RE2$summaries$statistics[1,1])^2
  MSE_BD <- ((log(4) - log(1.5)) - RE2$summaries$statistics[2,1])^2
  MSE_CD <- ((log(4) - log(3)) - RE3$summaries$statistics[1,1])^2
  
  
  ### Consistency #############################################################################################
  ## Inconsistency ##
  Dbar_B <- as.numeric(mcmc_RE$deviance$Dbar)
  
  ### Interert du model #######################################################################################
  #IC_0
  IC_AB <- ifelse(log(1) < IC_AB_inf,
                  1,
                  ifelse(IC_AB_sup > 0,
                         0,
                         2))
  IC_AC <- ifelse(log(1) < IC_AC_inf,
                  1,
                  ifelse(IC_AC_sup > 0,
                         0,
                         2))
  IC_AD <- ifelse(log(1) < IC_AD_inf,
                  1,
                  ifelse(IC_AD_sup > 0,
                         0,
                         2))
  IC_BC <- ifelse(log(1) < IC_BC_inf,
                  1,
                  ifelse(IC_BC_sup > 0,
                         0,
                         2))
  IC_BD <- ifelse(log(1) < IC_BD_inf,
                  1,
                  ifelse(IC_BD_sup > 0,
                         0,
                         2))
  IC_CD <- ifelse(log(1) < IC_CD_inf,
                  1,
                  ifelse(IC_CD_sup > 0,
                         0,
                         2))
  #rank - SUCRA
  rank_B <- rank.probability(mcmc_RE)
  Sucra <- print(sucra(rank_B))
  SA <- as.numeric(Sucra[1])
  SB <- as.numeric(Sucra[2])
  SC <- as.numeric(Sucra[3])
  SD <- as.numeric(Sucra[4])
  
  rank_A1 <- as.numeric(rank.probability(mcmc_RE)[1,1])
  rank_A2 <- as.numeric(rank.probability(mcmc_RE)[1,2])
  rank_A3 <- as.numeric(rank.probability(mcmc_RE)[1,3])
  rank_A4 <- as.numeric(rank.probability(mcmc_RE)[1,4])
  rank_B1 <- as.numeric(rank.probability(mcmc_RE)[2,1])
  rank_B2 <- as.numeric(rank.probability(mcmc_RE)[2,2])
  rank_B3 <- as.numeric(rank.probability(mcmc_RE)[2,3])
  rank_B4 <- as.numeric(rank.probability(mcmc_RE)[2,4])
  rank_C1 <- as.numeric(rank.probability(mcmc_RE)[3,1])
  rank_C2 <- as.numeric(rank.probability(mcmc_RE)[3,2])
  rank_C3 <- as.numeric(rank.probability(mcmc_RE)[3,3])
  rank_C4 <- as.numeric(rank.probability(mcmc_RE)[3,4])
  rank_D1 <- as.numeric(rank.probability(mcmc_RE)[4,1])
  rank_D2 <- as.numeric(rank.probability(mcmc_RE)[4,2])
  rank_D3 <- as.numeric(rank.probability(mcmc_RE)[4,3])
  rank_D4 <- as.numeric(rank.probability(mcmc_RE)[4,4])
  Data_rank <- data.frame(TT = c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D"),
                          C = c("1","2","3","4","1","2","3","4","1","2","3","4","1","2","3","4"),
                          PC = c(rank_A1,rank_A2,rank_A3,rank_A4,rank_B1,rank_B2,rank_B3,rank_B4,rank_C1, rank_C2,rank_C3,rank_C4,rank_D1,rank_D2,rank_D3,rank_D4)
  )                        
  
  return(list(Conv = Conv,
              LOR_AB = LOR_AB, LOR_AC = LOR_AC,  LOR_AD = LOR_AD, LOR_BC = LOR_BC, LOR_BD = LOR_BD, LOR_CD = LOR_CD,
              SD_AB = SD_AB, SD_AC = SD_AC, SD_AD = SD_AD, SD_BC = SD_BC, SD_BD = SD_BD, SD_CD = SD_CD,
              IC_AB_inf = IC_AB_inf, IC_AC_inf = IC_AC_inf, IC_AD_inf = IC_AD_inf, IC_BC_inf = IC_BC_inf, IC_BD_inf = IC_BD_inf, IC_CD_inf = IC_CD_inf,
              IC_AB_sup = IC_AB_sup, IC_AC_sup = IC_AC_sup, IC_AD_sup = IC_AD_sup, IC_BC_sup = IC_BC_sup, IC_BD_sup = IC_BD_sup, IC_CD_sup = IC_CD_sup,
              CP_AB = CP_AB, CP_AC = CP_AC, CP_AD = CP_AD, CP_BC = CP_BC, CP_BD = CP_BD, CP_CD = CP_CD,
              MSE_AB = MSE_AB, MSE_AC = MSE_AC, MSE_AD = MSE_AD, MSE_BC = MSE_BC,MSE_BD = MSE_BD, MSE_CD = MSE_CD,
              Dbar_B = Dbar_B,
              IC_AB = IC_AB, IC_AC = IC_AC, IC_AD = IC_AD, IC_BC = IC_BC, IC_BD = IC_BD, IC_CD = IC_CD,
              SA = SA, SB = SB, SC = SC, SD = SD,
              consistency_AB = consistency_AB, consistency_AC = consistency_AC, consistency_BC = consistency_BC,
              Data_rank = Data_rank)
  )
  
}

save(SB, file = paste("metadataB5_B8","Fonction","RData",sep = "."))