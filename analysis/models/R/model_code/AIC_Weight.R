
AIC_weight <- function(AIC){
  ncol <- ncol(AIC)
  for(i in 1:nrow(AIC)){
    AIC[i, ncol+1] <- min(AIC[i, 1:ncol])
  }
  
  delta_AIC <- AIC[,1:ncol] - AIC[,ncol+1]
  relative_AIC <- exp( -0.5 * delta_AIC)
  relative_AIC[,ncol+1] <- rowSums(relative_AIC)
  
  AIC_weight <- relative_AIC[,1:ncol]/relative_AIC[,ncol+1]
  
  write.csv(AIC_weight, file = 'AIC_Weight_exp2.csv')
}

setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
exp1 <- read.csv('Exp1_AIC.csv')

setwd("~/git/sourcemem_online/analysis/models/R/experiment_2/output")
exp2 <- read.csv('Exp2_AIC_trimmed.csv')
