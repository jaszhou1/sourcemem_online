# Individual AIC for Exp 1
setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
exp1 <- read.csv('Exp1_AIC.csv', fileEncoding="UTF-8-BOM")
setwd("~/git/sourcemem_online/analysis/models/MATLAB/experiment_1/output")
exp1_diff <- read.csv('Exp1_AIC.csv', fileEncoding="UTF-8-BOM")
AIC_weight <- function(AIC, filename){
  ncol <- ncol(AIC)
  for(i in 1:nrow(AIC)){
    AIC[i, ncol+1] <- min(AIC[i, 1:ncol])
  }
  
  delta_AIC <- AIC[,1:ncol] - AIC[,ncol+1]
  relative_AIC <- exp(-0.5 * delta_AIC)
  relative_AIC[,ncol+1] <- rowSums(relative_AIC)
  
  AIC_weight <- relative_AIC[,1:ncol]/relative_AIC[,ncol+1]
  AIC[,ncol+1] <- NULL
  
  # Interleave raw AIC values with AIC weights
  # AIC <- cbind(AIC, AIC_weight)
  # idx <- rep(1:ncol, each = 2) + (0:1) * ncol 
  # AIC <- AIC[idx]
  # 
  # Stack raw AIC and AIC weights
  AIC <- cbind(AIC, AIC_weight)
  write.csv(AIC, filename)
}