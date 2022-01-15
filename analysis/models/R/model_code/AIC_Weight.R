setwd("~/git/sourcemem_online/analysis/models/R/model_code")
AIC <- read.csv('Exp1_AIC.csv')
for(i in 1:nrow(AIC)){
  AIC[i, 6] <- min(AIC[i, 1:5])
}

delta_AIC <- AIC[,1:5] - AIC[,6]
relative_AIC <- exp( -0.5 * delta_AIC)
relative_AIC[,6] <- rowSums(relative_AIC)

AIC_weight <- relative_AIC[,1:5]/relative_AIC[,6]

write.csv(AIC_weight, file = 'AIC_Weight.csv')