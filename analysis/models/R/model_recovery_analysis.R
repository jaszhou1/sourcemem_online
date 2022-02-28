# Quick and dirty script to cobble together the parallel outputs from model cross fitting 
# and see how many times the correct model is recovered.

setwd("~/git/sourcemem_online/analysis/models/R/model_recovery_crossfits")
load("2022-02-24_P1_crossfit.RData")
P1 <- crossfit

load("2022-02-24_P2_crossfit.RData")
P2 <- crossfit

load("2022-02-24_P3_crossfit.RData")
P3 <- crossfit

load("2022-02-24_P4_crossfit.RData")
P4 <- crossfit

load("2022-02-24_P5_crossfit.RData")
P5 <- crossfit
crossfits <- rbind(P1,P2,P3,P4,P5)

PARTICIPANTS <- unique(crossfits$participant)
MODELS <- unique(crossfits$gen_model)

for(i in 1:nrow(crossfits)){
  this_participant <- crossfits[i, 1]
  this_gen_model <- crossfits[i, 2]
  this_crossfit <- crossfits[(crossfits$participant == this_participant) & 
                               (crossfits$gen_model == this_gen_model),]
  crossfits[i, 5] <- crossfits[i, 4] - min(this_crossfit$aic)
}

colnames(crossfits)[5] <- 'delta_AIC'

model_recovery_prop <- function(this_model){
  # Find the number of times the generating model matches the fitting model
  x <- nrow(this_model[this_model$gen_model == this_model$fit_model,])
  # Find the number of times the generating model was preferred when fit
  y <- nrow(this_model[(this_model$gen_model == this_model$fit_model) & (this_model$delta_AIC == 0),])
  return(y/x)
}
  