# Quick and dirty script to cobble together the parallel outputs from model cross fitting 
# and see how many times the correct model is recovered.

setwd("~/git/sourcemem_online/analysis/models/R/model_recovery_crossfits")
load("2022-03-03_P1_crossfit.RData")
P1 <- crossfit

load("2022-03-03_P2_crossfit.RData")
P2 <- crossfit

load("2022-03-03_P3_crossfit.RData")
P3 <- crossfit

load("2022-03-03_P4_crossfit.RData")
P4 <- crossfit

load("2022-03-02_P5_crossfit.RData")
P5 <- crossfit
crossfits <- rbind(P1,P2,P3,P4,P5)

# ## Temporary chunk to flag each run number
# run_idx <- rep(1:3, each = 4)
# run_idx <- rep(run_idx, 20)
# crossfits$n_run <- run_idx
# ## End temporary part

PARTICIPANTS <- unique(crossfits$participant)
MODELS <- unique(crossfits$gen_model)

for(i in 1:nrow(crossfits)){
  this_participant <- crossfits[i, 1]
  this_gen_model <- crossfits[i, 2]
  this_run <- crossfits[i, 5]
  this_crossfit <- crossfits[(crossfits$participant == this_participant) & 
                               (crossfits$gen_model == this_gen_model) &
                               crossfits$run == this_run,]
  crossfits[i, 6] <- crossfits[i, 4] - min(this_crossfit$aic)
}

colnames(crossfits)[6] <- 'delta_AIC'

model_recovery_prop <- function(this_model){
  # Find the number of times the generating model matches the fitting model
  x <- nrow(this_model[this_model$gen_model == this_model$fit_model,])
  # Find the number of times the generating model was preferred when fit
  y <- nrow(this_model[(this_model$gen_model == this_model$fit_model) & (this_model$delta_AIC == 0),])
  return(y/x)
}
  