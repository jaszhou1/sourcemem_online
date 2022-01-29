setwd("~/git/sourcemem_online/analysis/models/R")

# Load required packages
library(CircStats)
library(circular)
library(DEoptim)
library(ggplot2)
library(extraDistr)
library(R.utils)

# Load in simulated datasets from each model
load("~/git/sourcemem_online/analysis/models/R/model_recovery_data.RData")

# Load in models
setwd("~/git/sourcemem_online/analysis/models/R/model_code")

source('mixture_model.R')
source('pure_intrusion.R')
source('three_component_model_precision.R')
source('temporal_gradient_model_flat_guesses.R')
source('spatiotemporal_gradient_model.R')

models <- c('Pure Guess', 'Pure Intrusion', 'Intrusion + Guess',
            'Temporal Gradient', 'Spatiotemporal Gradient')

# Function to calculate aic 
get_aic <- function(L, n_params){
  aic <- 2*L + 2*n_params
  return(aic)
}

cross_fit <- function(gen_model, fit_model){
  fit <- data.frame(matrix(ncol = 3, nrow = 1))
  colnames(fit) <- c("aic", "gen_model", "fit_model")
  # Get the required simulated dataset
  if(gen_model == 'Pure Guess'){
    this_sim_data <- sim_mix
  } else if (gen_model == 'Pure Intrusion'){
    this_sim_data <- sim_pure_intrusion
  } else if (gen_model == 'Intrusion + Guess'){
    this_sim_data <- sim_intrusion
  } else if (gen_model == 'Temporal Gradient'){
    this_sim_data <- sim_temporal
  } else if (gen_model == 'Spatiotemporal Gradient'){
    this_sim_data <- sim_spatiotemporal
  } else {
    print("invalid generative model string")
    stop()
  }
  
  # Define the model used to fit the simulated data, and the parameter bounds
  if(fit_model == 'Pure Guess'){
    this_model <- mixture_model
    lower <- c(0.1, 0.001)
    upper <- c(250, 1)
  } else if (fit_model == 'Pure Intrusion'){
    this_model <- pure_intrusion_model
    lower <- c(0.1, 0.01)
    upper <- c(250, 1)
  } else if (fit_model == 'Intrusion + Guess'){
    this_model <- intrusion_model_precision
    lower <- c(0.1, 0.1, 0.05, 0.05)
    upper <- c(100, 50, 1, 1)
  } else if (fit_model == 'Temporal Gradient'){
    this_model <- temporal_model
    lower <- c(0.1, 0.1, 0, 0, 0, 0, 0)
    upper <- c(50, 50, 1, 1, 5, 5, 1)
  } else if (fit_model == 'Spatiotemporal Gradient'){
    this_model <- spatiotemporal_model
    lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0)
    upper <- c(50, 50, 2, 1, 5, 5, 1, 1, 1)
  } else {
    print("invalid fitting model string")
    stop()
  }
  
  this_fit <- DEoptim(this_model, lower, upper, control = DEoptim.control(itermax = 200), this_sim_data)
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  fit$aic<-aic
  fit$gen_model <- gen_model
  fit$fit_model <- fit_model
  return(fit)
}

model_recovery <- function(){
  res <- data.frame()
  for(i in models){
    cl <- makeForkCluster(5)
    registerDoParallel(cl)
    this_model = foreach (j = models,
                     .combine = rbind) %dopar% {
      this_fit <- cross_fit(i, j)
      return(this_fit)
      }
    res <- rbind(res, this_model)
  }
  return(res)
}