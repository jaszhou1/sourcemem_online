## fit_spatiotemporal_model.R

## This is the top level script to fit and plot simulated data from the temporal gradient model
# Load required packages
library(CircStats)
library(circular)
library(DEoptim)
library(ggplot2)
library(extraDistr)
library(foreach)
library(doParallel)
library(plyr)
library(R.utils)
library(statip)

# Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('sourcemem_data_2021.csv')

# Models
setwd("~/git/sourcemem_online/analysis/models/R/model_code")
source('spatiotemporal_gradient_model.R')

# Add one to each present trial, if the data is 0 indexed
if(min(data$present_trial == 0)){
  data$present_trial <- data$present_trial + 1
}

if(min(data$block == -1)){
  data$block <- data$block + 1
}

# Exclude practice block
data <- data[data$block != 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

# Function to compute angular difference

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

# Function to calculate aic 
get_aic <- function(L, n_params){
  aic <- 2*L + 2*n_params
  return(aic)
}

# Append spatial distances to the dataset
source('spatial_distance.R')
data <- append_distances(data)

#################################################################
# Fitting Functions
#################################################################
fit_spatiotemporal_model <- function(participant){
  
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(50, 50, 2, 1, 5, 5, 1, 1, 1)
  
  # Set some starting parameters
  
  
  # Optimise
  this_fit <- DEoptim(spatiotemporal_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_spatiotemporal_model(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:9])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 'lambda_b', "lambda_f", "beta", "zeta", "rho")
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_spatiotemporal_pest.csv', sep =""))
  sim_data <- simulate_dataset(data, res)
  save(res, sim_data, file = paste(toString(Sys.Date()), '_spatiotemporal.RData', sep =""))
  return(res)
}


## Simulate and plot the fitted model

simulate_dataset <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:12]
    this_simulated_data <- simulate_spatiotemporal_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}
