## fit_temporal_model.R

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
source('temporal_gradient_model_flat_guesses.R')

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

#################################################################
# Fitting Functions
#################################################################
fit_temporal <- function(participant){
  
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0)
  upper <- c(50, 50, 1, 1, 5, 5, 1)
  
  # Set some starting parameters
  
  
  # Optimise
  this_fit <- DEoptim(temporal_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_temporal_all <- function(is_parallel){
  if(missing(is_parallel)){
    is_parallel <- TRUE
  }
  participants <- unique(data$participant)
  
  if(is_parallel == FALSE){
    # Empty dataframe to store fitted parameters
    res <- data.frame(
      participant = integer(),
      nLL = numeric(),
      aic = numeric(),
      prec1 = numeric(),
      prec2 = numeric(),
      gamma = numeric(),
      kappa = numeric(),
      lambda_b = numeric(),
      lambda_f = numeric(),
      beta = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:length(participants)){
      optim <- fit_temporal(participants[i])
      pest <- optim$bestmem
      this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1], pest[2], pest[3], pest[4], pest[5], pest[6], pest[7])
      res[nrow(res)+1, ] <- this_fit
    }
    
    res$prec1 <- as.numeric(res$prec1)
    res$prec2 <- as.numeric(res$prec2)
    res$gamma <- as.numeric(res$gamma)
    res$kappa <- as.numeric(res$kappa)
    res$lambda_b <- as.numeric(res$lambda_b)
    res$lambda_f <- as.numeric(res$lambda_f)
    res$beta <- as.numeric(res$beta)
  } else {
    cl <- makeForkCluster((detectCores() - 1))
    registerDoParallel(cl)
    res = foreach (i = 1:length(participants),
                   .combine = rbind) %dopar% {
                     source('temporal_gradient_model_flat_guesses.R')     
                     optim <- fit_temporal(participants[i])
                     pest <- optim$bestmem
                     this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:7])
                     return(this_fit)
                   }
    colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 'lambda_b', "lambda_f", "beta")
    res <- as.data.frame(res)
  }
  write.csv(res, paste(toString(Sys.Date()), '_temporal_pest.csv', sep =""))
  sim_data <- simulate_dataset(data, res)
  save(res, sim_data, file = paste(toString(Sys.Date()), '_temporal_pest.RData', sep =""))
  return(res)
}


## Simulate and plot the fitted model

simulate_dataset <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:10]
    this_simulated_data <- simulate_temporal_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

plot_model <- function(participant){
  this_data <- data[data$participant == participant,]
  this_pest <- res[(res$participant == participant),4:10]
  
  this_observed_error <- this_data$response_error
  simulated_data <- simulate_temporal_model(participant, this_data, this_pest)
  this_simulated_error <- as.numeric(simulated_data$simulated_error)
  
  plot_title <- sprintf('Participant %i', participant)
  
  ggplot() +
    geom_histogram(aes(x=this_observed_error, y=..density..), alpha=0.6,
                   position="identity", lwd=0.2, bins = 30) +
    geom_histogram(aes(x=this_simulated_error, y=..density.., fill = 'red', colour='red'), alpha=0.2,
                   position="identity", lwd=1, bins = 30) +
    labs(title= plot_title,
         x ="Response Error", y = "Density") +
    guides(color = FALSE, fill = FALSE)
  
  # Save plot
  filename <- sprintf('Participant_%i.png', participant)
  ggsave(filename)
}

plot_all <- function(){
  participants <- unique(data$participant)
  for (i in 1:length(participants)){
    plot_model(i)
  }
}

