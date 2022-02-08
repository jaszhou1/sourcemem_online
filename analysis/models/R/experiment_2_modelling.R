#fit_gradient_models.R
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
data <- read.csv('experiment_2.csv')

# Exclude practice block
data <- data[data$block != 0,]

# Exclude first session as a practice sessions
data <- data[data$session != 1,]

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

shepard_similarity <- function(x, k){
  x <- exp(-k * x)
  return(x)
}

convert_orthographic_similarity <- function(x, length){
  similarity <- (length - x)/length
  return(similarity)
}

# Load in Models
setwd("~/git/sourcemem_online/analysis/models/R/model_code")

source('spatiotemporal_gradient_model.R') # Sorry for inconsistent naming. spatiotemporal = space_x_time
source('spacextime_ortho_model.R')
source('spacextime_sem_model.R')
source('spacextime_ortho_plus_sem_model.R')
source('spacextime_plus_orthoxsem_model.R')
source('all_x_model.R')

## Individual fitting functions for each model

fit_space_x_time <- function(participant){
  
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(50, 50, 2, 1, 5, 5, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(space_x_time_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_ortho <- function(participant){
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(ortho_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_semantic <- function(participant){
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(semantic_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_space_x_time_plus_ortho_plus_sem <- function(participant){
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(spacextime_ortho_plus_sem_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_space_x_time_plus_ortho_x_sem <- function(participant){
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(spacextime_plus_orthoxsem_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)

  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_all_x <- function(participant){
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(all_x_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

## Simulate datasets

simulate_dataset_space_x_time <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:12]
    this_simulated_data <- simulate_space_x_time_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_dataset_ortho <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:13]
    this_simulated_data <- simulate_ortho_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_dataset_semantic <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:13]
    this_simulated_data <- simulate_semantic_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_dataset_space_x_time_plus_ortho_plus_sem <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:14]
    this_simulated_data <- simulate_spacextime_ortho_plus_sem_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_dataset_space_x_time_plus_ortho_x_sem <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:14]
    this_simulated_data <- simulate_spacextime_plus_orthoxsem_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_dataset_all_x <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_pest <- res[(res$participant == i),4:14]
    this_simulated_data <- simulate_all_x_model(i, this_data, this_pest)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}
## Participant Loops

fit_space_x_time_all <- function(is_parallel){
  if(missing(is_parallel)){
    is_parallel <- TRUE
  }
  participants <- unique(data$participant)
  
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_space_x_time(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:9])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 'lambda_b', "lambda_f", "beta", "zeta", "rho")
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_space_x_time_pest.csv', sep =""))
  sim_SxT <- simulate_dataset_space_x_time(data, res)
  save(res, sim_SxT, file = paste(toString(Sys.Date()), '_sspace_x_time_pest.RData', sep =""))
  return(res)
}

fit_ortho_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_ortho(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:10])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
                     'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi')
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_ortho_pest.csv', sep =""))
  sim_ortho <- simulate_dataset_ortho(data, res)
  save(res, sim_ortho, file = paste(toString(Sys.Date()), '_ortho.RData', sep =""))
  return(res)
}

fit_semantic_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_semantic(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:10])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
                     'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi')
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_semantic_pest.csv', sep =""))
  sim_semantic <- simulate_dataset_semantic(data, res)
  save(res, sim_semantic, file = paste(toString(Sys.Date()), '_semantic.RData', sep =""))
  return(res)
}

fit_space_x_time_plus_ortho_plus_sem_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_space_x_time_plus_ortho_plus_sem(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:11])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
                     'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi', 'psi')
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_SxTpOpSe_pest.csv', sep =""))
  sim_SxTpOpSe <- simulate_dataset_space_x_time_plus_ortho_plus_sem(data, res)
  save(res, sim_SxTpOpSe, file = paste(toString(Sys.Date()), '_SxTpOpSe.RData', sep =""))
  return(res)
}

fit_space_x_time_plus_ortho_x_sem_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_space_x_time_plus_ortho_x_sem(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:11])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
                     'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi', 'psi')
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_SxTpOxSe_pest.csv', sep =""))
  sim_SxTpOxSe <- simulate_dataset_space_x_time_plus_ortho_x_sem(data, res)
  save(res, sim_SxTpOxSe, file = paste(toString(Sys.Date()), '_SxTpOxSe.RData', sep =""))
  return(res)
}

fit_all_x_all <- function(){
  participants <- unique(data$participant)
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = 1:length(participants),
                 .combine = rbind) %dopar% {
                   optim <- fit_all_x(participants[i])
                   pest <- optim$bestmem
                   this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:11])
                   return(this_fit)
                 }
  colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
                     'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi', 'psi')
  res <- as.data.frame(res)
  write.csv(res, paste(toString(Sys.Date()), '_all_x_pest.csv', sep =""))
  sim_all_x <- simulate_dataset_all_x(data, res)
  save(res, sim_all_x, file = paste(toString(Sys.Date()), '_all_x.RData', sep =""))
  return(res)
}
## Top-level function to fit all models and save the parameter estimates and simulated datasets
fit_all_models <- function(){
  #SxT <- fit_space_x_time_all()
  #SxTpOpSe <- fit_space_x_time_plus_ortho_plus_sem_all()
  ortho <- fit_ortho_all()
  semantic <- fit_semantic_all()
  SxTpOxSe <- fit_space_x_time_plus_ortho_x_sem_all()
  all_x <- fit_all_x_all()
}
