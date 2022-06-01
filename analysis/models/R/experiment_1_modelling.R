## fit_models.R
##
## This is a modified version of fit_models.R in the top level folder to run a recoded intrusion model for bugfixing 

# Load required packages
library(CircStats)
library(circular)
library(DEoptim)
library(ggplot2)
library(extraDistr)
library(R.utils)

# Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('sourcemem_data_2021.csv')

# Models
setwd("~/git/sourcemem_online/analysis/models/R/model_code")

source('mixture_model.R')
source('three_component_model_precision.R')
source('pure_intrusion.R')
source('temporal_gradient_model_flat_guesses.R')

if(min(data$present_trial == 0)){
  data$present_trial <- data$present_trial + 1
}

if(min(data$block == -1)){
  data$block <- data$block + 1
}

# Exclude practice block
data <- data[data$block!= 0,]

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

##########################################
# Read in data
# Define recognition bands in data
data$recog_band <- ifelse(data$recog_rating >= 0 & data$recog_rating <= 3, 'Unrecognized',
                          ifelse(data$recog_rating >=4 & data$recog_rating <=5, 'Low',
                                 ifelse(data$recog_rating ==6, 'High','N/A')
                          )
)

seq_data <- data[data$is_sequential == TRUE,]
sim_data <- data[data$is_sequential == FALSE,]


################################################################################
## Fitting Functions

fit_mixture <- function(participant){
  
  # Assemble data that goes into the matrix
  in_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.001)
  upper <- c(250, 1)
  
  # Optimise
  this_fit <- DEoptim(mixture_model, lower, upper, control = DEoptim.control(itermax = 200), in_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

fit_pure_intrusion <- function(participant){
  # Assemble data that goes into the matrix
  in_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.01)
  upper <- c(250, 1)
  
  # Optimise
  this_fit <- DEoptim(pure_intrusion_model, lower, upper, control = DEoptim.control(itermax = 200), in_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

# fit_intrusion <- function(participant){
#   
#   # Assemble data that goes into the matrix
#   in_data <- data[(data$participant == participant),]
#   
#   # Parameter Boundaries
#   lower <- c(0.1, 0.01, 0.01)
#   upper <- c(250, 1, 1)
#   
#   # Optimise
#   this_fit <- DEoptim(intrusion_model, lower, upper, control = DEoptim.control(itermax = 200), in_data)
#   
#   # Calculate aic
#   aic <- get_aic(this_fit$optim$bestval, length(upper))
#   this_fit$optim$aic<-aic
#   fit <- this_fit$optim
#   # Pass out best fitting parameters
#   return(fit)
# }

fit_intrusion_precision <- function(participant){
  
  # Assemble data that goes into the matrix
  in_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0.05, 0.05)
  upper <- c(100, 50, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(intrusion_model_precision, lower, upper, control = DEoptim.control(itermax = 200), in_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}


# Participant Loops

fit_mixture_all <- function(){
  # Empty dataframe to store fitted parameters
  mix <- data.frame(
    participant = integer(),
    nLL = numeric(),
    aic = numeric(),
    prec = numeric(),
    beta = numeric(),
    stringsAsFactors = FALSE
  )
  
  participants <- unique(data$participant)
  
  for (i in 1:length(participants)){
    optim <- fit_mixture(participants[i])
    pest <- optim$bestmem
    this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1], pest[2])
    mix[nrow(mix)+1, ] <- this_fit
  }
  
  mix$prec <- as.numeric(mix$prec)
  mix$beta <- as.numeric(mix$beta)
  return(mix)
}

fit_pure_intrusion_all <- function(){
  # Empty dataframe to store fitted parameters
  pure_int <- data.frame(
    participant = integer(),
    nLL = numeric(),
    aic = numeric(),
    prec = numeric(),
    gamma = numeric(),
    stringsAsFactors = FALSE
  )
  
  participants <- unique(data$participant)
  
  for (i in 1:length(participants)){
    optim <- fit_pure_intrusion(participants[i])
    pest <- optim$bestmem
    this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1], pest[2])
    pure_int[nrow(pure_int)+1, ] <- this_fit
  }
  
  pure_int$prec <- as.numeric(pure_int$prec)
  pure_int$gamma <- as.numeric(pure_int$gamma)
  return(pure_int)
}

# fit_intrusion_all <- function(){
#   # Empty dataframe to store fitted parameters
#   fit <- data.frame(
#     participant = integer(),
#     nLL = numeric(),
#     aic = numeric(),
#     prec = numeric(),
#     gamma = numeric(), 
#     beta = numeric(), 
#     stringsAsFactors = FALSE
#   )
#   
#   participants <- unique(data$participant)
#   
#   for (i in 1:length(participants)){
#     optim <- fit_intrusion(participants[i])
#     pest <- optim$bestmem
#     this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1], pest[2], pest[3])
#     fit[nrow(fit)+1, ] <- this_fit
#   }
#   
#   fit$prec <- as.numeric(fit$prec)
#   fit$gamma <- as.numeric(fit$gamma)
#   fit$beta <- as.numeric(fit$beta)
#   return(fit)
# }

fit_intrusion_prec_all <- function(){
  # Empty dataframe to store fitted parameters
  fit <- data.frame(
    participant = integer(),
    nLL = numeric(),
    aic = numeric(),
    prec1 = numeric(),
    prec2 = numeric(),
    gamma = numeric(), # this becomes the area under a normal distribution, p(intrusion)
    beta = numeric(), # this becomes the additional area on top of w2, p(guess)
    stringsAsFactors = FALSE
  )
  
  participants <- unique(data$participant)
  
  for (i in 1:length(participants)){
    optim <- fit_intrusion_precision(participants[i])
    pest <- optim$bestmem
    this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1], pest[2], pest[3], pest[4])
    fit[nrow(fit)+1, ] <- this_fit
  }
  
  fit$prec1 <- as.numeric(fit$prec1)
  fit$prec2 <- as.numeric(fit$prec2)
  fit$gamma <- as.numeric(fit$gamma)
  fit$beta <- as.numeric(fit$beta)
  return(fit)
}

# Simulate datasets for plotting
simulate_mixture_dataset <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_prec <- res[(res$participant == i),]$prec # Precision of memory distribution
    this_beta <- res[(res$participant == i),]$beta
    this_simulated_data <- simulate_mixture(i, this_prec, this_beta, this_data)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

simulate_pure_intrusion_dataset <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_prec <- res[(res$participant == i),]$prec # Precision of memory distribution
    this_gamma <- res[(res$participant == i),]$gamma
    this_simulated_data <- simulate_pure_intrusion(i, this_prec, this_gamma, this_data)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

# simulate_intrusion_dataset <- function(data, res){
#   simulated_data <- data.frame()
#   participants <- unique(data$participant)
#   for (i in participants){
#     this_data <- data[data$participant == i,]
#     this_prec <- res[(res$participant == i),]$prec # Precision of memory distribution
#     this_gamma <- res[(res$participant == i),]$gamma # Proportion of intrusions
#     this_beta <- res[(res$participant == i),]$beta
#     this_simulated_data <- sim_intrusion(i, this_prec, this_gamma, this_beta, this_data)
#     simulated_data <- rbind(simulated_data, this_simulated_data)
#   }
#   return(simulated_data)
# }

simulate_intprec_dataset <- function(data, res){
  simulated_data <- data.frame()
  participants <- unique(data$participant)
  for (i in participants){
    this_data <- data[data$participant == i,]
    this_prec1 <- res[(res$participant == i),]$prec1 # Precision of memory distribution
    this_prec2 <- res[(res$participant == i),]$prec2 # Proportion of uniform guesses
    this_gamma <- res[(res$participant == i),]$gamma # Proportion of intrusions
    this_beta <- res[(res$participant == i),]$beta
    this_simulated_data <- sim_intrusion_precision(i, this_prec1, this_prec2, this_gamma, this_beta, this_data)
    simulated_data <- rbind(simulated_data, this_simulated_data)
  }
  return(simulated_data)
}

## Temporal Gradient Model
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
  temporal <- as.data.frame(res)
  write.csv(temporal, paste(toString(Sys.Date()), '_temporal_pest.csv', sep =""))
  sim_temp <- simulate_temporal_dataset(data, res)
  save(temporal, sim_temp, file = paste(toString(Sys.Date()), '_temporal_pest.RData', sep =""))
  return(res)
}

simulate_temporal_dataset <- function(data, res){
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



fit_space_x_time <- function(participant){
  
  # Assemble data that goes into the matrix
  this_data <- data[(data$participant == participant),]
  
  # Parameter Boundaries
  lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0)
  upper <- c(50, 50, 2, 1, 5, 5, 1, 1, 1)
  
  # Optimise
  this_fit <- DEoptim(spatiotemporal_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
  
  # Calculate aic
  aic <- get_aic(this_fit$optim$bestval, length(upper))
  this_fit$optim$aic<-aic
  fit <- this_fit$optim
  # Pass out best fitting parameters
  return(fit)
}

simulate_dataset_space_x_time <- function(data, res){
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
  spatiotemporal <- as.data.frame(res)
  write.csv(spatiotemporal, paste(toString(Sys.Date()), '_exp2_spatiotemporal_pest.csv', sep =""))
  sim_SxT <- simulate_dataset_space_x_time(data, spatiotemporal)
  #save(res, sim_SxT, file = paste(toString(Sys.Date()), '_space_x_time_pest.RData', sep =""))
  save(spatiotemporal, sim_SxT, file = paste(toString(Sys.Date()), '_exp2_spatiotemporal.RData', sep =""))
  return(spatiotemporal)
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

# fit_space_x_time_plus_ortho_plus_sem <- function(participant){
#   # Assemble data that goes into the matrix
#   this_data <- data[(data$participant == participant),]
#   
#   # Parameter Boundaries
#   lower <- c(0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#   upper <- c(40, 40, 1, 1, 5, 5, 0.8, 1, 1, 1, 1, 1)
#   
#   # Optimise
#   this_fit <- DEoptim(spacextime_ortho_plus_sem_model, lower, upper, control = DEoptim.control(itermax = 200), this_data)
#   
#   # Calculate aic
#   aic <- get_aic(this_fit$optim$bestval, length(upper))
#   this_fit$optim$aic<-aic
#   fit <- this_fit$optim
#   # Pass out best fitting parameters
#   return(fit)
# }

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
  ortho <- as.data.frame(res)
  write.csv(ortho, paste(toString(Sys.Date()), '_exp2_ortho_pest.csv', sep =""))
  sim_ortho <- simulate_dataset_ortho(data, ortho)
  #save(ortho, sim_ortho, file = paste(toString(Sys.Date()), '_ortho.RData', sep =""))
  save(ortho, sim_ortho, file = paste(toString(Sys.Date()), '_exp2_ortho.RData', sep =""))
  return(ortho)
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
  sem <- as.data.frame(res)
  write.csv(sem, paste(toString(Sys.Date()), '_exp2_semantic_pest.csv', sep =""))
  sim_semantic <- simulate_dataset_semantic(data, sem)
  #save(sem, sim_semantic, file = paste(toString(Sys.Date()), '_semantic.RData', sep =""))
  save(sem, sim_semantic, file = paste(toString(Sys.Date()), '_exp2_semantic.RData', sep =""))
  return(sem)
}

# fit_space_x_time_plus_ortho_plus_sem_all <- function(){
#   participants <- unique(data$participant)
#   cl <- makeForkCluster((detectCores() - 1))
#   registerDoParallel(cl)
#   res = foreach (i = 1:length(participants),
#                  .combine = rbind) %dopar% {
#                    optim <- fit_space_x_time_plus_ortho_plus_sem(participants[i])
#                    pest <- optim$bestmem
#                    this_fit <- c(participants[i], optim$bestval, optim$aic, pest[1:12])
#                    return(this_fit)
#                  }
#   colnames(res) <- c('participant','nLL','aic','prec1','prec2','gamma', 'kappa', 
#                      'lambda_b', 'lambda_f', 'beta', 'zeta', 'rho', 'chi', 'psi', 'iota')
#   res <- as.data.frame(res)
#   write.csv(res, paste(toString(Sys.Date()), '_SxTpOpSe_pest.csv', sep =""))
#   sim_SxTpOpSe <- simulate_dataset_space_x_time_plus_ortho_plus_sem(data, res)
#   save(res, sim_SxTpOpSe, file = paste(toString(Sys.Date()), '_SxTpOpSe.RData', sep =""))
#   return(res)
# }

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
  add <- as.data.frame(res)
  write.csv(add, paste(toString(Sys.Date()), '_exp2_add_pest.csv', sep =""))
  sim_SxTpOxSe <- simulate_dataset_space_x_time_plus_ortho_x_sem(data, res)
  #save(res, sim_SxTpOxSe, file = paste(toString(Sys.Date()), '_SxTpOxSe.RData', sep =""))
  save(add, sim_SxTpOxSe, file = paste(toString(Sys.Date()), '_exp2_add.RData', sep =""))
  return(add)
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
  multi <- as.data.frame(res)
  write.csv(multi, paste(toString(Sys.Date()), '_exp2_multi_pest.csv', sep =""))
  sim_multi <- simulate_dataset_all_x(data, res)
  #save(res, sim_all_x, file = paste(toString(Sys.Date()), '_all_x.RData', sep =""))
  save(multi, sim_multi, file = paste(toString(Sys.Date()), '_exp2_multi.RData', sep =""))
  return(multi)
}
# Fit models and simulate datasets from each model for plotting
run_fits <- function(){
  mix <- fit_mixture_all()
  write.csv(mix, paste(toString(Sys.Date()), '_mix_pest.csv', sep =""))
  sim_mix <- simulate_mixture_dataset(data, mix)
  
  pure_int <- fit_pure_intrusion_all()
  write.csv(pure_int, paste(toString(Sys.Date()), 'pure_int_pest.csv', sep =""))
  sim_pure_intrusion <- simulate_pure_intrusion_dataset(data, pure_int)

  int_prec <- fit_intrusion_prec_all()
  write.csv(int_prec, paste(toString(Sys.Date()), '_flat_intrusion_pest.csv', sep =""))
  sim_intprec <- simulate_intprec_dataset(data, int_prec)
  
  temporal <- fit_temporal_all()
  sim_temporal <- simulate_temporal_dataset(data, temporal)
  
  spatiotemporal <- fit_space_x_time_all()
  sim_spatiotemporal <- simulate_dataset_space_x_time(data, spatiotemporal)
  
  ortho <- fit_ortho_all()
  sim_ortho <- simulate_dataset_ortho(data, ortho)
  
  semantic <- fit_semantic_all()
  sim_sematic <- simulate_dataset_semantic(data, semantic)
  
  add <- fit_space_x_time_plus_ortho_x_sem_all()
  sim_add <- simulate_dataset_space_x_time_plus_ortho_x_sem(data, add)
  
  multi <- fit_all_x_all()
  sim_multi <- simulate_dataset_all_x(data, multi)
  
  filename <- paste(toString(Sys.Date()), '.RData', sep = "")
  save.image(file = filename)
}
