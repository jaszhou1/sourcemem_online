# Generate recentered histograms (errors recentered on intrusion angles) and plot the recentered
# histograms
library(R.utils)
library(foreach)
library(doParallel)
# Load in Experiment 1 Fits
load("~/git/sourcemem_online/analysis/models/R/experiment_1/output/2022-04-05.RData")
data <- read.csv('~/git/sourcemem_online/analysis/models/R/data/experiment_1.csv')
data <- data[data$valid_RT == TRUE,]

sim_mix$model <- 'Pure Guess'
sim_pure_intrusion$model <- 'Pure Intrusion'
sim_intrusion$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_spatiotemporal$model <- 'Spatiotemporal'
sim_ortho$model <- 'Orthographic'
sim_semantic$model <- 'Semantic'
sim_all_x$model <- 'Four Factor (Multiplicative)'
sim_SxTpOxSe$model <- 'Four Factor (Additive)'
sim_data <- rbind(sim_mix, sim_pure_intrusion, sim_intrusion, sim_temporal, sim_spatiotemporal, 
                  sim_ortho, sim_semantic, sim_all_x, sim_SxTpOxSe)

n_intrusions <- 9

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

# Recenter Empirical Data
recenter_data <- function(data){
  recentered_errors <- data.frame(matrix(ncol=8,nrow=nrow(data)*n_intrusions, 
                                         dimnames=list(NULL, c('intruding_angle', 'offset', 'lag', 'spatial', 'orthographic', 'semantic', 'participant', 'type'))))
  idx <- 1
  for (i in 1:nrow(data)){
    this_trial <- data[i,]$present_trial
    this_response_angle <- data[i,]$response_angle
    this_target_angle <- data[i,]$target_angle
    this_intrusions <- data[i,14:22]
    temporal <- data[i,32:40]
    spatial <- data[i,42:50]
    orthographic <- data[i,51:59]
    semantic <- data[i,60:68]
    for(j in 1:n_intrusions){
      recentered_errors[idx,1] <- this_intrusions[[j]]
      recentered_errors[idx,2] <- angle_diff(this_response_angle, this_intrusions[[j]])
      recentered_errors[idx,3] <- temporal[[j]]
      recentered_errors[idx,4] <- spatial[[j]]
      recentered_errors[idx,5] <- orthographic[[j]]
      recentered_errors[idx,6] <- semantic[[j]]
      recentered_errors[idx,7] <- data[i, 'participant']
      recentered_errors[idx,8] <- 'data'
      idx <- idx + 1
    }
  }
  return(recentered_errors)
}

recenter_model <- function(data, model, model_string){
  data <- data[rep(seq_len(nrow(data)), each = nrow(model)/nrow(data)), ] # Replicate each row of data for simulated dataset (need semantic/ortho info)
  recentered_errors <- data.frame(matrix(ncol=8,nrow=nrow(data)*n_intrusions, 
                                         dimnames=list(NULL, c('intruding_angle', 'offset', 'lag', 'spatial', 'orthographic', 'semantic', 'participant', 'type'))))
  idx <- 1
  for (i in 1:nrow(data)){
    this_trial <- model[i,]$target_position
    this_response_angle <- as.numeric(model[i,]$simulated_response)
    this_target_angle <- as.numeric(model[i,]$target_angle)
    this_intrusions <- data[i,14:22]
    no_offset_angles <- model[i, 6:15]
    temporal <- data[i,32:40]
    spatial <- data[i,42:50]
    orthographic <- data[i,51:59]
    semantic <- data[i,60:68]
    for(j in 1:n_intrusions){
      recentered_errors[idx,1] <- this_intrusions[[j]]
      recentered_errors[idx,2] <- angle_diff(this_response_angle, this_intrusions[[j]])
      recentered_errors[idx,3] <- temporal[[j]]
      recentered_errors[idx,4] <- spatial[[j]]
      recentered_errors[idx,5] <- orthographic[[j]]
      recentered_errors[idx,6] <- semantic[[j]]
      recentered_errors[idx,7] <- model[i, 'participant']
      recentered_errors[idx,8] <- model_string
      idx <- idx + 1
    }
  }
  return(recentered_errors)
}


recenter_all <- function(){
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = unique(sim_data$model),
                 .combine = rbind) %dopar% {
                   recenter_model(data, sim_data[sim_data$model == i,], i)
                 }
  recentered_model <- as.data.frame(res)
  recentered_data <- recenter_data(data)
  recentered_all <- rbind(recentered_data, recentered_model)
  setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
  save(recentered_all, file = paste(toString(Sys.Date()), '_recentered_exp1_v2.RData', sep =""))
  return(recentered_all)
}
