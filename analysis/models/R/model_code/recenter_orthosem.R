# Generate recentered histograms (errors recentered on intrusion angles) and plot the recentered
# histograms
library(R.utils)
# Load in Experiment 1 Fits
load("~/git/sourcemem_online/analysis/models/R/experiment_1/output/2022-01-14.RData")
data <- read.csv('~/git/sourcemem_online/analysis/models/R/data/experiment_1.csv')
data <- data[data$valid_RT == TRUE,]


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
  recentered_data <- recenter_data(data)
  recentered_threshold <- recenter_model(data, sim_mix, 'Pure Guess')
  recentered_pure_intrusion <- recenter_model(data, sim_pure_intrusion, 'Pure Intrusion')
  recentered_intrusion <- recenter_model(data, sim_intrusion, 'Intrusion + Guess')
  recentered_temporal <- recenter_model(data, sim_temporal, 'Temporal')
  recentered_spatiotemporal <- recenter_model(data, sim_spatiotemporal, 'Spatiotemporal')
  recentered_all <- rbind(recentered_data, recentered_threshold, recentered_pure_intrusion,
                          recentered_intrusion, recentered_temporal, recentered_spatiotemporal)
  setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
  save(recentered_all, file = paste(toString(Sys.Date()), '_recentered_exp1_v2.RData', sep =""))
  return(recentered_all)
}
