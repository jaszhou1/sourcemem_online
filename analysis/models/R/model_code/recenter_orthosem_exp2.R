# Generate recentered histograms (errors recentered on intrusion angles) and plot the recentered
# histograms
library(R.utils)

# Load in Experiment 2 Fits
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/exp_2_sim_data_updated.RData")

# Fix some column name discrepency
sim_flat_intrusion$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_SxT$model <- 'Spatiotemporal'
sim_ortho$model <- 'Orthographic'
sim_semantic$model <- 'Semantic'
sim_all_x$model <- 'Four Factor (Multiplicative)'
sim_SxTpOxSe$model <- 'Four Factor (Additive)'
sim_data <- rbind(sim_flat_intrusion, sim_temporal, sim_SxT, sim_ortho, sim_semantic,
                  sim_all_x, sim_SxTpOxSe)


data <- read.csv('~/git/sourcemem_online/analysis/models/R/data/experiment_2.csv')
# Exclude practice block
data <- data[data$block != 0,]

# Exclude first session as a practice sessions
data <- data[data$session != 1,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

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


recenter_all <- function(data, sim_data){
  cl <- makeForkCluster((detectCores() - 1))
  registerDoParallel(cl)
  res = foreach (i = unique(sim_data$model),
                 .combine = rbind) %dopar% {
                   recenter_model(data, sim_data[sim_data$model == i,], i)
                 }
  recentered_model <- as.data.frame(res)
  recentered_data <- recenter_data(data)
  recentered_all <- rbind(recentered_data, recentered_model)
  save(recentered_all, file = paste(toString(Sys.Date()), '_recentered_exp2_v2.RData', sep =""))
  return(recentered_all)
}
