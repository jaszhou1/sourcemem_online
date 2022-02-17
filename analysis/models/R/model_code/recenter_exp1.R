# Generate recentered histograms (errors recentered on intrusion angles) and plot the recentered
# histograms

# Load in Experiment 1 Fits
load("~/git/sourcemem_online/analysis/models/R/experiment_1/output/2022-01-14.RData")

# Filter is the number of positions away from the target we are allowing intrusions to come from
n_intrusions <- 9

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

# Recenter Empirical Data
recenter_data <- function(filter, data){
  recentered_errors <- data.frame()
  idx <- 1
  for (i in 1:nrow(data)){
    this_trial <- data[i,]$present_trial
    this_response_angle <- data[i,]$response_angle
    this_target_angle <- data[i,]$target_angle
    this_block_angles <- data[i,14:22]
    no_offset_angles <- insert(as.vector(t(this_block_angles[1:9])), ats = this_trial, values = this_target_angle)
    
    for (j in 1:filter){
      if (this_trial + filter <= n_intrusions){
        this_intrusion <- no_offset_angles[[this_trial + j]]
        this_offset <- angle_diff(this_response_angle, this_intrusion)
        recentered_errors[idx,1] <- this_offset
        recentered_errors[idx,2] <- 'forwards'
        recentered_errors[idx,3] <- data[i,]$participant
        idx <- idx + 1
      }
      if (this_trial - filter > 0){
        this_intrusion <- no_offset_angles[[this_trial - j]]
        this_offset <- angle_diff(this_response_angle, this_intrusion)
        recentered_errors[idx,1] <- this_offset
        recentered_errors[idx,2] <- 'backwards'
        recentered_errors[idx,3] <- data[i,]$participant
        idx <- idx + 1
      }
    }
  }
  colnames(recentered_errors) <- c('error', 'direction')
  recentered_errors$model <- 'data'
  recentered_errors$filter <- filter
  return(recentered_errors)
}

recenter_model <- function(filter, this_data, model){
  sim_errors <- data.frame()
  idx <- 1
  for (i in 1:nrow(this_data)){
    this_trial <- as.numeric(this_data[i,]$target_position)
    this_response_angle <- as.numeric(this_data[i,]$simulated_response)
    this_intrusions <- this_data[i, 6:15]
    for (j in 1:filter){
      if (this_trial + filter <= n_intrusions){
        this_intrusion <- as.numeric(this_intrusions[[this_trial+j]])
        this_offset <- angle_diff(this_response_angle, this_intrusion)
        sim_errors[idx,1] <- this_offset
        sim_errors[idx,2] <- 'forwards'
        sim_errors[idx,3] <- this_data[i,]$participant
        idx <- idx + 1
      }
      if (this_trial - filter > 0){
        this_intrusion <- as.numeric(this_intrusions[[this_trial-j]])
        this_offset <- angle_diff(this_response_angle, this_intrusion)
        sim_errors[idx,1] <- this_offset
        sim_errors[idx,2] <- 'backwards'
        sim_errors[idx,3] <- this_data[i,]$participant
        idx <- idx + 1
      }
    }
  }
  colnames(sim_errors) <- c('error', 'direction')
  sim_errors$model <- model
  sim_errors$filter <- filter
  return(sim_errors)
}

generate_recentered_dataset <- function(data){
  recentered_dataset <- data.frame()
  for(i in 1:3){
    # Filter 1 - 3
    this_recenter_data <- recenter_data(i, data)
    recentered_dataset <- rbind(recentered_dataset, this_recenter_data)  
  }
  return(recentered_dataset)
}

generate_recentered_model <- function(data, model_string){
  recentered_dataset <- data.frame()
  for(i in 1:3){
    # Filter 1 - 3
    this_recenter_data <- recenter_model(i, data, model_string)
    recentered_dataset <- rbind(recentered_dataset, this_recenter_data)  
  }
  return(recentered_dataset)
}

recenter_all <- function(){
  recentered_data <- generate_recentered_dataset(data)
  recentered_threshold <- generate_recentered_model(sim_mix, 'Pure Guess')
  recentered_pure_intrusion <- generate_recentered_model(sim_pure_intrusion, 'Pure Intrusion')
  recentered_intrusion <- generate_recentered_model(sim_intrusion, 'Intrusion + Guess')
  recentered_temporal <- generate_recentered_model(sim_temporal, 'Temporal Gradient')
  recentered_spatiotemporal <- generate_recentered_model(sim_spatiotemporal, 'Spatiotemporal Gradient')
  recentered_all <- rbind(recentered_data, recentered_threshold, recentered_pure_intrusion,
                          recentered_intrusion, recentered_temporal, recentered_spatiotemporal)
  save(recentered_all, file = paste(toString(Sys.Date()), '_recentered_exp1.RData', sep =""))
  return(recentered_all)
}
