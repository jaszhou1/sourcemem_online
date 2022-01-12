## pure_intrusion.R
##
## Only intrusions, strong prediction of no uniform guesses


pure_intrusion_model <- function(params, in_data){
  n_intrusions <- 9
  
  # Get parameters
  prec <- params[1] # Precision of Memory Distribution
  gamma <- params[2] # Proportion of Intrusions
  
  # Empty dataframe to store likelihoods for each trial
  likelihoods <- data.frame(matrix(ncol=12,nrow=nrow(in_data), dimnames=list(NULL, c('target', 'intrusion_1','intrusion_2',
                                                                                     'intrusion_3','intrusion_4','intrusion_5',
                                                                                     'intrusion_6','intrusion_7','intrusion_8',
                                                                                     'intrusion_9', 'guess', 'weighted_intrusion'))))
  # Memory component
  likelihoods$target <- dvm(in_data$response_error, 0, prec)
  
  # Intrusion component (There must be a better way to do this? like likelihoods[2:10] or something)
  likelihoods$intrusion_1 <- dvm(in_data$response_error, in_data$intrusion_offset1, prec)
  likelihoods$intrusion_2 <- dvm(in_data$response_error, in_data$intrusion_offset2, prec)
  likelihoods$intrusion_3 <- dvm(in_data$response_error, in_data$intrusion_offset3, prec)
  likelihoods$intrusion_4 <- dvm(in_data$response_error, in_data$intrusion_offset4, prec)
  likelihoods$intrusion_5 <- dvm(in_data$response_error, in_data$intrusion_offset5, prec)
  likelihoods$intrusion_6 <- dvm(in_data$response_error, in_data$intrusion_offset6, prec)
  likelihoods$intrusion_7 <- dvm(in_data$response_error, in_data$intrusion_offset7, prec)
  likelihoods$intrusion_8 <- dvm(in_data$response_error, in_data$intrusion_offset8, prec)
  likelihoods$intrusion_9 <- dvm(in_data$response_error, in_data$intrusion_offset9, prec)
  
  # Weight the non-targets within the intrusion components (Equal probability across intrusions)
  likelihoods$weighted_intrusion <- rowSums(likelihoods[2:10])/n_intrusions
  
  # Weight the components
  weighted_like <- ((1-gamma) * likelihoods$target) + (gamma * likelihoods$weighted_intrusion)
  
  # Where likelihood is very low, substitute with a minimum value to avoid values of 0
  weighted_like[weighted_like < 1e-10] <- 1e-10
  nLL <- -1*sum(log(weighted_like))
  return(nLL)
}


simulate_pure_intrusion <- function(participant, this_prec, this_gamma, this_data){
  
  # Empty dataframe to store simulated data
  sim_data <- data.frame(
    target_word = character(),
    target_angle = numeric(),
    target_position = integer(),
    simulated_response = numeric(),
    simulated_error = numeric(),
    angle_1 = numeric(),
    angle_2 = numeric(),
    angle_3 = numeric(),
    angle_4 = numeric(),
    angle_5 = numeric(),
    angle_6 = numeric(),
    angle_7 = numeric(),
    angle_8 = numeric(),
    angle_9 = numeric(),
    angle_10 = numeric(),
    participant = integer(),
    model = character(),
    stringsAsFactors = FALSE
  )
  
  nSims = 5
  
  intrusions <- this_data[,14:22] # rvm wants values between 0 and 2pi, intrusions are expressed as an offset 
  # Get the angles for each trial
  block_angles <- cbind(this_data[,6], this_data[,14:22])
  
  # Simultate each trial one by one
  for (i in 1:nrow(this_data)){
    
    # Stimulus identity (I'm using the stimulus word itself to match each observations with its intrusions)
    word <- as.character(this_data$word[i])
    target_angle <- this_data$target_angle[i]
    target_position <- this_data$present_trial[i]
    this_block_angles <- block_angles[i,]
    # Find the possible intrusions for this trial
    this_intrusions <- intrusions[i,]
    
    # Simulate each trial *nSims
    for (j in 1:nSims){
      no_offset_angles <- insert(as.vector(t(this_block_angles[2:10])), ats = target_position, values = target_angle)
      
      # Center on target
      sim_target <- rvm(1, target_angle, this_prec)
      
      # Intrusions  
      sim_intrusion <- vector()
      n_intrusions <- 9
      # Center on each of the possible intrusions
      for (k in 1:n_intrusions){
        this_intrusion <- rvm(1, this_intrusions[[k]], this_prec)
        sim_intrusion[k] <- this_intrusion
      }
      
      weight <- rmnom(1, 1, c((1-this_gamma), this_gamma))
      
      if (weight[,1] == TRUE){
        sim_response <- sim_target
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'pure_intrusion')
        
      } else if (weight[,2] == TRUE){
        # Intrusion, with equal probability of each possible intrusion
        intrusion_idx <- sample(1:n_intrusions, 1)
        # This is the simulated intrusion
        sim_response <- sim_intrusion[intrusion_idx]
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'pure_intrusion')
        
      } 
      sim_data$simulated_error <- as.numeric(sim_data$simulated_error)
    }
  }
  return(sim_data)
}
