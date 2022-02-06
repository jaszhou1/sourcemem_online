## temporal_gradient_model_beta.R

# This code implements the temporal gradient model with a third uniform guessing component.

# This version has beta (the proportion of guesses) constant regardless of the position of the target
# So variation in the proportion of intrusions due to temporal gradient effect is all in memory/intrusions

# params = [prec, gamma, kappa, lambda_b, lambda_f, beta1, beta2]
#             1    2      3       4          5        6      7
# prec: Precision of von Mises components
# gamma: Overall scaling of intrusions
# kappa: Scaling parameter for forwards vs backwards intrusion decay slope
# lambda_b: Decay rate of backwards temporal gradient
# lambda_f: Decay rate of forwards temporal gradient
# beta: proportion of guesses
temporal_beta_model <- function(params, data){
  
  n_trials <- 10
  n_intrusions <- 9
  # Get parameters out from vector
  prec1 <- params[1]
  prec2 <- params[2]
  gamma <- params[3]
  kappa <- params[4]
  lambda_b <- params[5]
  lambda_f <- params[6]
  beta <- params[7]
  beta_primacy <- params[8]
  beta_recency <- params[9]
  
  # Function to compute angular difference
  
  angle_diff <- function(a,b){
    diff <- atan2(sin(a-b), cos(a-b))
    return(diff)
  }
  
  # Define a vector of raw temporal similarities
  temporal_similarity <- setNames(data.frame(matrix(ncol = n_intrusions*2, nrow = 0)), setdiff(seq(-n_intrusions, n_intrusions), 0))
  # Backwards intrusion slope
  temporal_similarity[1,1:n_intrusions] <- (1-kappa)*exp(-lambda_b*(abs(-n_intrusions:-1)))
  # Forwards intrusion slope
  temporal_similarity[1,(n_intrusions+1):(n_intrusions*2)] <- kappa*exp(-lambda_f*(abs(1:n_intrusions)))
  # Normalise across serial positions
  temporal_similarity <- temporal_similarity/sum(temporal_similarity)
  # Multiply with an overall intrusion scaling parameter
  temporal_similarity <- gamma*temporal_similarity
  
  # Replace the intrusion lag positions with the normalised temporal similarities
  intrusion_weights <- data[,32:40]
  for(i in setdiff(seq(-n_intrusions, n_intrusions), 0)){
    intrusion_weights[intrusion_weights == i] <- temporal_similarity[[as.character(i)]]
  }
  colnames(intrusion_weights) <- c("weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", "weight_7", "weight_8", "weight_9")
  
  target_weight <- 1 - rowSums(intrusion_weights)
  trial_weights <- cbind(target_weight, intrusion_weights)
  
  # Multiply all weights by 1-beta, the non-guessed responses, based on the serial position of the target
  # Different betas for primacy and recency items
  
  trial_weights[data$present_trial == 1,] <- trial_weights[data$present_trial == 1,] * (1-beta_primacy)
  trial_weights[data$present_trial == 1, length(trial_weights)+1] <- beta_primacy
  
  trial_weights[data$present_trial == 10,] <- trial_weights[data$present_trial == 10,] * (1-beta_recency)
  trial_weights[data$present_trial == 10, length(trial_weights)] <- beta_recency
  
  trial_weights[(data$present_trial != 1) & (data$present_trial != 10),] <- 
    trial_weights[(data$present_trial != 1) & (data$present_trial != 10),] * beta
  
  trial_weights[(data$present_trial != 1) & (data$present_trial != 10), length(trial_weights)] <- beta
  
  # Make sure all weights are positive numbers
  if(any(trial_weights < 0)){
    print("Invalid negative weight")
    nLL <- 1e7
    return(nLL)
  }
  
  
  cbind(data, intrusion_weights)
  # Get likelihoods of the response angle coming from a von Mises distribution centered on each of the angles in its block
  likelihoods <- data.frame(matrix(ncol=12,nrow=nrow(data), dimnames=list(NULL, c('position','target', 'intrusion_1','intrusion_2',
                                                                                  'intrusion_3','intrusion_4','intrusion_5',
                                                                                  'intrusion_6','intrusion_7','intrusion_8',
                                                                                  'intrusion_9', 'guess'))))
  # Save the study list position of each target 
  likelihoods$position <- data$present_trial
  
  # Memory component
  likelihoods$target <- dvm(data$response_angle, data$target_angle, prec1)
  
  # Intrusion component (There must be a better way to do this? like likelihoods[2:10] or something)
  likelihoods$intrusion_1 <- dvm(data$response_angle, data$intrusion_1, prec2)
  likelihoods$intrusion_2 <- dvm(data$response_angle, data$intrusion_2, prec2)
  likelihoods$intrusion_3 <- dvm(data$response_angle, data$intrusion_3, prec2)
  likelihoods$intrusion_4 <- dvm(data$response_angle, data$intrusion_4, prec2)
  likelihoods$intrusion_5 <- dvm(data$response_angle, data$intrusion_5, prec2)
  likelihoods$intrusion_6 <- dvm(data$response_angle, data$intrusion_6, prec2)
  likelihoods$intrusion_7 <- dvm(data$response_angle, data$intrusion_7, prec2)
  likelihoods$intrusion_8 <- dvm(data$response_angle, data$intrusion_8, prec2)
  likelihoods$intrusion_9 <- dvm(data$response_angle, data$intrusion_9, prec2)
  
  # Guessing component
  likelihoods$guess <- dcircularuniform(data$response_angle)
  
  likelihoods <- cbind(likelihoods, trial_weights)
  
  # This is still a loop, cant figure out how to use lapply using the columns 12:21 (weights) as an argument for weighted.mean function
  # I think vectorising this part of the code is the limiting factor in making this code run much faster
  likelihoods$weighted_likelihood <- NA
  for(i in 1:nrow(likelihoods)){
    weighted_like <- weighted.mean(likelihoods[i,2:12], likelihoods[i, 13:23])
    if (weighted_like < 1e-10){
      weighted_like <- 1e-10
    }
    likelihoods$weighted_likelihood[i] <- weighted_like
  }
  
  nLL <- -sum(log(likelihoods$weighted_likelihood))
  return(nLL)
}



# pest = temp[participant,5:9]

# Simulate data from fitted parameters of the temporal gradient model
simulate_temporal_beta_model <- function(participant, data, pest){
  
  # Check that trial numbers are 1-indexed
  if(min(data$present_trial) == 0){
    data$present_trial <- data$present_trial + 1
  }
  
  n_trials <- 10
  n_intrusions <- 9 
  # Get parameters
  prec1 <- pest$prec1
  prec2 <- pest$prec2
  gamma <- pest$gamma
  kappa <- pest$kappa
  lambda_b <- pest$lambda_b
  lambda_f <- pest$lambda_f
  beta <- pest$beta
  beta_primacy <- pest$beta_primacy
  beta_recency <- pest$beta_recency
  
  # Intrusion weights
  # Define a vector of raw temporal similarities
  temporal_similarity <- setNames(data.frame(matrix(ncol = n_intrusions*2, nrow = 0)), setdiff(seq(-n_intrusions, n_intrusions), 0))
  # Backwards intrusion slope
  temporal_similarity[1,1:n_intrusions] <- (1-kappa)*exp(-lambda_b*(abs(-n_intrusions:-1)))
  # Forwards intrusion slope
  temporal_similarity[1,(n_intrusions+1):(n_intrusions*2)] <- kappa*exp(-lambda_f*(abs(1:n_intrusions)))
  # Normalise across serial positions
  temporal_similarity <- temporal_similarity/sum(temporal_similarity)
  # Multiply with an overall intrusion scaling parameter
  temporal_similarity <- gamma*temporal_similarity
  
  intrusion_weights <- data[,32:40]
  for(i in setdiff(seq(-n_intrusions, n_intrusions), 0)){
    intrusion_weights[intrusion_weights == i] <- temporal_similarity[[as.character(i)]]
  }
  colnames(intrusion_weights) <- c("weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", "weight_7", "weight_8", "weight_9")
  
  target_weight <- 1 - rowSums(intrusion_weights)
  trial_weights <- cbind(target_weight, intrusion_weights)
  
  trial_weights[data$present_trial == 1,] <- trial_weights[data$present_trial == 1,] * (1-beta_primacy)
  trial_weights[data$present_trial == 1, length(trial_weights)+1] <- beta_primacy
  
  trial_weights[data$present_trial == 10,] <- trial_weights[data$present_trial == 10,] * (1-beta_recency)
  trial_weights[data$present_trial == 10, length(trial_weights)] <- beta_recency
  
  trial_weights[(data$present_trial != 1) & (data$present_trial != 10),] <- 
    trial_weights[(data$present_trial != 1) & (data$present_trial != 10),] * beta
  
  trial_weights[(data$present_trial != 1) & (data$present_trial != 10), length(trial_weights)] <- beta
  
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
  this_data <- data
  # Get the angles for each trial
  block_angles <- cbind(this_data[,6], this_data[,14:22])
  
  # Simulate each trial one by one
  for (i in 1:nrow(this_data)){
    
    # Stimulus identity
    word <- as.character(this_data$word[i])
    target_angle <- this_data$target_angle[i]
    target_position <- this_data$present_trial[i]
    this_block_angles <- block_angles[i,]
    this_weights <- trial_weights[i,]
    
    positions <- 1:n_trials
    
    # Simulate each trial *nSims
    for (j in 1:nSims){
      sim_intrusion_position <- rmnom(1, 1, this_weights)
      # This line is a bit hacky, I'm taking the intrusion angles only [2:10], and inserting the target angle in its serial position
      no_offset_angles <- insert(as.vector(t(this_block_angles[2:10])), ats = target_position, values = target_angle)
      
      # See if this trial is a guess
      if(sim_intrusion_position[length(sim_intrusion_position)]){
        sim_angle <- NA
        sim_response <- runif(1, -pi, pi)
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'temporal_beta')
      } else if (sim_intrusion_position[1]){
        # Decide which stimulus angle is the center of this retrieval
        sim_angle <- this_block_angles[sim_intrusion_position == 1]
        sim_response <- rvm(1, sim_angle, prec1)
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'temporal_beta')
      } else {
        sim_angle <- this_block_angles[sim_intrusion_position == 1]
        sim_response <- rvm(1, sim_angle, prec2)
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'temporal_beta')
      }
    }
  }
  sim_data[2:16]  <- lapply(sim_data[2:16], as.numeric)
  return(sim_data)
}
