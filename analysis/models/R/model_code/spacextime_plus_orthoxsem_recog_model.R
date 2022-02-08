## spacextime_plus_orthoxsem_recog_model.R

# This code is based on the spatiotemporal gradient model.

# In this variant, space and time are multiplicative components, while semantic and orthographic are additive.

# Also adds a binary recognition factor so unrecognised items do not intrude, regardless of other factors

# params = [prec1, prec2, gamma, kappa, lambda_b, lambda_f, beta, zeta,  tau, rho, chi, psi]
#             1    2       3      4         5    , 6    ,     7,    8    9    10    11, 12
# prec1, prec2: Precision of von Mises components (mem, intrusion)
# gamma: Overall scaling of intrusions
# kappa: Scaling parameter for forwards vs backwards intrusion decay slope
# lambda_b: Decay rate of backwards temporal gradient
# lambda_f: Decay rate of forwards temporal gradient
# beta: proportion of guesses
# zeta: precision for Shepard similarity function (perceived spatial distance)
# chi: precision for Shepard similarity from orthographic levenshtein distance
# psi : weighting for semantic similarity
spacextime_plus_orthoxsem_recog_model <- function(params, data){
  
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
  zeta <- params[8]
  #tau <- params[8]
  rho <- params[9]
  chi <- params[10]
  psi <- params[11]
  
  if(rho+chi+psi >= 1){
    print("Invalid intrusion component weight")
    nLL <- 1e7
    return(nLL)
  }
  
  tau <- 1-(rho+chi+psi)
  # Function to compute angular difference
  
  angle_diff <- function(a,b){
    diff <- atan2(sin(a-b), cos(a-b))
    return(diff)
  }
  
  shepard_similarity <- function(x, k){
    x <- exp(-k * x)
    return(x)
  }
  
  convert_orthographic_similarity <- function(x, length){
    similarity <- (length - x)/length
    return(similarity)
  }
  
  # Turn levenshtein distance into shepard similarity
  orthographic_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  orthographic_similarity[,1:9] <- lapply(data[,51:59], convert_orthographic_similarity, length = 4)
  
  # Scale semantic cosine similarity
  semantic_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  semantic_similarity[,1:9] <- data[,60:68] 
  
  # Turn cosine distances between target and intrusions into Shepard similarity
  spatial_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  spatial_similarity[,1:9] <- lapply(data[,42:50], shepard_similarity, k = zeta)
  
  # Define a vector of raw temporal similarities
  temporal_gradient <- setNames(data.frame(matrix(ncol = n_intrusions*2, nrow = 0)), setdiff(seq(-n_intrusions, n_intrusions), 0))
  # Backwards intrusion slope
  temporal_gradient[1,1:n_intrusions] <- (1-kappa)*exp(-lambda_b*(abs(-n_intrusions:-1)))
  # Forwards intrusion slope
  temporal_gradient[1,(n_intrusions+1):(n_intrusions*2)] <- kappa*exp(-lambda_f*(abs(1:n_intrusions)))
  # Normalise across serial positions
  temporal_gradient <- temporal_gradient/sum(temporal_gradient)
  
  # Replace the intrusion lag positions with the normalised temporal similarities
  temporal_similarity <- data[,32:40]
  for(i in setdiff(seq(-n_intrusions, n_intrusions), 0)){
    temporal_similarity[temporal_similarity == i] <- temporal_gradient[[as.character(i)]]
  }
  
  intrusion_recognition <- data[,70:78]
  
  # Multiply the temporal similarities with corresponding spatial similarity to get a spatiotemporal gradient on each trial
  intrusion_weights <- (((temporal_similarity^tau) * (spatial_similarity^rho)) + ((orthographic_similarity^chi) * (semantic_similarity^psi))) * intrusion_recognition
  
  # Multiply all intrusion weights with overall intrusion scaling parameter
  intrusion_weights <- gamma*intrusion_weights
  
  colnames(intrusion_weights) <- c("weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", "weight_7", "weight_8", "weight_9")
  
  target_weight <- 1 - rowSums(intrusion_weights)
  trial_weights <- cbind(target_weight, intrusion_weights)
  
  # Multiply all weights by 1-beta, the non-guessed responses, based on the serial position of the target
  # Different betas for primacy and recency items
  
  trial_weights <- trial_weights * (1-beta)
  trial_weights[, length(trial_weights)+1] <- beta
  
  
  # Make sure all weights are positive numbers
  if(any(trial_weights < 0)){
    print("Invalid: Negative weight")
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
simulate_spacextime_plus_orthoxsem_recog_model <- function(participant, data, pest){
  
  # Check that trial numbers are 1-indexed
  if(min(data$present_trial) == 0){
    data$present_trial <- data$present_trial + 1
  }
  
  n_trials <- 10
  n_intrusions <- 9 
  # Get parameters
  prec1 <- pest[[1]]
  prec2 <- pest[[2]]
  gamma <- pest[[3]]
  kappa <- pest[[4]]
  lambda_b <- pest[[5]]
  lambda_f <- pest[[6]]
  beta <- pest[[7]]
  zeta <- pest[[8]]
  #tau <- pest[[8]]
  rho <- pest[[9]]
  chi <- pest[[10]]
  psi <- pest[[11]]
  
  tau <- 1-(rho+chi+psi)
  
  shepard_similarity <- function(x, k){
    x <- exp(-k * x)
    return(x)
  }
  
  # Turn levenshtein distance into shepard similarity
  orthographic_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  orthographic_similarity[,1:9] <- lapply(data[,51:59], convert_orthographic_similarity, length = 4)
  
  # Scale semantic cosine similarity
  semantic_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  semantic_similarity[,1:9] <- data[,60:68] 
  
  # Turn cosine distances between target and intrusions into Shepard similarity
  spatial_similarity <- data.frame(matrix(nrow = nrow(data), ncol = n_intrusions))
  spatial_similarity[,1:9] <- lapply(data[,42:50], shepard_similarity, k = zeta)
  
  # Define a vector of raw temporal similarities
  temporal_gradient <- setNames(data.frame(matrix(ncol = n_intrusions*2, nrow = 0)), setdiff(seq(-n_intrusions, n_intrusions), 0))
  # Backwards intrusion slope
  temporal_gradient[1,1:n_intrusions] <- (1-kappa)*exp(-lambda_b*(abs(-n_intrusions:-1)))
  # Forwards intrusion slope
  temporal_gradient[1,(n_intrusions+1):(n_intrusions*2)] <- kappa*exp(-lambda_f*(abs(1:n_intrusions)))
  # Normalise across serial positions
  temporal_gradient <- temporal_gradient/sum(temporal_gradient)
  
  # Replace the intrusion lag positions with the normalised temporal similarities
  temporal_similarity <- data[,32:40]
  for(i in setdiff(seq(-n_intrusions, n_intrusions), 0)){
    temporal_similarity[temporal_similarity == i] <- temporal_gradient[[as.character(i)]]
  }
  
  intrusion_recognition <- data[,70:78]
  
  # Multiply the temporal similarities with corresponding spatial similarity to get a spatiotemporal gradient on each trial
  intrusion_weights <- (((temporal_similarity^tau) * (spatial_similarity^rho)) + ((orthographic_similarity^chi) * (semantic_similarity^psi))) * intrusion_recognition
  
  
  # Multiply all intrusion weights with overall intrusion scaling parameter
  intrusion_weights <- gamma*intrusion_weights
  
  colnames(intrusion_weights) <- c("weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", "weight_7", "weight_8", "weight_9")
  
  target_weight <- 1 - rowSums(intrusion_weights)
  trial_weights <- cbind(target_weight, intrusion_weights)
  
  if(any(trial_weights < 0)){
    print("Invalid: Negative weight")
  }
  
  # Multiply all weights by 1-beta, the non-guessed responses, based on the serial position of the target
  # Different betas for primacy and recency items
  
  trial_weights <- trial_weights * (1-beta)
  trial_weights[, length(trial_weights)+1] <- beta
  
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
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant)
      } else if (sim_intrusion_position[1]){
        # Decide which stimulus angle is the center of this retrieval
        sim_angle <- this_block_angles[sim_intrusion_position == 1]
        sim_response <- rvm(1, sim_angle, prec1)
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant)
      } else {
        sim_angle <- this_block_angles[sim_intrusion_position == 1]
        sim_response <- rvm(1, sim_angle, prec2)
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant)
      }
    }
  }
  sim_data[2:16]  <- lapply(sim_data[2:16], as.numeric)
  return(sim_data)
}
