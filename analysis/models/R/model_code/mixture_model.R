## mixture_model_vectorised.R
##
## This script contains the Zhang and Luck (2008) mixture model which is a mixture between
## a uniform distribution. This version runs without loops for efficiency

## Zhang and Luck Mixture Model Function
mixture_model <- function(params, in_data){
  # Parameters
  prec <- params[1] # Precision of Memory Distribution
  beta <- params[2] # Proportion of responses due to uniform guesses
  
  # Check that parameters are ok
  
  
  likelihoods <- data.frame(matrix(ncol=2,nrow=nrow(in_data), dimnames=list(NULL, c('memory', 'guess'))))
  
  # Memory component
  likelihoods$memory <- dvm(in_data$response_error, 0, prec)
  
  # Guessing component
  likelihoods$guess <- dcircularuniform(in_data$response_angle)

  # Weight the memory and guessing processes
  weighted_likelihood <- ((1-beta) * likelihoods$memory) + (beta * likelihoods$guess) 
  weighted_likelihood[weighted_likelihood < 1e-10] = 1e-10 # to avoid likelihoods of zero, substitute with a minimum value

  # Joint probability by summing logs (easier on computers than multiplying), and make negative for optimisation
  nLL <- -1*sum(log(weighted_likelihood))
  return(nLL)
}

# Simulate a fully formatted dataset, in line with the intrusion models
simulate_mixture <- function(participant, this_prec, this_beta, this_data){
  
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
  
  # Get the angles for each trial
  block_angles <- cbind(this_data[,6], this_data[,14:22])
  
  # Simultate each trial one by one
  for (i in 1:nrow(this_data)){
    
    # Stimulus identity (I'm using the stimulus word itself to match each observations with its intrusions)
    # Stimulus identity
    
    word <- as.character(this_data$word[i])
    target_angle <- this_data$target_angle[i]
    target_position <- this_data$present_trial[i]
    this_block_angles <- block_angles[i,]
    participant <- this_data$participant[i]

    
    # Simulate each trial *nSims
    for (j in 1:nSims){
      no_offset_angles <- insert(as.vector(t(this_block_angles[2:10])), ats = target_position, values = target_angle)
      
      # Center on target
      sim_target <- rvm(1, target_angle, this_prec)
      
      # Guesses
      sim_guess <- runif(1, -pi, pi)
      

      weight <- rmnom(1, 1, c((1-this_beta), this_beta))
      
      if (weight[,1] == TRUE){
        sim_response <- sim_target
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'no_intrusion')
      } else if (weight[,2] == TRUE){
        sim_response <- sim_guess
        sim_error <- angle_diff(target_angle, sim_response)
        sim_data[nrow(sim_data)+1,] <- c(word, target_angle, target_position, sim_response, sim_error, no_offset_angles, participant, 'no_intrusion')
      }
    }
  }
  sim_data[2:16]  <- lapply(sim_data[2:16], as.numeric)
  return(sim_data)
}

