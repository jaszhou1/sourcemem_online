# Load in modelling functions
source('~/git/sourcemem_online/analysis/models/R/experiment_2_modelling.R')
source('~/git/sourcemem_online/analysis/models/R/model_code/temporal_gradient_model_flat_guesses.R')
# Load in fitted model (need the parameter estimates)
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/fitted_exp2.RData")

MODELS <- c('Temporal', 'Spatiotemporal',
            'Semantic', 'Orthographic')
PARTICIPANTS <- unique(data$participant)

model_recovery <- function(participant){
  crossfit <- data.frame(matrix(ncol = 4, nrow = 0))
  this_data <- data[data$participant == participant,]
  for(j in MODELS){
    # Define the fitted estimates and the simulation function for this model
    model_name <- j
    if (model_name  == 'Temporal'){
      this_model <- temporal
      this_simulation_function <- simulate_temporal_model
    } else if (model_name  == 'Spatiotemporal'){
      this_model <- spatiotemporal
      this_simulation_function <- simulate_spatiotemporal_model
    } else if (model_name  == 'Semantic'){
      this_model <- semantic
      this_simulation_function <- simulate_semantic_model
    } else if (model_name  == 'Orthographic'){
      this_model <- ortho
      this_simulation_function <- simulate_ortho_model
    } else {
      print("invalid generating model string")
      stop()
    }
    # Get the parameter estimates for this model, for this participant
    this_pest <- this_model[participant,4:length(this_model)]
    
    # Generate a simulated dataset for this model/participant
    # from the estimated parameters.
    simulated_error <- this_simulation_function(participant, this_data, this_pest)
    this_simulated_data <- this_data
    this_simulated_data$response_angle <- simulated_error$simulated_response
    this_simulated_data$response_error <- simulated_error$simulated_error
    
    # Fit each of the models to this simulated dataset
    for(k in MODELS){
      if (k  == 'Temporal'){
        this_fitting_model <- temporal_model
        lower <- c(10, 1, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 1)
      } else if (k  == 'Spatiotemporal'){
        this_fitting_model <- spatiotemporal_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 2, 1, 5, 5, 1, 1, 1)
      } else if (k  == 'Semantic'){
        this_fitting_model <- semantic_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 0.8, 1, 1, 1)
      } else if (k  == 'Orthographic'){
        this_fitting_model <- ortho_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 0.8, 1, 1, 1)
      } else {
        print("invalid fitting model string")
        stop()
      }
      this_fit <- DEoptim(this_fitting_model, lower, upper, 
                          control = DEoptim.control(itermax = 2), 
                          this_simulated_data)
      # Calculate aic
      aic <- get_aic(this_fit$optim$bestval, length(upper))
      this_crossfit <- data.frame()
      this_crossfit[1,1] <- participant
      this_crossfit[1,2] <- j
      this_crossfit[1,3] <- k
      this_crossfit[1,4] <-aic
      crossfit <- rbind(crossfit, this_crossfit)
    }
  }
  colnames(crossfit) <- c("participant", "gen_model", "fit_model", "aic")
}
