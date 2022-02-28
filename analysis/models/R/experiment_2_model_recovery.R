# Load in modelling functions
source('~/git/sourcemem_online/analysis/models/R/experiment_2_modelling.R')
source('~/git/sourcemem_online/analysis/models/R/model_code/temporal_gradient_model_flat_guesses.R')
# Load in fitted model (need the parameter estimates)
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/fitted_exp2.RData")
setwd("~/git/sourcemem_online/analysis/models/R/model_recovery_crossfits")

MODELS <- c('Temporal', 'Spatiotemporal',
            'Semantic', 'Orthographic')
PARTICIPANTS <- unique(data$participant)

model_recovery <- function(participant, n_runs){
  model_idx <- rep(1:length(MODELS), each = n_runs)
  run_idx <- rep(1:n_runs, length(MODELS))
  this_data <- data[data$participant == participant,]
  cl <- makeForkCluster(12)
  registerDoParallel(cl)
  crossfit = foreach (j = 1:length(model_idx),
                 .combine = rbind) %dopar% {
    # Define the fitted estimates and the simulation function for this model
    model_name <- MODELS[model_idx[j]]
    # Find which run of this model this is
    n_run <- run_idx[j]
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
    
    # Empty dataframe for cross fitted models to live
    this_crossfit <- data.frame(matrix(nrow = length(MODELS), ncol = 5))
    # Fit each of the models to this simulated dataset
    for(k in 1:length(MODELS)){
      this_fitting_model_name <- MODELS[k]
      if (this_fitting_model_name  == 'Temporal'){
        this_fitting_model <- temporal_model
        lower <- c(10, 1, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 1)
      } else if (this_fitting_model_name  == 'Spatiotemporal'){
        this_fitting_model <- spatiotemporal_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 2, 1, 5, 5, 1, 1, 1)
      } else if (this_fitting_model_name  == 'Semantic'){
        this_fitting_model <- semantic_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 0.8, 1, 1, 1)
      } else if (this_fitting_model_name  == 'Orthographic'){
        this_fitting_model <- ortho_model
        lower <- c(10, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        upper <- c(40, 20, 1, 1, 5, 5, 0.8, 1, 1, 1)
      } else {
        print("invalid fitting model string")
        stop()
      }
      this_fit <- DEoptim(this_fitting_model, lower, upper, 
                          control = DEoptim.control(itermax = 200), 
                          this_simulated_data)
      # Calculate aic
      aic <- get_aic(this_fit$optim$bestval, length(upper))
      this_crossfit[k,1] <- participant
      this_crossfit[k,2] <- model_name
      this_crossfit[k,3] <- this_fitting_model_name
      this_crossfit[k,4] <-aic
      this_crossfit[k,5] <- n_run
    }
    return(this_crossfit)
  }
  colnames(crossfit) <- c("participant", "gen_model", "fit_model", "aic", "run")
  crossfit <- as.data.frame(crossfit)
  save(crossfit, file = paste(toString(Sys.Date()), '_P',participant,'_crossfit.RData', sep =""))
  return(crossfit)
}