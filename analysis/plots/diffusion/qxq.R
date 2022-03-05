# Per Participant
participant_qq_points <- function(rt_quantile, error_quantiles){
  for(i in participants){
    this_p_data <- data[data$participant == i,]
    # Covert response error to absolute, because we dont care about asymmetry alone y axis
    this_p_data$response_error <- abs(this_p_data$response_error)
    # Order data by absolute response error
    this_p_data <- this_p_data[order(this_p_data$response_error),]
    # Find response error quantiles
    this_error_quantiles <- quantile(this_p_data$response_error, probs = error_quantiles)
    # Sort data into bins based on quantiles
    this_participant_qq <- data.frame()
    for (j in 1:length(this_error_quantiles)){
      this_qq <- data.frame(matrix(nrow = length(rt_quantiles), ncol = 5))
      colnames(this_qq) <- c('theta', 'rt', 'theta_q', 'rt_q', 'participant')
      # Calculate RT quantiles for this bin of responses
      this_bin <- this_p_data[this_p_data$response_error < this_error_quantiles[[j]],]
      this_rt_quantiles <- quantile(this_bin$source_RT, probs = rt_quantiles)
      # Populate dataframe with requisite information for plot
      this_qq[,1] <- this_error_quantiles[[j]]
      this_qq[,2] <- this_rt_quantiles
      this_qq[,3] <- error_quantiles[[j]]
      this_qq[,4] <- rt_quantiles
      this_qq[,5] <- i
      this_participant_qq <- rbind(this_participant_qq, this_qq)
    }
  }
}

# All data
all_qq_points <- function(rt_quantile, error_quantiles, data, model_string){
  # Covert response error to absolute, because we dont care about asymmetry alone y axis
  data$response_error <- abs(data$response_error)
  # Order data by absolute response error
  data <- data[order(data$response_error),]
  # Find response error quantiles
  this_error_quantiles <- quantile(data$response_error, probs = error_quantiles)
  # Sort data into bins based on quantiles
  res <- data.frame()
  for (i in 1:length(this_error_quantiles)){
    this_qq <- data.frame(matrix(nrow = length(rt_quantiles), ncol = 5))
    colnames(this_qq) <- c('theta', 'rt', 'theta_q', 'rt_q', 'model')
    # Calculate RT quantiles for this bin of responses
    if(i == 1){
      this_bin <- data[data$response_error < this_error_quantiles[[i]],]
    } else{
      this_bin <- data[((data$response_error > this_error_quantiles[[i-1]])) & 
                         (data$response_error < this_error_quantiles[[i]]),]
    }
    this_rt_quantiles <- quantile(this_bin$source_RT, probs = rt_quantiles)
    # Populate dataframe with requisite information for plot
    this_qq[,1] <- this_error_quantiles[[i]]
    this_qq[,2] <- this_rt_quantiles
    this_qq[,3] <- error_quantiles[[i]]
    this_qq[,4] <- rt_quantiles
    this_qq[,5] <- model_string
    res <- rbind(res, this_qq)
  }
  return(res)
}