## sourcemem-online-bays.R
##
## The top-level script for analysing the data from the online version
## of Jason Zhou's source memory experiment, including accessing the
## data from the Google App Engine instance, downloading the latest
## data (or working from a stored local copy), and plotting the data.

## This version of the code calls all target angles in a block for
## the purposes of developing the Bays random intrusions model.

#####

## Connection parameters
SERVER.BASE.URL <- "https://jzhou-sourcemem-online.appspot.com"
SERVER.PORT <- NULL
SERVER.MASTER.API.KEY <- "zjFdXfQ64sgAVwQMx84IhzqzUPygpSguUkeLKLqQBIyxo8kP3yphBqF9ysd4IQsA" # <-- This needs to be in agreement with
# whatever is on the server.
#####
setwd("~/git/sourcemem_online/analysis")
library(ggplot2)
source("access-data.R")
sessions <- 1:10

# Function to compute angular difference

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

## Get the started users
started.users <- get.started.users(SERVER.BASE.URL, SERVER.PORT,
                                   SERVER.MASTER.API.KEY)

## Get the completed users.
completed.users <- get.completed.users(SERVER.BASE.URL, SERVER.PORT,
                                       SERVER.MASTER.API.KEY)
# 
# last.session.data <- get.last.experiment.data.by.user.id(SERVER.BASE.URL, completed.users[[1]],
#                                                       SERVER.PORT, SERVER.MASTER.API.KEY)
# 
# this.user.info <- get.user.information(SERVER.BASE.URL, started.users[[1]],
#                                        SERVER.PORT, SERVER.MASTER.API.KEY)


get_session <- function(p,s){
  
  this.session.data <- get.session.data.by.user.id(SERVER.BASE.URL, p, s,
                                                   SERVER.PORT, SERVER.MASTER.API.KEY)
  ## Extract the required information for each stimuli across the trial types.
  data <- data.frame(matrix(ncol=12,nrow=length(this.session.data$present_trials), dimnames=list(NULL, c("word", "is_sequential", "present_trial",
                                                                                                         "recog_rating","recog_RT","target_angle",
                                                                                                         "response_angle","response_error","source_RT", "valid_RT",
                                                                                                         "block", "session"))))
  ## Add a number of empty columns for intrusions that is equivalent to the number of trials per block -1 
  
  ## Extract presentation data
  for(i in 1:length(this.session.data$present_trials)){
    data$word[i] <- this.session.data$present_trials[[i]]$target_word
    data$is_sequential[i] <- this.session.data$present_trials[[i]]$presentation_sequential
    data$present_trial[i] <- this.session.data$present_trials[[i]]$trial
  }
  
  ## Function to sort through the scrambled blocks and find the index needed for extraction
  find_recog_index <- function(x, list){
    for (i in 1:length(list)){
      if (list[[i]]$stimulus == x){
        return(i)
      } 
    }
  }
  
  ## The stimulus word is called "target_angle" for the recall trials. Could change how it is named in the database, 
  ## but easier for now to just use the different name here
  find_source_index <- function(x, list){
    for (i in 1:length(list)){
      if (list[[i]]$target_word == x){
        return(i)
      } 
    }
  }
  
  ## Extract recognition data
  words <- unique(data$word)
  
  for (i in 1:length(words)){
    index <- find_recog_index(words[i],this.session.data$confidence_trials)
    data$recog_rating[i] <- this.session.data$confidence_trials[[index]]$response
    data$recog_RT[i] <- this.session.data$confidence_trials[[index]]$rt
  } 
  
  ## Extract source recall data
  for (i in 1:length(words)){
    index <- find_source_index(words[i],this.session.data$recall_trials)
    data$block[i] <- this.session.data$recall_trials[[index]]$block
    data$session[i] <- this.session.data$session_number
    data$target_angle[i] <- this.session.data$recall_trials[[index]]$target_angle
    data$response_angle[i] <- this.session.data$recall_trials[[index]]$hitting_angle
    data$response_error[i] <- this.session.data$recall_trials[[index]]$angular_error
    ## Correcting for an error in the old javascript calculation of angular error  
    if(data$response_error[i] < -pi){
      data$response_error[i] <- data$response_error[i] + 4*pi 
    }
    data$source_RT[i] <- this.session.data$recall_trials[[index]]$response_time
    data$valid_RT [i] <- (this.session.data$recall_trials[[index]]$num_fast_attempts == 0 &&
                            this.session.data$recall_trials[[index]]$num_slow_attempts == 0)
  }
  
  data$recog_band <- ifelse(data$recog_rating >= 1 & data$recog_rating <= 3, 'Unrecognized',
                            ifelse(data$recog_rating >=8 & data$recog_rating <=9, 'Low',
                                   ifelse(data$recog_rating ==0, 'High','N/A')))
  
  # Convert the indexing to start at 1
  data$block <- data$block + 1
  data$present_trial <- data$present_trial + 1
  
  # Assemble a list of intrusions for each trial, and append this to the data structure
  num_intrusions <- 9 # This is n-1 where n is number of trials per block
  
  intrusion_labels <- vector()
  for (i in 1:num_intrusions){
    intrusion_labels[i] <- sprintf('intrusion_%i',i)
  }
  
  intrusion_offset_labels <- vector()
  for (i in 1:num_intrusions){
    intrusion_offset_labels[i] <- sprintf('intrusion_offset%i',i)
  }
  
  lag_labels <- vector()
  for (i in 1:num_intrusions){
    lag_labels[i] <- sprintf('intrusion_lag_%i',i)
  }
  
  intrusions <- data.frame(matrix(ncol=9,nrow=125))
  colnames(intrusions) <- intrusion_labels
  
  intrusion_lags <- data.frame(matrix(ncol=9,nrow=125))
  colnames(intrusion_lags) <- lag_labels
  
  intrusion_offsets <- data.frame(matrix(ncol=9,nrow=125))
  colnames(intrusion_offsets) <- intrusion_offset_labels
  
  for (i in 1:nrow(data)){
    this_block_data <- data[data$block == data$block[i],]
    # The possible intrusions are items in the same block as this trial, but do not have the same stimulus word
    this_block_intrusions <- this_block_data[this_block_data$word != data$word[i],]
    this_intrusions <- this_block_intrusions$target_angle
    this_intrusion_offsets <- angle_diff(data$target_angle[i], this_intrusions)
    this_intrusion_lags <- this_block_intrusions$present_trial - data$present_trial[i]
    if (data$block[i] == 0){
      this_intrusions <- c(this_intrusions, NA, NA, NA, NA, NA)
      this_intrusion_offsets <- c(this_intrusion_offsets, NA, NA, NA, NA, NA) # Filling up with NAs for practice block
      this_intrusion_lags <- c(this_intrusion_lags, NA, NA, NA, NA, NA)
    }
    intrusions[i,] <- this_intrusions
    intrusion_offsets[i,] <- this_intrusion_offsets
    intrusion_lags[i,] <- this_intrusion_lags
  }
  data <- cbind(data,intrusions, intrusion_offsets, intrusion_lags)
  return(data)
}


# Plot a particular session for a participant
plot_session <- function(p,s){
  
  data <- get_session(p,s)
  
  high_error <- ggplot(data = data[data$recog_band == 'High',], aes(x = response_error)) +
    geom_histogram(bins = 50) +
    labs(title ="High Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  low_error <- ggplot(data = data[data$recog_band == 'Low',], aes(x = response_error)) +
    geom_histogram(bins = 50) +
    labs(title ="Low Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  unrecog_error <- ggplot(data = data[data$recog_band == 'Unrecognized',], aes(x = response_error)) +
    geom_histogram(bins = 50) +
    labs(title ="Unrecognized Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  error <- ggplot(data = data, aes(x = response_error)) +
    geom_histogram(bins = 50) +
    labs(title ="All Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  rt <- ggplot(data = data, aes(x = source_RT)) +
    geom_histogram(bins = 50) +
    labs(title ="All Recognition Ratings", x = "Response Time (ms)", y = "Frequency") +
    theme_classic()
  
  #print(high_error)
  print(error)
  print(rt)
  return(data)
}  

## Plot histograms of response error and response times for all sessions for this participant

plot_participant <- function(p){
  this.user.data = data.frame()
  for(s in 1:length(sessions)){
    
    this.session.data <- get_session(p,s)
    # Bind sessions together
    
    this.user.data <- rbind(this.user.data, this.session.data)
    
    # Filter out invalid RTs
    # this.user.data <- this.user.data[this.user.data$valid_RT == TRUE,]
  }
  
  
  
  # high_error <- ggplot(data = this.user.data[this.user.data$recog_band == 'High',], aes(x = response_error)) +
  #   geom_histogram(bins = 50) +
  #   labs(title ="High Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
  #   theme_classic()
  # 
  # low_error <- ggplot(data = this.user.data[this.user.data$recog_band == 'Low',], aes(x = response_error)) +
  #   geom_histogram(bins = 50) +
  #   labs(title ="Low Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
  #   theme_classic()
  # 
  # unrecog_error <- ggplot(data = this.user.data[this.user.data$recog_band == 'Unrecognized',], aes(x = response_error)) +
  #   geom_histogram(bins = 50) +
  #   labs(title ="Unrecognized Recognition Ratings", x = "Response Error (radians)", y = "Frequency") +
  #   theme_classic()
  
  if (this.user.data$is_sequential){
    cond = "Sequential"
  } else {
    cond = "Simultaneous"
  }
  
  title <- sprintf("Participant ID: %.0f \nCondition: %s \nResponse Times", p, cond)
  
  rt <- ggplot(data = this.user.data[this.user.data$recog_band != 'Unrecognized',], aes(x = source_RT)) +
    geom_histogram(bins = 50) +
    labs(title = title, x = "Response Time (ms)", y = "Frequency") +
    theme_classic()
  
  title <- sprintf("Participant ID: %.0f \nCondition: %s \nResponse Error", p, cond)
  
  error <- ggplot(data = this.user.data[this.user.data$recog_band != 'Unrecognized',], aes(x = response_error)) +
    geom_histogram(bins = 50) +
    labs(title = title, x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  print(error)
  print(rt)
  
  # Save Plots
  
  filename = sprintf('Subject_%.0f_Error.png', p)
  ggsave(filename, plot = error)
  
  filename = sprintf('Subject_%.0f_RT.png', p)
  ggsave(filename, plot = rt)
  
  return(this.user.data)
}

## Save all the data as one csv

save.all.data <- function(filename){
  all.data = data.frame()
  # Participant Loop
  for(h in 1:length(completed.users)){
    this.user.data = data.frame()
    # Session Loop
    for(j in 1:length(sessions)){
      data <- get_session(completed.users[[h]],j)
      # Bind sessions together
      
      this.user.data <- rbind(this.user.data, data)
      # Filter out invalid RTs
      #this.user.data <- this.user.data[this.user.data$valid_RT == TRUE,]
      
    }
    this.user.data$participant <- h
    all.data <- rbind(all.data, this.user.data)
  }
  
  # Recode the confidence ratings to be between 1 and 6
  all.data[all.data$recog_rating == 8,]$recog_rating <- 4
  all.data[all.data$recog_rating == 9,]$recog_rating <- 5
  all.data[all.data$recog_rating == 0,]$recog_rating <- 6
  
  # Save data as .csv file
  write.csv(all.data, filename, row.names = FALSE)
  
  ## Save all users data
  # for(i in 1:length(completed.users)){
  #   this.user.data <- get.last.experiment.data.by.user.id(SERVER.BASE.URL, completed.users[[i]],
  #                                                         SERVER.PORT, SERVER.MASTER.API.KEY)
  #   filename = sprintf('Subject%s.RData', i)
  #   save(this.user.data, file = filename)
  # }
  return(all.data)
}


