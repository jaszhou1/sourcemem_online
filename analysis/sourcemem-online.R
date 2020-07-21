## sourcemem-online.R
##
## The top-level script for analysing the data from the online version
## of Jason Zhou's source memory experiment, including accessing the
## data from the Google App Engine instance, downloading the latest
## data (or working from a stored local copy), and plotting the data.


#####

## Connection parameters
SERVER.BASE.URL <- "https://jzhou-sourcemem-online.appspot.com"
SERVER.PORT <- NULL
SERVER.MASTER.API.KEY <- "zjFdXfQ64sgAVwQMx84IhzqzUPygpSguUkeLKLqQBIyxo8kP3yphBqF9ysd4IQsA" # <-- This needs to be in agreement with
                                                                                            # whatever is on the server.
#####
setwd("~/GitHub/sourcemem_online/analysis")
source("access-data.R")

## Get the completed users.
completed.users <- get.completed.users(SERVER.BASE.URL, SERVER.PORT,
                                       SERVER.MASTER.API.KEY)


this.user.data <- get.last.experiment.data.by.user.id(SERVER.BASE.URL, completed.users[[9]],
                                                      SERVER.PORT, SERVER.MASTER.API.KEY)

this.user.info <- get.user.information(SERVER.BASE.URL, completed.users[[9]],
                                       SERVER.PORT, SERVER.MASTER.API.KEY)

## Extract the required information for each stimuli across the trial types.
data <- data.frame(matrix(ncol=9,nrow=length(this.user.data$present_trials), dimnames=list(NULL, c("word", "is_sequential",
                                                               "recog_rating","recog_RT","target_angle",
                                                               "response_angle","response_error","source_RT", "valid_RT"))))

## Extract presentation data
  for(i in 1:length(this.user.data$present_trials)){
    data$word[i] <- this.user.data$present_trials[[i]]$target_word
    data$is_sequential[i] <- this.user.data$present_trials[[i]]$presentation_sequential
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
    index <- find_recog_index(words[i],this.user.data$confidence_trials)
    data$recog_rating[i] <- this.user.data$confidence_trials[[index]]$response
    data$recog_RT[i] <- this.user.data$confidence_trials[[index]]$rt
  } 
  
## Extract source recall data
  for (i in 1:length(words)){
    index <- find_source_index(words[i],this.user.data$recall_trials)
    data$target_angle[i] <- this.user.data$recall_trials[[index]]$target_angle
    data$response_angle[i] <- this.user.data$recall_trials[[index]]$hitting_angle
    data$response_error[i] <- abs(this.user.data$recall_trials[[index]]$target_angle - this.user.data$recall_trials[[index]]$hitting_angle)
    data$source_RT[i] <- this.user.data$recall_trials[[index]]$response_time
    if (this.user.data$recall_trials[[index]]$num_fast_attempts == 0 &&
        this.user.data$recall_trials[[index]]$num_slow_attempts == 0){
      data$valid_RT <- TRUE
    } else {
      data$valid_RT <- FALSE
    }
  } 