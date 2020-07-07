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

source("access-data.R")

## Get the completed users.
completed.users <- get.completed.users(SERVER.BASE.URL, SERVER.PORT,
                                       SERVER.MASTER.API.KEY)


this.user.data <- get.last.experiment.data.by.user.id(SERVER.BASE.URL, completed.users[[1]],
                                                      SERVER.PORT, SERVER.MASTER.API.KEY)

## Extract the required information for each stimuli across the trial types.
data <- data.frame(matrix(ncol=9,nrow=length(this.user.data$present_trials), dimnames=list(NULL, c("word", "stimulus", "contition",
                                                               "recog_rating","recog_RT","target_angle",
                                                               "response_angle","response_error","response_RT"))))

for(i in length(this.user.data$present_trials)){
  data$word[i] <- this.user.data$present_trials[[i]]$target_word
}
