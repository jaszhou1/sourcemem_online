# Redefine Valid RTs
# Currently, the upper bound of RTs is up to 7, 
# which allows a very small proportion (<0.1%)
# of responses that happen between 5 and 7 seconds
# This seems tricky for the diffusion model to fit,
# so we are going to cut the data off earlier,
# and see how that changes the model fits

setwd("~/git/sourcemem_online/analysis/models/MATLAB")
data <- read.csv("experiment_2.csv")
participants <- unique(data$participant)

new_data <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
for(i in participants){
  this_participant_data <- data[data$participant == i,]
  this_cutoff <- median(this_participant_data$source_RT) + 
    2.5*sd(this_participant_data$source_RT)
  this_participant_data$valid_RT <- this_participant_data$source_RT < this_cutoff
  new_data <- rbind(new_data, this_participant_data)
}

write.csv(new_data, file = "experiment_2_cutoff.csv")