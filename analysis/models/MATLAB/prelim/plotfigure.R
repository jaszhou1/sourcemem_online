## plotfigure.R
##
## Function to put together a quick plot of one participants model fits against data.
#####

## Load Libraries, Data etc.
library(ggplot2)
setwd("~/GitHub/sourcemem_online/analysis/models/MATLAB/prelim")

data <- read.csv('sourcemem_data.csv')

# Just interested in recognised items for now
data <- data[data$recog_rating >= 3,]

Thresh <- read.csv('2021-01-12-13-32_Thresh.csv')
Cont <- read.csv('2021-01-12-13-31_Cont.csv')

models <- rbind(Cont,Thresh)
models$is_theta <- models$is_theta == ' true'
models$is_model <- models$is_model == ' true'

## Global plotting parameters
NUM.BINS <- 50

## Functions to extract relevant data
get_participant_data <- function(p){
  this_data <- data[data$participant == p,]
  return(this_data)
}

get_participant_model <- function(p){
  this_model <- data[models$participant == p,]
  return(this_model)
}

## Plotting functions
plot_participant <- function(p){
  
# Subset out this participants data and model fits from the larger datasets
  p.data <- get_participant_data(p)
  p.models <- get_participant_model(p)
  p.data$density=p.data$response_error/sum(p.data$response_error)*100

# Plot the data
  if (p.data$is_sequential){
    cond = "Sequential"
  } else {
    cond = "Simultaneous"
  }
  
  title <- sprintf("Participant ID: %.0f \nCondition: %s \nResponse Times", p, cond)
 
   error <- ggplot(data = p.data, aes(x = density)) +
    geom_histogram(bins = 50) +
    labs(title = title, x = "Response Error (radians)", y = "Frequency") +
    theme_classic()
  
  return(error)
}