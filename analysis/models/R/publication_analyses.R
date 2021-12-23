# Load required packages
library(ggplot2)
library(ggpubr)
library(stringdist)
library(statip)
library(stats)

# Define some generally useful functions
# - Function to compute angular difference

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

# Data Processing
# - Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('sourcemem_data_2021.csv')
# - Make sure indexing of blocks starts at 0 (not -1, as coded in jsPsych)
if(min(data$present_trial == 0)){
  data$present_trial <- data$present_trial + 1
}

if(min(data$block == -1)){
  data$block <- data$block + 1
}

# - Exclude practice block
data <- data[data$block!= 0,]

# - Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

# Set up separate intrusion dataframe. This makes indexing intrusions easier
n_trials <- 10
n_intrusions <- n_trials - 1
intrusions <- data[,14:22]

# ===================== Difference between conditions ===========================
# Is there a difference between sequential and simultaneous conditions?
# - Recognition 

# Individual Hit Rates
get_hit_rate <- function(data, p){
  this_p_data <- data[data$participant == p,]
  n_trials <- nrow(this_p_data)
  n_hits <- nrow(this_p_data[this_p_data$recog_rating > 3,])
  this_hit_rate <- n_hits/n_trials
  return(this_hit_rate)
}

hit_rates <- data.frame(matrix(NA, nrow = 36, ncol = 2))
colnames(hit_rates)<- c("hit","cond")
for(i in unique(data$participant)){
  this_hit_rate <- get_hit_rate(data, i)
  this_condition <- unique(data[data$participant == i,]$is_sequential)
  hit_rates[i, 1] <- this_hit_rate
  hit_rates[i, 2] <- this_condition
}

seq_hit_rates <- hit_rates[hit_rates$cond == TRUE,]
sim_hit_rates <- hit_rates[hit_rates$cond == FALSE,]

# - Source error, pooled within and compared across condition
seq_data <- data[data$is_sequential == TRUE,]
sim_data <- data[data$is_sequential == FALSE,]

t.test(seq_data$response_error, sim_data$response_error)

data[data$is_sequential == TRUE,]$is_sequential <- 'Sequential Presentation'
data[data$is_sequential == FALSE,]$is_sequential <- 'Simultaneous Presentation'

condition_plot <- ggplot(data, aes(response_error)) + geom_histogram(aes(y=..density..)) + facet_wrap(~is_sequential) +
    theme_pubr() + theme(strip.text = element_text(size=12,lineheight=5.0)) +
    scale_x_continuous(breaks = c(-pi, 0, pi),
                       labels = c("-\u03c0", "0", "\u03c0")) +
    xlab('Response Error') + ylab('Density')

# ===================== Modelling ===========================
