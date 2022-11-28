# Plot Experiment 2 - "Second Criterion"

# In response to reviewer request that we explore how the model might be improved,
# I have simulated the model with a discrete mixture of targets with second criterion
# I now wish to plot the simulated predictions

# Plot marginal response error and time model predictions against data
# Plot marginal response error and time model predictions against data
setwd("~/git/sourcemem_online/analysis/models/MATLAB")
data <- read.csv('experiment_2.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

setwd("~/git/sourcemem_online/analysis/models/MATLAB/revision")

base_spatiotemporal <- read.csv('sim_base.csv', header = FALSE)
base_spatiotemporal$model <- 'Baseline'

mod_1 <- read.csv('sim_moderate.csv', header = FALSE)
mod_1$model <- 'Moderate'

mod_2 <- read.csv('sim_extreme.csv', header = FALSE)
mod_2$model <- 'Extreme'

model_predictions <- rbind(base_spatiotemporal,
                           mod_1,
                           mod_2)
colnames(model_predictions) <- c('error', 'time', 'model')

models <- unique(model_predictions$model)

# Exclude participants from plot if response error histograms appear uniform
# exclude_participants <- c(14, 29)
# data <- data[!data$participant%in%exclude_participants,]
# model_predictions <- model_predictions[!model_predictions%in%exclude_participants,]

# Transform data into (wrapped) density 

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "epanechnikov")
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:50] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-50):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  return(this_predictions)
}

response_error_predictions <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in 1:length(models)){
  this_model <- model_predictions[model_predictions$model == models[i],]
  this_predictions <- get_response_error_density(this_model)
  response_error_predictions <- rbind(response_error_predictions, this_predictions)
}
colnames(response_error_predictions) <- c("value", "prob", "model")

## Plotting ##

# Response Error charting parameters.

# Define some colours
color_wheel <- c('#00468BFF',
                 '#009E73',
                 '#ED0000FF')

line_types <- c('longdash', 'dotted', 'dashed')
MODEL.LTY <- list(
  "Baseline"= line_types[1],
  "Moderate"= line_types[2],
  "Extreme"= line_types[3]
)

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  "Baseline"= color_wheel[1],
  "Moderate"= color_wheel[2],
  "Extreme"= color_wheel[3]
)

## Compute variables required for chart layout.

AXIS.CEX <- 2
AXIS.LABEL.CEX <- 2
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 2.1


# Plot Response Error 
plot_response_error <- function(model_list, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, Y.RESP.HI))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(data$response_error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- response_error_predictions[response_error_predictions$model == model.type, ]
    points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 3, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  mtext(paste("Response Error (rad)"), side=1, cex= AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0, 1, 2), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  # legend("topright", legend= MODEL.TYPES[model_list],
  #        col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
}



## Response Time plot

get_response_time_density <- function(model){
  preds <- density(as.numeric(model$time), from = 0, to = 7, cut = FALSE, kernel = "gaussian", adjust = 2)
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  return(this_predictions)
}

response_time_predictions <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in 1:length(models)){
  this_model <- model_predictions[model_predictions$model == models[i],]
  this_predictions <- get_response_time_density(this_model)
  response_time_predictions <- rbind(response_time_predictions, this_predictions)
}
colnames(response_time_predictions) <- c("value", "prob", "model")

# Plot Response Time
plot_response_time <- function(model_list, filename){
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
  }
  
  plot.new()
  plot.window(xlim=c(0, 4),
              ylim=c(0, 2.5))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(data$source_RT/1000,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- response_time_predictions[response_time_predictions$model == model.type, ]
    points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 3, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(0, 1, 2, 3, 4), labels= c("0", "1", "2", "3", "4"), cex.axis=AXIS.CEX)
  mtext(paste("Response Time (s)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0.0, 1.0, 2.0), cex.axis=AXIS.CEX)
  #mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=line_types[model_list], lwd = 5, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  if(filename != "") {
    dev.off()
  }
}

rt_quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
error_quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
Q_SYMBOLS <- c(25, 23, 24)
# Joint Q-Q Plot
source('~/git/sourcemem_online/analysis/plots/diffusion/qxq.R')
source('~/git/sourcemem_online/analysis/plots/diffusion/RT_confidence_interval.R')
plot_qq <- function(data, model, filename){
  # Covert from ms to s
  data$source_RT <- data$source_RT/1000
  data_qq <- qq_CI(rt_quantiles, error_quantiles, data, 'data')
  model_qq <- data.frame()
  # For the model predictions, need to rename columns of simulated data to keep everything consistent
  colnames(model) <- c('response_error', 'source_RT', 'model')
  for(i in MODEL.TYPES){
    this_model <- model[model$model == i,]
    this_qq <- all_qq_points(rt_quantiles, error_quantiles, this_model, i)
    model_qq <- rbind(model_qq, this_qq)
  }
  model_qq$model_f <- factor(model_qq$model, c('Baseline', 'Moderate', 'Extreme'))
  # Plot (ggplot)
  plot <- ggplot() +
    geom_point(data=data_qq, size = 3, aes(x= theta, y = rt, shape = factor(rt_q))) +
    geom_segment(data = data_qq, linetype = "solid", size = 1, alpha = 0.5, 
                 aes(x = theta, xend = theta, y = rt_lower, yend = rt_upper, group = rt_q)) +
    geom_point(data=model_qq, size = 3, alpha = 1, aes(x= theta, y = rt, shape = factor(rt_q), color = model_f)) +
    geom_line(data = model_qq, alpha = 0.5, size = 1.5, aes(x = theta, y = rt, linetype= model_f,
                                                            color = model_f, group = interaction(model_f, rt_q))) +
    scale_linetype_manual(values=c('longdash', 
                                   'dotted', 
                                   'dashed')) + 
    scale_color_manual(values=c('#00468BFF',
                                '#009E73',
                                '#ED0000FF')) +
    scale_x_continuous(name = 'Absolute Error (rad)', breaks = c(0, pi), limits = c(0, pi),
                       labels = c(0, expression(pi))) +
    scale_y_continuous(name = 'Response Time (s)', breaks = c(0.5, 1.0, 1.5, 2.0)) +
    guides(size = "none",
           linetype = "none",
           color= "none",
           #color= guide_legend(title="Model"),
           shape= guide_legend(title="Response Time Quantile")) +
    theme(
      axis.text.x = element_text(color="black", size = 14),
      axis.text.y = element_text(color="black", size = 14),
      plot.title = element_blank(),
      axis.title.x = element_text(color="black", size=16),
      axis.title.y = element_text(color="black", size=16),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      legend.text=element_text(size= 14),
      #legend.position = 'bottom',
      axis.line = element_line(colour = "black")
    )  +
    facet_wrap(~model_f, nrow = 1)
  
  if(filename == ""){
    return(plot)
  } else {
    ggsave(filename, width=10.7, height=4, units = "in")
    return(plot)
  }
}
setwd("~/git/sourcemem_online/analysis/plots/diffusion")
individual_qq <- function(){
  for (i in unique(data$participant)){
    filename = sprintf('p_%i_qxq.png', i)
    plot_qq(data[data$participant == i,], model_predictions[model_predictions$participant == i,], filename)
  }
}
# Base R
# if(filename == "") {
#   X11() # Write to the screen
# } else {
#   png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
#   #pdf(file=filename, width=8.3, height=10.7)
# }
# par(mar = c(5, 5, 4, 6))
# plot.new()
# plot.window(xlim=c(0, pi),
#             ylim=c(0, 2.2))
# # Plot each RT quantile with a different character
# for(i in 1:length(rt_quantiles)){
#   this_data_rt_qq <- data_qq[data_qq$rt_q == rt_quantiles[i],]
#   points(x = this_data_rt_qq$theta, y = this_data_rt_qq$rt, type="p", pch = Q_SYMBOLS[i], bg = 'black', cex=2)
# }
# 
# for(model.type in MODEL.TYPES){
#   this_model_qq <- model_qq[model_qq$model == model.type,]
#   for(j in 1:length(rt_quantiles)){
#     this_model_rt_qq <- this_model_qq[this_model_qq$rt_q == rt_quantiles[j],]
#     points(x = this_model_rt_qq$theta, y = this_model_rt_qq$rt, type="l", lty = 2, lwd = 2, col = MODEL.COL[[model.type]])
#   }
# }
# axis(side=1, at=c(0, pi), labels= c(0, expression(-pi)), cex.axis=AXIS.CEX)
# mtext(paste("Absolute Response Error (rad)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
# axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
# mtext(paste("Response Time (s)"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
# ## Add in legend
# legend("topleft", legend= MODEL.TYPES, inset = c(0, 0),
#        col=color_wheel, lty=2, lwd = 2, bty = "n",cex=0.5, title="Models", title.adj= 0.5, xpd = TRUE, horiz = TRUE)
# dev.off()

## Plot individual RT predictions, trying to figure out whats going on with the bimodal RT prediction in Pure Guess
# Plot Response Time
plot_individual_response_time <- function(model_list){
  # Model_list only takes a single number in this function
  participants <- unique(model_predictions$participant)
  for(i in participants){
    
    this_model <- model_predictions[(model_predictions$model == models[model_list]) &
                                      (model_predictions$participant == i),]
    this_response_time_predictions <- get_response_time_density(this_model)
    colnames(this_response_time_predictions) <- c("value", "prob", "model")
    
    filename = sprintf('RT_participant_%i.png', i)
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
    
    plot.new()
    plot.window(xlim=c(0, 7),
                ylim=c(0, 1.7))
    
    ## Compute and plot the empirical histograms for response error.
    this_data <- data[data$participant == i,]
    resp.hist <- hist(this_data$source_RT/1000,
                      breaks=NUM.BINS, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    
    for(model.type in MODEL.TYPES[model_list]) {
      model.data <- this_response_time_predictions[this_response_time_predictions$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 6, col=MODEL.COL[[model.type]])
    }
    
    axis(side=1, at=c(0, 2, 4, 6, 7), labels= c("0", "2", "4", "6", "7"), cex.axis=AXIS.CEX)
    mtext(paste("Response Time (s)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
    axis(side=2, at=c(0.0, 0.5, 1.0, 1.5), cex.axis=AXIS.CEX)
    #mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
    
    ## Add in legend
    legend("topright", legend= MODEL.TYPES[model_list],
           col=color_wheel[model_list], lty=line_types[model_list], lwd = 5, bty = "n",cex=AXIS.CEX, title="Models")
    
    # Close the plotting device
    dev.off()
  }
}
