# Plot Experiment 1 Diffusion

## Read in, and transform data as required. ##

# Plot marginal response error and time model predictions against data
setwd("~/git/sourcemem_online/analysis/models/MATLAB/")
data <- read.csv('experiment_1.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

setwd("~/git/sourcemem_online/analysis/models/MATLAB/experiment_1")

pure_guess <- read.csv('sim_pure_guess.csv', header = FALSE)
pure_guess$model <- 'Pure Guess'

pure_intrusion <- read.csv('sim_pure_intrusion.csv', header = FALSE)
pure_intrusion$model <- 'Pure Intrusion'

intrusion <- read.csv('sim_intrusion_eta.csv', header = FALSE)
intrusion$model <- 'Intrusion + Guess'

# intrusion_eta <- read.csv('sim_intrusion_eta.csv', header = FALSE)
# intrusion_eta$model <- 'Intrusion + Guess (Different Eta)'

temporal <- read.csv('sim_temporal.csv', header = FALSE)
temporal$model <- 'Temporal Gradient'

spatiotemporal <- read.csv('sim_spatiotemporal.csv', header = FALSE)
spatiotemporal$model <- 'Spatiotemporal Gradient'

model_predictions <- rbind(pure_guess, pure_intrusion, intrusion, temporal, spatiotemporal)
colnames(model_predictions) <- c('error', 'time', 'participant', 'model')

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
                 '#ED0000FF',
                 '#42B540FF',
                 '#0099B4FF',
                 '#925E9FFF')

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  "Pure Guess"= color_wheel[1],
  "Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[3],
  #"Intrusion + Guess (Different Eta)" = color_wheel[4],
  "Temporal Gradient" = color_wheel[4],
  "Spatiotemporal Gradient" = color_wheel[5]
)

## Compute variables required for chart layout.

AXIS.CEX <- 2
AXIS.LABEL.CEX <- 2
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 0.8


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
    points(model.data$value, model.data$prob, type="l", lty=2, lwd = 3, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  mtext(paste("Response Error (rad)"), side=1, cex= AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  # legend("topright", legend= MODEL.TYPES[model_list],
  #        col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  dev.off()
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
  plot.window(xlim=c(0, 7),
              ylim=c(0, 1.7))
  
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
    points(model.data$value, model.data$prob, type="l", lty = 2, lwd = 3, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(0, 2, 4, 6, 7), labels= c("0", "2", "4", "6", "7"), cex.axis=AXIS.CEX)
  mtext(paste("Response Time (s)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0.0, 0.5, 1.0, 1.5), cex.axis=AXIS.CEX)
  #mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  dev.off()
}

rt_quantiles <- c(0.1, 0.5, 0.9)
error_quantiles <- c(0.1, 0.3, 0.5, 0.9)
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
  colnames(model) <- c('response_error', 'source_RT', 'participant', 'model')
  for(i in MODEL.TYPES){
    this_model <- model[model$model == i,]
    this_qq <- all_qq_points(rt_quantiles, error_quantiles, this_model, i)
    model_qq <- rbind(model_qq, this_qq)
  }
  
  # Plot (ggplot)
  plot <- ggplot() +
    geom_point(data=data_qq, size = 3, aes(x= theta, y = rt, shape = factor(rt_q))) +
    geom_segment(data = data_qq, linetype = "solid", size = 1, alpha = 0.4, 
                 aes(x = theta, xend = theta, y = rt_lower, yend = rt_upper, group = rt_q)) +
    geom_point(data=model_qq, size = 3, alpha = 0.5, aes(x= theta, y = rt, shape = factor(rt_q), color = model)) +
    geom_line(data = model_qq, linetype="dashed", alpha = 0.5, size = 1, aes(x = theta, y = rt,
                                   color = model, group = interaction(model, rt_q))) +
    scale_color_manual(values=c('#42B540FF',
                                '#00468BFF',
                                '#ED0000FF',
                                '#925E9FFF',
                                '#0099B4FF')) +
    scale_x_continuous(name = 'Absolute Error (rad)', breaks = c(0, pi), limits = c(0, pi),
                       labels = c(0, expression(pi))) +
    scale_y_continuous(name = 'Response Time (s)', breaks = c(0.5, 1.0, 1.5, 2.0)) +
    guides(size = "none",
           color= guide_legend(title="Model"),
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
      axis.line = element_line(colour = "black")
    )
  
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

