# Plot Experiment 1 Diffusion

## Read in, and transform data as required. ##

# Plot marginal response error and time model predictions against data
setwd("~/git/sourcemem_online/analysis/models/MATLAB/data")
data <- read.csv('sourcemem_data_2021_distances.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

setwd("~/git/sourcemem_online/analysis/models/MATLAB")

pure_guess <- read.csv('sim_pure_guess.csv', header = FALSE)
pure_guess$model <- 'Pure Guess'

pure_intrusion <- read.csv('sim_pure_intrusion.csv', header = FALSE)
pure_intrusion$model <- 'Pure Intrusion'

intrusion <- read.csv('sim_intrusion.csv', header = FALSE)
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
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian")
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
AXIS.LABEL.CEX <- 2.5
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


# Joint Q-Q Plot
