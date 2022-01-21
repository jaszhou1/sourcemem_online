# Plot Experiment 1 Diffusion
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

intrusion_eta <- read.csv('sim_intrusion_eta.csv', header = FALSE)
intrusion_eta$model <- 'Intrusion + Guess (Different Eta)'

temporal <- read.csv('sim_temporal.csv', header = FALSE)
temporal$model <- 'Temporal Gradient'

spatiotemporal <- read.csv('sim_spatiotemporal.csv', header = FALSE)
spatiotemporal$model <- 'Spatiotemporal Gradient'

model_predictions <- rbind(pure_guess, pure_intrusion, intrusion, intrusion_eta, temporal, spatiotemporal)
colnames(model_predictions) <- c('error', 'time', 'participant', 'model')

models <- unique(model_predictions$model)

# model_predictions <- model_predictions[model_predictions$participant == 1,]
# data <- data[data$participant == 1,]

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

get_response_time_density <- function(model){
  preds <- density(as.numeric(model$time), from = 0, to = 7, cut = FALSE, kernel = "gaussian", adjust = 2)
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


## Response Error charting parameters.

# Define some colours
# Get a number of equally spaced colours
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

color_wheel <- gg_color_hue(6)

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  "Pure Guess"= color_wheel[1],
  "Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[3],
  "Intrusion + Guess (Different Eta)" = color_wheel[4],
  "Temporal Gradient" = color_wheel[5],
  "Spatiotemporal Gradient" = color_wheel[6]
)

## Compute variables required for chart layout.
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 1.5

# Plot Response Error 
plot_response_error <- function(){
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
  
  
  for(model.type in MODEL.TYPES) {
    model.data <- response_error_predictions[response_error_predictions$model == model.type, ]
    points(model.data$value, model.data$prob, type="l", col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c("-pi", "0", "pi"), cex.axis=0.75)
  mtext(paste("Response error (rads)"), side=1, cex=0.6, line=2.5)
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8), cex.axis=0.75)
  mtext(paste("Density"), side=2, cex=0.6, line=2.5)
}

plot_response_error <- function(data, model, colors){
  plot <- ggplot(data, aes(x=response_error, y = ..density..)) + geom_histogram(bins = 50,  alpha = 0.9) +
    geom_line(data = model, aes(x = value, y = prob, color = model), size = 1.2, adjust = 2) +
    scale_x_continuous(breaks  = c(-pi, 0, pi), 
                       labels = c("-\u03c0", "0", "\u03c0")) +
    scale_color_manual(values= colors) +
    xlab("Error (rad)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
    theme(
      axis.text.x = element_text(color="black", size = 12),
      axis.text.y = element_text(color="black", size = 12),
      plot.title = element_blank(),
      axis.title.x = element_text(color="black", size=14),
      axis.title.y = element_text(color="black", size=14),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  return(plot)
}


## Response Time plot

response_time_predictions <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in 1:length(models)){
  this_model <- model_predictions[model_predictions$model == models[i],]
  this_predictions <- get_response_time_density(this_model)
  response_time_predictions <- rbind(response_time_predictions, this_predictions)
}
colnames(response_time_predictions) <- c("value", "prob", "model")


plot_response_time <- function(data, model, colors){
  plot <- ggplot(data, aes(x=source_RT/1000, y = ..density..)) + geom_histogram(bins = 50,  alpha = 0.9) +
    geom_line(data = model, aes(x = value, y = prob, color = model), size = 1.2, adjust = 2) +
    scale_x_continuous(breaks  = c(0,2, 4, 6), 
                       labels = c("0", "2", "4", "6")) +
    scale_color_manual(values= colors) +
    xlab("Response Time (s)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
    theme(
      axis.text.x = element_text(color="black", size = 12),
      axis.text.y = element_text(color="black", size = 12),
      plot.title = element_blank(),
      axis.title.x = element_text(color="black", size=14),
      axis.title.y = element_text(color="black", size=14),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  return(plot)
}
