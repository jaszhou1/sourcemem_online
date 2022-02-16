# Load in best fits of the models
setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
load("2022-01-14.RData")

# Get a number of equally spaced colours
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# color_wheel <- gg_color_hue(5)

color_wheel <- c('#00468BFF',
                 '#ED0000FF',
                 '#42B540FF',
                 '#0099B4FF',
                 '#925E9FFF')

sim_pure_intrusion$model <- 'Pure Intrusion'
sim_mix$model <- 'Pure Guess'
sim_intprec$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_spatiotemporal$model <- 'Spatiotemporal'

model_predictions <- rbind(sim_mix, sim_pure_intrusion, sim_intprec, sim_temporal, sim_spatiotemporal)
models <- unique(model_predictions$model)

# Transform data into (wrapped) density 

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$simulated_error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian")
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

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  "Pure Guess"= color_wheel[1],
  "Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[3],
  "Temporal" = color_wheel[4],
  "Spatiotemporal" = color_wheel[5]
)

## Compute variables required for chart layout.

AXIS.CEX <- 1.2
AXIS.LABEL.CEX <- 1.5
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 0.8


# Plot Response Error 
plot_response_error <- function(model_list, data, filename){
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
    points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2.5, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  mtext(paste("Response Error (rad)"), side=1, cex= AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  dev.off()
}


## Functions to plot average error across serial positions

serial_position <- function(position, data){
  this_position_data <- data[data$present_trial == position,]
  this_position_absolute_error <- abs(this_position_data$response_error)
  avg_absolute_error <- mean(this_position_absolute_error)
  return(avg_absolute_error)
}

sim_serial_position <- function(position, data){
  this_position_data <- data[data$target_position == position,]
  this_position_absolute_error <- abs(as.numeric(this_position_data$simulated_error))
  avg_absolute_error <- mean(this_position_absolute_error)
  return(avg_absolute_error)
}

get_average_error_across_position <- function(this_data, model_string){
  this_average_error <- data.frame(matrix(nrow = 10, ncol = 3))
  colnames(this_average_error) <- c('position','error','model')
  this_average_error[,3] <- model_string
  for (i in 1:10){
    this_average_error[i,1] <- i
    this_average_error[i,2] <- sim_serial_position(i, this_data)
  }
  return(this_average_error)
}

concatenate_model_average_error <- function(){
  mix <- get_average_error_across_position(sim_mix, 'Pure Guess')
  pure_int <- get_average_error_across_position(sim_pure_intrusion, 'Pure Intrusion')
  int <- get_average_error_across_position(sim_intprec, 'Intrusion + Guess')
  temp <- get_average_error_across_position(sim_temporal, 'Temporal')
  spatio <- get_average_error_across_position(sim_spatiotemporal, 'Spatiotemporal')
  res <- rbind(mix, pure_int, int, temp, spatio)
  return(res)
}

errors_across_serial_position <- function(data){
  serial_position_data <- data.frame(matrix(nrow = 10, ncol = 3))
  colnames(serial_position_data) <- c('position','error','model')
  serial_position_data[,3] <- 'data'
  for (i in 1:10){
    serial_position_data[i,1] <- i
    serial_position_data[i,2] <- serial_position(i, data)
  }
  models <- concatenate_model_average_error()
  plot <- ggplot(data=serial_position_data, aes(x=position, y = error)) +
    geom_point(size = 3) +
    geom_point(data = models, aes(x = position, color = model)) +
    geom_smooth(data = models, method = 'loess', se = FALSE, linetype="solid", aes(x = position, color = model), span = 1.5) +
    scale_color_manual(values=c('#42B540FF',
                                '#00468BFF',
                                '#ED0000FF',
                                '#925E9FFF',
                                '#0099B4FF')) +
    scale_x_continuous(name = 'Study List Position', breaks = 1:10) +
    scale_y_continuous(name = 'Average Absolute Error (rad)', breaks = c(1, 1.05), limits = c(0.95, 1.1)) +
    guides(color= guide_legend(title="Model")) +
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
      legend.key = element_rect(colour = "transparent", fill = "white"),
      legend.text=element_text(size= 12),
      axis.line = element_line(colour = "black")
    )
  return(plot)
}