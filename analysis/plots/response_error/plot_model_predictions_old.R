# Load in best fits of the models
load("~/git/sourcemem_online/analysis/models/R/model_code/2022-01-14.RData")
library(ggplot2)
# Get a number of equally spaced colours
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

color_wheel <- gg_color_hue(5)

sim_pure_intrusion$model <- 'Pure Intrusion'
sim_mix$model <- 'Pure Guess'
sim_intprec$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_spatiotemporal$model <- 'Spatiotemporal'

plot_data <- function(data, model, colors){
  plot <- ggplot(data, aes(x=response_error, y = ..density..)) + geom_histogram(bins = 30,  alpha = 0.9) +
    geom_density(data = model, aes(x = simulated_error, color = model), size = 1.2, adjust = 2) +
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

# Plot 1
plot_1_models <- rbind(sim_mix, sim_pure_intrusion, sim_intprec)
plot_1 <- plot_data(data, plot_1_models, color_wheel[1:3])

plot_2_models <- rbind(sim_intprec, sim_temporal, sim_spatiotemporal)
plot_2 <- plot_data(data, plot_2_models, color_wheel[3:5])


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
    geom_point(size = 2) +
    geom_smooth(data = models, method = 'loess', se = FALSE, aes(x = position, color = model)) +
    scale_x_continuous(name = 'Study List Position', breaks = 1:10) +
    scale_y_continuous(name = 'Average Absolute Error (rad)', breaks = c(1, 1.05), limits = c(0.95, 1.1)) +
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

geom_density(data = model, aes(x = simulated_error, color = model), size = 1.2, adjust = 2)