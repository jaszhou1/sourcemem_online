library(ggplot2)

load("~/git/sourcemem_online/analysis/models/R/model_code/2022-01-13_recentered_exp1.RData")

# Get a number of equally spaced colours
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

color_wheel <- gg_color_hue(5)


## Plot just the data
plot_data <- function(data){
  plot <- ggplot(data, aes(x=error, y = ..density..)) + geom_histogram(bins = 30,  alpha = 0.9) +
    scale_x_continuous(breaks  = c(-pi, 0, pi), 
                       labels = c("-\u03c0", "0", "\u03c0")) +
    ggtitle('Distances between Response and Non-Target Angles') + xlab("Error (rad)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
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

plot_data_asymm <- function(data){
  plot <- ggplot(data, aes(x=error, y = ..density..)) + geom_histogram(bins = 30,  alpha = 0.9) +
    facet_wrap(~  direction + filter) + 
    scale_x_continuous(breaks  = c(-pi, 0, pi), 
                       labels = c("-\u03c0", "0", "\u03c0")) +
    ggtitle('Distances between Response and Non-Target Angles by Direction and Lag') + xlab("Error (rad)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
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

## Plot comparison of intrusion and no intrusion
compare_model <- function(data, models, title, colors){
  plot <- ggplot(data, aes(x=error, y = ..density..)) + geom_histogram(bins = 30,  alpha = 0.9) +
    geom_density(data = models, aes(color = model), size = 1.2, adjust = 2) +
    scale_x_continuous(breaks  = c(-pi, 0, pi), 
                       labels = c("-\u03c0", "0", "\u03c0")) +
    scale_color_manual(values= colors) +
    ggtitle(title) + xlab("Error (rad)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
    guides(size = FALSE) +
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

compare_model_asymm <- function(data, models, title, colors){
  plot <- ggplot(data, aes(x=error, y = ..density..)) + geom_histogram(bins = 30,  alpha = 0.9) +
    geom_density(data = models, aes(color = model), size = 1.2, adjust = 2) +
    scale_x_continuous(breaks  = c(-pi, 0, pi), 
                       labels = c("-\u03c0", "0", "\u03c0")) +
    scale_color_manual(values= colors) +
    facet_wrap(~  direction + filter) + 
    ggtitle(title) + xlab("Error (rad)") + ylab("Density") + expand_limits(y = c(0, 0.20)) +
    guides(size = FALSE) +
    theme(
      axis.text.x = element_text(color="black", size = 16),
      axis.text.y = element_text(color="black", size = 14),
      plot.title = element_blank(),
      axis.title.x = element_text(color="black", size=16),
      axis.title.y = element_text(color="black", size=16),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  return(plot)
}


data <- recentered_all[recentered_all$model == 'data',]

plot_1_model_strings <- c("Pure Guess", "Pure Intrusion")
plot_1_models <- recentered_all[recentered_all$model %in% plot_1_model_strings,]
compare_model(data,plot_1_models, 'Pure Threshold and Intrusion Recentered Predictions', c(color_wheel[2], color_wheel[3]))

# plot_2_model_strings <- c("Pure Guess", "Intrusion + Guess")
# plot_2_models <- recentered_all[recentered_all$model %in% plot_2_model_strings,]
# compare_model(data,plot_1_models, 'Pure Threshold and Intrusion + Guess Recentered Predictions',c(color_wheel[1], color_wheel[3]))

plot_3_model_strings <- c("Intrusion + Guess", "Temporal Gradient", "Spatiotemporal Gradient")
plot_3_models <- recentered_all[recentered_all$model %in% plot_3_model_strings,]
compare_model(data,plot_3_models, 'Intrusion + Guess and Gradient Recentered', c(color_wheel[3], color_wheel[4], color_wheel[5]))

plot_3_model_strings <- c("Intrusion + Guess", "Temporal Gradient", "Spatiotemporal Gradient")
plot_3_models <- recentered_all[recentered_all$model %in% plot_3_model_strings,]
compare_model_asymm(data,plot_3_models, 'Recentered Asymmetry', c(color_wheel[3], color_wheel[4], color_wheel[5]))




