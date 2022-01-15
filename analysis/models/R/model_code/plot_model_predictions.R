# Load in best fits of the models
load("~/git/sourcemem_online/analysis/models/R/model_code/2022-01-10.RData")

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