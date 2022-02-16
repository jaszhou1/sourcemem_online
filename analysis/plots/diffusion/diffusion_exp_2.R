# Plot Experiment 2 Diffusion

## Read in, and transform data as required. ##

# Plot marginal response error and time model predictions against data
setwd("~/git/sourcemem_online/analysis/models/MATLAB/")
data <- read.csv('experiment_2.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

setwd("~/git/sourcemem_online/analysis/models/MATLAB/experiment_2")

pure_guess <- read.csv('sim_pure_guess.csv', header = FALSE)
pure_guess$model <- 'Pure Guess'

pure_intrusion <- read.csv('sim_pure_intrusion.csv', header = FALSE)
pure_intrusion$model <- 'Pure Intrusion'

intrusion <- read.csv('sim_flat.csv', header = FALSE)
intrusion$model <- 'Intrusion + Guess'

temporal <- read.csv('sim_temporal.csv', header = FALSE)
temporal$model <- 'Temporal Gradient'

spatiotemporal <- read.csv('sim_spatiotemporal.csv', header = FALSE)
spatiotemporal$model <- 'Spatiotemporal Gradient'

orthographic <- read.csv('sim_ortho.csv', header = FALSE)
orthographic$model <- 'Orthographic'

semantic <- read.csv('sim_semantic.csv', header = FALSE)
semantic$model <- 'Semantic'

add <- read.csv('sim_add.csv', header = FALSE)
add$model <- 'Four Factor (Additive)'

multi <- read.csv('sim_multi.csv', header = FALSE)
multi$model <- "Four Factor (Multiplicative)"



model_predictions <- rbind(pure_guess, pure_intrusion, intrusion, temporal, spatiotemporal,
                           orthographic, semantic, add, multi)
colnames(model_predictions) <- c('error', 'time', 'participant', 'model')

models <- unique(model_predictions$model)