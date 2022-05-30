# Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('experiment_2.csv') # Experiment 2

#data <- read.csv('experiment_1.csv') # Experiment 1


# Exclude practice block
data <- data[data$block != 0,]

# Exclude first session as a practice sessions
# data <- data[data$session != 1,]
library(extraDistr)
library(R.utils)
library(CircStats)
# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

source('~/git/sourcemem_online/analysis/models/R/model_code/temporal_gradient_model_flat_guesses.R')

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

prec1 <- 15
prec2 <- 10
gamma <- 0.8
kappa <- 0.5
lambda_b <- 1.5
lambda_f <- 1.5
beta <- 0.1

parameters <- data.frame(matrix(nrow = 3, ncol = 7))
parameters[1,] <- c(prec1, prec2, gamma, kappa, lambda_b, lambda_f, beta)
parameters[2,] <- c(prec1, prec2, gamma, 0.7, lambda_b, lambda_f, beta)
parameters[3,] <- c(prec1, prec2, gamma, 0.7, lambda_b, 0.5, beta)
colnames(parameters) <- c('prec1', 'prec2', 'gamma', 'kappa', 'lambda_b', 'lambda_f', 'beta')

equal <- simulate_temporal_model(1, data, parameters[1,])
tau <- simulate_temporal_model(1, data, parameters[2,])
lambda <- simulate_temporal_model(1, data, parameters[3,])

source('~/git/sourcemem_online/analysis/models/R/model_code/recenter_exp2.R')
recentered_equal <- generate_recentered_model(equal, 'Equal')
recentered_tau <- generate_recentered_model(tau, 'Tau')
recentered_lambda <- generate_recentered_model(lambda, 'Lambda')

recentered_all <- rbind(recentered_equal, recentered_tau, recentered_lambda)

## Plotting stuff

AXIS.CEX <- 1.2
AXIS.LABEL.CEX <- 1.5
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 0.25

MODEL.TYPES <- unique(as.character(recentered_all$model))

#  Function to transform data into (wrapped) density 

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian", adjust = 1.5)
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:75] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-75):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  colnames(this_predictions) <- c("value", "prob", "model")
  return(this_predictions)
}

models <- unique(recentered_all$model)
directions <- unique(recentered_all$direction)
lags <- unique(recentered_all$filter)
asymm_predictions <- data.frame(matrix(ncol = 5, nrow = 0))
for(i in 1:length(models)){
  for(j in directions){
    for(k in lags){
      this_model <- recentered_all[(recentered_all$model == models[i])&
                                        (recentered_all$direction == j)&
                                        (recentered_all$filter == k),]
      this_predictions <- get_response_error_density(this_model)
      this_predictions[,4] <- j
      this_predictions[,5] <- k
      # Add on tags for lag and direction
      ## HERE
      asymm_predictions <- rbind(asymm_predictions, this_predictions)
    }
  }
}
colnames(asymm_predictions) <- c("value", "prob", "model", "direction", "lag")
asymm_predictions$direction_f <- factor(asymm_predictions$direction, levels = c('forwards', 'backwards'))
asymm_predictions$model_f <- factor(asymm_predictions$model, levels = c('Equal', 'Tau', 'Lambda'))
ggplot(data = asymm_predictions, aes(x = value, y = prob, lty = model_f, color = model_f)) +  
  geom_line(lwd = 1.2) + facet_wrap(~ direction_f + lag) +
  scale_color_manual(values=c("#009E73",
                              "#0072B2",
                              "#D55E00")) +
  scale_x_continuous(name = 'Response Error', 
                     breaks=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi))) +
  scale_y_continuous(name = 'Density', breaks = c(0.2, 0.4)) +
  theme(
    axis.text.x = element_text(color="black", size = 18),
    axis.text.y = element_text(color="black", size = 18),
    plot.title = element_blank(),
    axis.title.x = element_text(color="black", size=20),
    axis.title.y = element_text(color="black", size=20),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    #legend.key = element_blank(),
    #legend.text=element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(colour = 'white')
  )

ggsave(file='asymm_predictions.png', width=10.7, height=4, units = "in")