# Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('experiment_2.csv')
# Exclude practice block
data <- data[data$block != 0,]

# Exclude first session as a practice sessions
data <- data[data$session != 1,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

# Function to compute angular difference

angle_diff <- function(a,b){
  diff <- atan2(sin(a-b), cos(a-b))
  return(diff)
}

convert_orthographic_similarity <- function(x, length){
  similarity <- (length - x)/length
  return(similarity)
}

get_orthographic_index <- function(data){
  orthographic_similarity <- data.frame(matrix(nrow = nrow(data), ncol = 9))
  orthographic_similarity[,1:9] <- lapply(data[,51:59], convert_orthographic_similarity, length = 4)
  orthographic_similarity[,10] <- rowSums(orthographic_similarity)
  breaks <- quantile(orthographic_similarity[,10], probs = c(0.25, 0.75))
  orthographic_index <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  for(i in 1:nrow(orthographic_similarity)){
    if(orthographic_similarity[i,10] < breaks[[1]]){
      orthographic_index[i, 1] <- 'Low'
    } else if (orthographic_similarity[i,10] > breaks[[2]]){
      orthographic_index[i, 1] <- 'High'
    } else {
      orthographic_index[i, 1] <- 'Medium'
    }
  }
  colnames(orthographic_index) <- 'ortho_bin'
  return(orthographic_index)
}

load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/exp_2_sim_data_updated.RData")

# Rename models and concatenate simulated data
#sim_no_intrusion$model <- 'Pure Guess'
sim_flat_intrusion$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_SxT$model <- 'Spatiotemporal'
sim_ortho$model <- 'Orthographic'
sim_semantic$model <- 'Semantic'

model_predictions <- rbind(sim_flat_intrusion, sim_temporal,
                           sim_SxT, sim_ortho, sim_semantic)
idx <- get_orthographic_index(data)
data[,ncol(data)+1] <- idx
data$ortho_bin_f <- factor(data$ortho_bin, levels = c('Low', 'Medium', 'High'))

model_predictions$ortho_bin <- rep(idx[rep(seq_len(nrow(idx)), each = 5), ],5)
model_predictions$ortho_bin_f <- factor(model_predictions$ortho_bin, levels = c('Low', 'Medium', 'High'))


ggplot() +
  geom_histogram(data = data, aes(x = response_error, y=..density..)) +
  geom_density(data = model_predictions, aes(x = simulated_error, col = model), alpha = 0.4, position = "identity") +
  facet_wrap(participant ~ ortho_bin_f, ncol = 3)