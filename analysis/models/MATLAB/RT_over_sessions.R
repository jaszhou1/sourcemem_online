# Have a look at response time distributions across sessions
library(ggplot2)
setwd("~/git/sourcemem_online/analysis/models/MATLAB")
data <- read.csv('experiment_2_cutoff.csv')
data <- data[data$valid_RT == TRUE,]
data$source_RT <- data$source_RT/1000

setwd("~/git/sourcemem_online/analysis/plots/diffusion")
plot <- ggplot() +
  geom_histogram(data = data, aes(x = source_RT), bins = 50) +
  facet_wrap(~participant + session, ncol = 10) +
  theme(text = element_text(size = 20))
ggsave(filename = 'RTs_sessions.png', plot, width = 100, height = 30, units = "cm")

participants <- unique(data$participant)
sessions <- unique(data$session)
RTs <- data.frame(matrix(nrow = length(participants)*length(sessions), ncol = 4))
idx <- 1
for(i in participants){
  for(j in sessions){
    this_data <- data[(data$participant == i) & (data$session == j),]
    this_median <- median(this_data$source_RT)
    this_sd <- sd(this_data$source_RT)
    RTs[idx,] <- c(this_median, this_sd, i, j)
    idx <- idx + 1
  }
}
colnames(RTs) <- c('median', 'sd', 'participant', 'session')
