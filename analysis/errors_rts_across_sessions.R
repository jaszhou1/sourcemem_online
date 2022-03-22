setwd("~/git/sourcemem_online/analysis/models/MATLAB/")
data <- read.csv('experiment_2_cutoff.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]
participants <- unique(data$participant)
sessions <- unique(data$session)

res <- data.frame()
for(i in participants){
  for (j in sessions){
    this_data <- data[(data$participant == i) & (data$session == j),]
    error_mean <- mean(abs(this_data$response_error))
    error_sd <- sd(abs(this_data$response_error))
    rt_median <- median(this_data$source_RT)
    rt_sd <- sd(this_data$source_RT)
    this_line <- c(error_mean, error_sd, rt_median, rt_sd, i, j)
    res <- rbind(res, this_line)
  }
}
colnames(res) <- c('error_mean', 'error_sd', 'rt_median', 'rt_sd', 'participant', 'session')

ggplot(res) + geom_point(aes(x = session, y = error_mean)) + facet_wrap(participant)