# Plot individual data histograms

# Data Processing
# - Load in data
setwd("~/git/sourcemem_online/analysis/models/R/data")
data <- read.csv('sourcemem_data_2021.csv')

# - Exclude practice block
data <- data[data$block!= 0,]

# - Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

participant <- unique(data$participant)

for(i in 1:length(participant)){
  filename = sprintf('histogram_%s.png', i)
  png(file= filename,
      width=600, height=350)
  plot_title <- sprintf('Participant %s', i)
  plot <- hist(data[data$participant == i,]$response_error, breaks= 50, freq=FALSE,
               main=plot_title)
  dev.off()
}
