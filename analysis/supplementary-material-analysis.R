# Compare parameter estimates of models across presentation conditions
setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")

pure_guess <- read.csv('threshold_simseq.csv', stringsAsFactors = FALSE, fileEncoding='UTF-8-BOM')
pure_intrusion <- read.csv('intrusion_simseq.csv', stringsAsFactors = FALSE, 
                           fileEncoding='UTF-8-BOM')
flat_intrusion <- read.csv('flat_simseq.csv', stringsAsFactors = FALSE, 
                           fileEncoding='UTF-8-BOM')

compare_descript <- function(pest){
  sim <- pest[pest$cond == 'Simultaneous',]
  seq <- pest[pest$cond == 'Sequential',]
  
  # Memory Precision
  mean_prec1_sim <- mean(sim$prec1)
  sd_prec1_sim <- sd(sim$prec1)
  mean_prec1_seq <- mean(seq$prec1)
  sd_prec
}