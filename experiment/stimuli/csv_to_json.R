library(jsonlite)
setwd("~/GitHub/sourcemem_online/experiment/stimuli")

wordlist <- read.csv('wordlist.csv', fileEncoding="UTF-8-BOM")
jsonwords <- toJSON(wordlist)