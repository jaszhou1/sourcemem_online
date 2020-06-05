library(jsonlite)
setwd("~/GitHub/sourcemem_online/experiment/stimuli")

wordlist <- read.csv('wordlist.csv', fileEncoding="UTF-8-BOM")
words <- as.character(wordlist$word)
jsonwords <- toJSON(words, pretty = TRUE)