library(jsonlite)
setwd("~/GitHub/sourcemem_online/experiment/stimuli")

wordlist <- read.csv('word_list_freq6_manual.csv', fileEncoding="UTF-8-BOM")

# Randomise order (for session split)
set.seed(42)
words <- as.character(wordlist[sample(nrow(wordlist)),])
jsonwords <- toJSON(words, pretty = TRUE)
