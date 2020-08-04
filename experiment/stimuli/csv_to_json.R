library(jsonlite)
setwd("~/GitHub/sourcemem_online/experiment/stimuli")

wordlist <- read.csv('wordlist_freq7.csv', fileEncoding="UTF-8-BOM")

# Randomise order (for session split)
set.seed(42)
words <- as.character(wordlist[sample(nrow(wordlist)),])

# Number of sessions to split stimuli into
num_sessions <- 3

# Function to split character array into sessions
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
sessions <- chunk(words,num_sessions)

jsonwords <- toJSON(words, pretty = TRUE)

json_s1 <- toJSON(sessions[1], pretty = TRUE)
json_s2 <- toJSON(sessions[2], pretty = TRUE)
json_s3 <- toJSON(sessions[3], pretty = TRUE)


