# Append orthographic and semantic similarity values to the dataset
library(stringdist)
library(rjson)
library(lsa)

# Load in semantic vectors
setwd("~/git/sourcemem_models/BaysCataloHussain/data/word2vec")

# Load in word2vec semantic vectors
word2vec <- fromJSON(file = 'word2vec_filtered.json')
n_trials <- 10

cosine_distance <- function(theta, phi){
  distance <- 1 - cos(theta - phi)
  return(distance)
}

append_distances <- function(data){
  distances <- c()
  for (i in 1:nrow(data)){
    this_target <- data$target_angle[i]
    this_intrusions <- data[i, 14:22]
    this_trial_distances <- cosine_distance(this_target, this_intrusions)
    distances <- rbind(distances, this_trial_distances)
  }
  
  colnames(distances) <- c("distance_1", "distance_2", "distance_3", "distance_4", 
                           "distance_5", "distance_6", "distance_7", "distance_8", 
                           "distance_9")
  
  data <- cbind(data, distances)
  return(data)
}

append_orthographic_semantic <- function(data){
  # First, add the spatial distance
  data <- append_distances(data)
  
  # Then, append the orthographic levenshtein distance, and the semantic cosine similarity.
  orthographic_similarities <- c()
  semantic_similarities <- c()
  for(i in 1:nrow(data)){
    this_word <- data$word[i]
    this_trial <- data$present_trial[i]
    this_block <- data$block[i]
    this_session <- data$session[i]
    this_participant <- data$participant[i]
    
    this_block_data <- data[(data$participant == this_participant) & (data$session == this_session) & (data$block == this_block),]
    
    trials <- 1:n_trials
    non_targets <- trials[trials!=this_trial]
    
    this_block_orthographic <- c()
    this_block_semantic <- c()
    idx <- 1
    for (j in non_targets){
      this_intruding_word <- this_block_data$word[j]
      
      # Find the levenshtein distance between the target word and intruding word
      # Since all stimuli are 4 letters, the maximum distance is 4 (swapping all letters)
      str_dist = stringdist(this_word, this_intruding_word, method = 'lv')/4
      this_block_orthographic[idx] <- str_dist
      if (is.null(word2vec[[tolower(this_intruding_word)]])){
        browser()
      }
      semantic_dist = cosine(word2vec[[tolower(this_word)]], word2vec[[tolower(this_intruding_word)]])
      
      # Truncate semantic similarity at 0, per Adam's email (alternative is to shift all values by largest minimum value)
      if (semantic_dist < 0){
        semantic_dist <- 0
      }
      
      this_block_semantic[idx] <- semantic_dist
      idx <- idx + 1
    }
    orthographic_similarities <- rbind(orthographic_similarities, this_block_orthographic)
    semantic_similarities <- rbind(semantic_similarities, this_block_semantic)
  }
  colnames(orthographic_similarities) <- c("orthographic_1", "orthographic_2", "orthographic_3", "orthographic_4", 
                                           "orthographic_5", "orthographic_6", "orthographic_7", "orthographic_8", "orthographic_9")
  colnames(semantic_similarities) <- c("semantic_1", "semantic_2", "semantic_3", "semantic_4", 
                                       "semantic_5", "semantic_6", "semantic_7", "semantic_8", "semantic_9")
  data <- cbind(data, orthographic_similarities, semantic_similarities)
  return(data)
}