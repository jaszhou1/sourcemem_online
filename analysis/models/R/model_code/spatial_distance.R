## spatial_distance.R
# 
# The purpose of this function is to append distances between a target and each of the non-targets in its block
# We aren't using the signed angular distance, but the 1-cos() metric. In Philip's words:

# It's a fairly common distance function in circular statistics. 
# It's well behaved as a distance metric: it runs from 0 when theta = phi to a maximum of 2 when they are antipodal. 
# There are other functions that would probably do as well, but if this one works it would have the advantage of 
# linking with the category model in the circular diffusion model, which would be a nice theoretical tie-up.

# The data should already have a set of $offset columns which expresses the raw, signed angular difference in rads
# This function instead calculates the distance in the 1-cos() metric for the purposes of Shepard's law for percieved distance

# Function to find the 1-cos(phi-theta) distance between each target and the non-targets associated with it

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
