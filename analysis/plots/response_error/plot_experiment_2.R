# Load in best fits of the models
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/exp_2_simulated_data.RData")

# Rename models and concatenate simulated data
sim_no_intrusion$model <- 'Pure Guess'
sim_flat_intrusion$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_SxT$model <- 'Spatiotemporal'
sim_orthographic$model <- 'Orthographic'
sim_semantic$model <- 'Semantic'
sim_SxTpOxSe$model <- 'Four Factor (Additive)'
sim_all_x$model <- 'Four Factor (Multiplicative)'
sim_SxT_recog$model <- 'Unrecognised = Guesses'

model_predictions <- rbind(sim_no_intrusion, sim_flat_intrusion, sim_temporal,
                           sim_SxT, sim_orthographic, sim_semantic, sim_SxTpOxSe, 
                           sim_all_x, sim_SxT_recog)
models <- unique(model_predictions$model)

# Load in data
data <- read.csv("~/git/sourcemem_online/analysis/models/R/data/experiment_2.csv")
if(min(data$present_trial == 0)){
  data$present_trial <- data$present_trial + 1
}

if(min(data$block == -1)){
  data$block <- data$block + 1
}

# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

participants <- unique(data$participant)

color_wheel <- c('#00468BFF',
                 #'#ED0000FF', no pure intrusion, maybe need to add?
                 '#42B540FF',
                 '#0099B4FF',
                 '#925E9FFF',
                 '#FDAF91FF',
                 '#AD002AFF',
                 '#ADB6B6FF',
                 '#1B1919FF',
                 '#ED0000FF') #Find new colour for the recognition model maybe

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$simulated_error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian")
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:50] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-50):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  this_predictions[4] <- model$participant[1]
  return(this_predictions)
}

response_error_predictions <- data.frame(matrix(ncol = 4, nrow = 0))
for(i in 1:length(models)){
  for(j in participants){
    this_model <- model_predictions[(model_predictions$model == models[i]) & (model_predictions$participant == j),]
    this_predictions <- get_response_error_density(this_model)
    response_error_predictions <- rbind(response_error_predictions, this_predictions)
  }
}
colnames(response_error_predictions) <- c("value", "prob", "model", "participant")

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  "Pure Guess"= color_wheel[1],
  #"Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[2],
  "Temporal" = color_wheel[3],
  "Spatiotemporal" = color_wheel[4],
  "Orthographic" = color_wheel[5],
  "Semantic" = color_wheel[6],
  "Four Factor (Additive)" = color_wheel[7],
  "Four Factor (Multiplicative)" = color_wheel[8],
  "Unrecognised = Guesses" = color_wheel[9]
)

individual_response_error_plot <- function(model_list, data, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    pdf(file=filename, width=7, height=10)
  }
  
  ## Overall charting parameters.
  NUM.BINS <- 50
  
  ## Get the summary variables from the data.
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS)
  PARTICIPANTS.PER.ROW <- 2
  
  ## Compute variables required for chart layout.
  AXIS.CEX <- 1
  AXIS.LABEL.CEX <- 1.1
  
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW)
  NUM.COLS <- PARTICIPANTS.PER.ROW 
  
  X.RESP.LOW <- -pi - 0.01
  X.RESP.HI <- pi + 0.01
  Y.RESP.LOW <- 0.0
  Y.RESP.HI <- 2.5
  
  ## Set up the global presentation parameters for the plot.
  par(mfrow=c(NUM.ROWS, NUM.COLS))
  par(mar=c(0.5, 0.5, 0, 0.5),
      oma=c(4, 3.5, 0, 3.5))
  
  ## Iterate through each participant...
  for(p.idx in 1:NUM.PARTICIPANTS) {
    p <- PARTICIPANTS[p.idx]
    
    ## Get the participant's data.
    p.data <- data[(data$participant==p), ]
    p.model <- response_error_predictions[response_error_predictions$participant == p,]
    
    ## Plot marginal response proportion for participant.
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, Y.RESP.HI))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(p.data$response_error,
                      breaks=NUM.BINS, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    for(model.type in MODEL.TYPES[model_list]) {
      model.data <- p.model[p.model$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2.5, col=MODEL.COL[[model.type]])
    }
    
    ## Plot the participant number and data type
    mtext(paste0("P", p), side=3, cex=AXIS.LABEL.CEX, line=-2, adj=0.2)
    
    ## Plot the x axes (for the last two participants only)
    if(p %in% tail(PARTICIPANTS, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
      mtext(paste("Response error (rads)"), side=1, cex=AXIS.LABEL.CEX, line=2.5)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if((p.idx %% PARTICIPANTS.PER.ROW) == 1) {
      axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
    }
    
    ## Put the outer margin axis labels.
    mtext("Error density", side=2, line=2, outer=TRUE, cex = AXIS.LABEL.CEX)
    
    ## If we're writing to a file (i.e. a PDF), close the device.
    if(filename != "") {
      dev.off()
    }
  }
}

individual_recentered_error_plot <- function(model_list, data, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    pdf(file=filename, width=7, height=10)
  }
  
  ## Overall charting parameters.
  NUM.BINS <- 50
  
  ## Get the summary variables from the data.
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS)
  PARTICIPANTS.PER.ROW <- 2
  
  ## Compute variables required for chart layout.
  AXIS.CEX <- 1
  AXIS.LABEL.CEX <- 1.1
  
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW)
  NUM.COLS <- PARTICIPANTS.PER.ROW 
  
  X.RESP.LOW <- -pi - 0.01
  X.RESP.HI <- pi + 0.01
  Y.RESP.LOW <- 0.0
  Y.RESP.HI <- 1
  
  ## Set up the global presentation parameters for the plot.
  par(mfrow=c(NUM.ROWS, NUM.COLS))
  par(mar=c(0.5, 0.5, 0, 0.5),
      oma=c(4, 3.5, 0, 3.5))
  
  ## Iterate through each participant...
  for(p.idx in 1:NUM.PARTICIPANTS) {
    p <- PARTICIPANTS[p.idx]
    
    # insert actual recentered plot here
    
    ## Plot the participant number and data type
    mtext(paste0("P", p), side=3, cex=AXIS.LABEL.CEX, line=-2, adj=0.2)
    
    ## Plot the x axes (for the last two participants only)
    if(p %in% tail(PARTICIPANTS, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
      mtext(paste("Response error (rads)"), side=1, cex=AXIS.LABEL.CEX, line=2.5)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if((p.idx %% PARTICIPANTS.PER.ROW) == 1) {
      axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
    }
    
    ## Put the outer margin axis labels.
    mtext("Error density", side=2, line=2, outer=TRUE, cex = AXIS.LABEL.CEX)
    
    ## If we're writing to a file (i.e. a PDF), close the device.
    if(filename != "") {
      dev.off()
    }
  }
}

## Recentered plot
get_asymmetric_recenter <- function(){
  directions <- unique(recentered_all$direction)
  lags <- unique(recentered_all$filter)
  asymm_predictions <- data.frame(matrix(ncol = 6, nrow = 0))
  for(h in PARTICIPANTS){
    for(i in 1:length(models)){
      for(j in directions){
        for(k in lags){
          this_model <- model_predictions[(model_predictions$participant == h)&
                                            (model_predictions$model == models[i])&
                                            (model_predictions$direction == j)&
                                            (model_predictions$filter == k),]
          this_predictions <- get_response_error_density(this_model)
          this_predictions[,4] <- j
          this_predictions[,5] <- k
          this_predictions[,6] <- h
          # Add on tags for lag and direction
          ## HERE
          asymm_predictions <- rbind(asymm_predictions, this_predictions)
        }
      }
    }
  }
  colnames(asymm_predictions) <- c("value", "prob", "model", "direction", "lag")
  return(asymm_predictions)
}

recenter_data <- recentered_all[recentered_all$model == 'data',]
recenter_model <- recentered_all[recentered_all$model != 'data',]

recentered_model_densities <- get_asymmetric_recenter(recenter_model)