# Load in best fits of the models
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/exp_2_sim_data_updated.RData")

# Rename models and concatenate simulated data
#sim_no_intrusion$model <- 'Pure Guess'
sim_flat_intrusion$model <- 'Intrusion + Guess'
sim_temporal$model <- 'Temporal'
sim_SxT$model <- 'Spatiotemporal'
sim_ortho$model <- 'Orthographic'
sim_semantic$model <- 'Semantic'
sim_SxTpOxSe$model <- 'Four Factor (Additive)'
sim_all_x$model <- 'Four Factor (Multiplicative)'
sim_SxT_recog$model <- 'Unrecognised = Guesses'

model_predictions <- rbind(sim_flat_intrusion, sim_temporal,
                           sim_SxT, sim_ortho, sim_semantic, sim_SxTpOxSe, 
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

## Overall charting parameters.
NUM.BINS <- 50
AXIS.CEX <- 1.3
AXIS.LABEL.CEX <- 1.1
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 2.5

color_wheel <- c(#'#00468BFF',
  #'#ED0000FF', no pure intrusion, maybe need to add?
  '#42B540FF',
  '#0099B4FF',
  '#925E9FFF',
  '#FDAF91FF',
  '#AD002AFF',
  '#8F7700FF',
  '#80796BFF',
  '#ED0000FF') #Find new colour for the recognition model maybe

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$simulated_error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian")
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:75] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-75):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
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

group_response_error_predictions <- data.frame(matrix(ncol = 4, nrow = 0))
for(i in 1:length(models)){
  this_model <- model_predictions[model_predictions$model == models[i],]
  this_predictions <- get_response_error_density(this_model)
  group_response_error_predictions <- rbind(group_response_error_predictions, this_predictions)
  group_response_error_predictions[4] <- 'Average'
}
colnames(group_response_error_predictions ) <- c("value", "prob", "model", "participant")

# Assign models to colours
MODEL.TYPES <- unique(as.character(response_error_predictions$model))
MODEL.COL <- list(
  #"Pure Guess"= color_wheel[1],
  #"Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[1],
  "Temporal" = color_wheel[2],
  "Spatiotemporal" = color_wheel[3],
  "Orthographic" = color_wheel[4],
  "Semantic" = color_wheel[5],
  "Four Factor (Additive)" = color_wheel[6],
  "Four Factor (Multiplicative)" = color_wheel[7],
  "Unrecognised = Guesses" = color_wheel[8]
)

individual_response_error_plot <- function(model_list, data, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  ## Compute variables required for chart layout.
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS) #Adding one for the average plot
  PARTICIPANTS.PER.ROW <- 2
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW)
  NUM.COLS <- PARTICIPANTS.PER.ROW 
  NUM.BINS <- 50
  
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
    
    ## Plot response error for participant.
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
  }
  ## Put the outer margin axis labels.
  mtext("Error density", side=2, line=2, outer=TRUE, cex = AXIS.LABEL.CEX)
  mtext("Recentered density", side=4, line=2, outer=TRUE, cex = AXIS.LABEL.CEX)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  ## If we're writing to a file (i.e. a PDF), close the device.
  if(filename != "") {
    dev.off()
  }
}


individual_combined_plot <- function(model_list, data, model_predictions, recenter_data, recenter_model,
                                     group_response_error_predictions, group_recentered, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=10, units = "in", pointsize = 14, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  ## Compute variables required for chart layout.
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS) #Adding one for the average plot
  PARTICIPANTS.PER.ROW <- 2
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW)
  NUM.COLS <- PARTICIPANTS.PER.ROW * 2
  
  ## Set up the global presentation parameters for the plot.
  par(mfrow=c(NUM.ROWS, NUM.COLS))
  par(mar=c(0.5, 0.5, 0, 0.5),
      oma=c(4, 3.5, 3.5, 3.5))
  
  ## Iterate through each participant...
  for(p.idx in 1:NUM.PARTICIPANTS) {
    p <- PARTICIPANTS[p.idx]
    
    ## Get the participant's data.
    p.data <- data[(data$participant==p), ]
    p.model <- model_predictions[model_predictions$participant == p,]
    
    ## Plot response error for participant.
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
    if(p %in% tail(1:NUM.PARTICIPANTS+1, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if((p.idx %% PARTICIPANTS.PER.ROW) == 1) {
      axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
    }
    
    ## Plot Recentered Error for participant.
    ## Get the participant's data.
    p.recenter.data <- recenter_data[(recenter_data$participant==p), ]
    p.recenter.model <- recenter_model[recenter_model$participant == p,]
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, 0.25))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(p.recenter.data$error,
                      breaks=NUM.BINS, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    for(model.type in MODEL.TYPES[model_list]) {
      model.data <- p.recenter.model[p.recenter.model$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2, col=MODEL.COL[[model.type]])
    }
    
    ## Plot the x axes (for the last two participants only)
    if(p %in% tail(1:NUM.PARTICIPANTS+1, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    ## Plot the y axes (for the participants in the second col)
    if((p.idx %% PARTICIPANTS.PER.ROW) == 0) {
      axis(side=4, at=c(0, 0.25), labels = c("0", "0.3"), cex.axis=AXIS.CEX)
    }
  }
  ## Plot Participant Average
  ## Get the participant's data.
  p.data <- data
  p.model <- group_response_error_predictions
  
  ## Plot response error pooled across participants
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
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
  ## Plot the participant number and data type
  mtext("Group", side=3, cex=AXIS.LABEL.CEX, line=-2, adj=0.2)
  
  # Plot recentered error pooled across participants
  p.recenter.data <- recenter_data
  p.recenter.model <- group_recentered
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, 0.25))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(p.recenter.data$error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- p.recenter.model[p.recenter.model$model == model.type, ]
    points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
  axis(side=4, at=c(0, 0.25), labels = c("0", "0.3"), cex.axis=AXIS.CEX)
  
  ## Plot the x axes (for the last two participants only)
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis=AXIS.CEX)
  
  ## Put the outer margin axis labels.
  mtext("Response error (rads)", side=1, line = 2, outer = TRUE, cex=AXIS.LABEL.CEX)
  mtext("Error density", side=2, line=2, outer=TRUE, cex=AXIS.LABEL.CEX)
  mtext("Recentered density", side=4, line=2, outer=TRUE, cex=AXIS.LABEL.CEX)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=1, lwd = 3, xpd = TRUE, horiz = TRUE, cex = 1.5, seg.len=1, bty = 'n')
  
  ## If we're writing to a file (i.e. a PDF), close the device.
  if(filename != "") {
    dev.off()
  }
}
# Load in recentered data
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/2022-02-18_recentered_exp2_updated.RData")
colnames(recentered_all) <- c('error', 'direction', 'participant', 
                              'orthographic','semantic','model', 'filter')
recenter_data <- recentered_all[recentered_all$model == 'data',]
recenter_model <- recentered_all[recentered_all$model != 'data',]

## Recentered plot
# Get densities from recentered points
get_recenter_density <- function(model){
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "epanechnikov", adjust = 1)
  preds$y[1:75] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-75):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  this_predictions[4] <- model$participant[1]
  return(this_predictions)
}
models <- unique(recenter_model$model)
individual_recentered <- data.frame(matrix(ncol = 4, nrow = 0))
for(i in participants){
  for(j in 1:length(models)){
    this_model <- recenter_model[(recenter_model$participant == i) & (recenter_model$model == models[j]),]
    this_predictions <- get_recenter_density(this_model)
    individual_recentered <- rbind(individual_recentered , this_predictions)
  }
}
colnames(individual_recentered) <- c("value", "prob", "model", "participant")

group_recentered <- data.frame(matrix(ncol = 4, nrow = 0))
for(j in 1:length(models)){
  this_model <- recenter_model[recenter_model$model == models[j],]
  this_predictions <- get_recenter_density(this_model)
  this_predictions[4] <- 'Average'
  group_recentered <- rbind(group_recentered, this_predictions)
}
colnames(group_recentered) <- c("value", "prob", "model", "participant")

plot_recentered <- function(model_list, this_recentered_predictions, data, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10, height=7, units = "in", pointsize = 12, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, 0.25))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(data$error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- this_recentered_predictions[this_recentered_predictions$model == model.type, ]
    points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2, col=MODEL.COL[[model.type]])
  }
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  mtext(paste("Response Offset (rad)"), side=1, cex= AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  axis(side=2, at=c(0, 0.25), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  dev.off()
}

# ## Asymmetric these look messy, maybe just show the group level?
# 
get_asymmetric_individual_recenter <- function(recenter_model){
  directions <- unique(recenter_model$direction)
  lags <- unique(recenter_model$filter)
  asymm_predictions <- data.frame(matrix(ncol = 6, nrow = 0))
  for(h in participants){
    for(i in 1:length(models)){
      for(j in directions){
        for(k in lags){
          this_model <- recenter_model[(recenter_model$participant == h)&
                                         (recenter_model$model == models[i])&
                                         (recenter_model$direction == j)&
                                         (recenter_model$filter == k),]
          this_predictions <- get_recenter_density(this_model)
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
  colnames(asymm_predictions) <- c("value", "prob", "model", "direction", "lag", "participant")
  return(asymm_predictions)
}

get_asymmetric_group_recenter <- function(recenter_model){
  directions <- unique(recenter_model$direction)
  lags <- unique(recenter_model$filter)
  asymm_predictions <- data.frame(matrix(ncol = 5, nrow = 0))
  for(i in 1:length(models)){
    for(j in directions){
      for(k in lags){
        this_model <- recenter_model[(recenter_model$model == models[i])&
                                       (recenter_model$direction == j)&
                                       (recenter_model$filter == k),]
        this_predictions <- get_recenter_density(this_model)
        this_predictions[,4] <- j
        this_predictions[,5] <- k
        # Add on tags for lag and direction
        ## HERE
        asymm_predictions <- rbind(asymm_predictions, this_predictions)
      }
    }
  }
  colnames(asymm_predictions) <- c("value", "prob", "model", "direction", "lag")
  return(asymm_predictions)
}

asymm_group_densities <- get_asymmetric_group_recenter(recenter_model)
plot_asymm_recenter <- function(model_list, data, asymm_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
  }
  
  # Set up panels
  par(mfrow=c(2,3))
  par(mar=c(0.1, 1.5, 0.5, 0.1),
      oma=c(4, 4, 3, 4),
      xaxs="i")
  
  panel_idx <- 1
  panel_labs <- c('+1', '+2', '+3', '-1', '-2', '-3')
  directions <- unique(data$direction)
  lags <- unique(data$filter)
  for(i in directions){
    for(j in lags){
      this_panel_data <- data[(data$direction == i) & (data$filter == j),]
      this_panel_model <- asymm_predictions[(asymm_predictions$direction == i) & 
                                              (asymm_predictions$lag == j),]
      plot.new()
      plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                  ylim=c(Y.RESP.LOW, 0.3))
      
      ## Compute and plot the empirical histograms for response error.
      resp.hist <- hist(this_panel_data$error,
                        breaks=30, freq=FALSE,
                        plot=FALSE)
      for(b in 2:length(resp.hist$breaks)) {
        lo.break <- resp.hist$breaks[b-1]
        hi.break <- resp.hist$breaks[b]
        bar.height <- resp.hist$density[b-1]
        rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
      }
      
      for(model.type in MODEL.TYPES[model_list]) {
        model.data <- this_panel_model[this_panel_model$model == model.type, ]
        points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2.5, col=MODEL.COL[[model.type]])
      }
      ## Plot the participant number and data type
      mtext(paste0("Lag", panel_labs[panel_idx]), side=3, cex=0.85, line=-2, adj=0.1)
      
      ## Plot the x axes (for the bottom row only)
      if(panel_idx %in% c(4, 5, 6)) {
        axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
             cex.axis=AXIS.CEX)
      } else {
        axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=0.75)
      }
      
      ## Plot the y axes (for the participants in the first col)
      if(panel_idx %in% c(1,4)) {
        axis(side=2, at=c(0, 0.2), cex.axis= AXIS.CEX)
      }
      panel_idx <- panel_idx + 1
    }
  }
  
  mtext("Response error (rads)", side=1, line = 2, outer = TRUE, cex=AXIS.LABEL.CEX)
  mtext("Density", side=2, line=2, outer=TRUE, cex=AXIS.LABEL.CEX)

  ## Add in legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
  # xpd = TRUE makes the legend plot to the figure
  if(filename != "") {
    dev.off()
  }
}

# plot_asymm_recenter <- function(model_list, recenter_data, individual_recentered, group_recentered, filename){
#   ## Opens a drawing device (either X11 for testing or a
#   ## PDF for saving).
#   if(filename == "") {
#     X11() # Write to the screen
#   } else {
#     png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
#   }
# 
#   # Set up panels
#   ## Compute variables required for chart layout.
#   PARTICIPANTS <- unique(recenter_data$participant)
#   NUM.PARTICIPANTS <- length(PARTICIPANTS) #Adding one for the average plot
#   PARTICIPANTS.PER.ROW <- 1
#   NUM.ROWS <- ceiling((NUM.PARTICIPANTS+1)*2 / PARTICIPANTS.PER.ROW)
#   NUM.COLS <- PARTICIPANTS.PER.ROW * 3
#   
#   ## Set up the global presentation parameters for the plot.
#   par(mfrow=c(NUM.ROWS, NUM.COLS))
#   par(mar=c(0.5, 0.5, 0, 0.5),
#       oma=c(4, 3.5, 3.5, 3.5))
# 
#   panel_idx <- 1
#   panel_labs <- c('+1', '+2', '+3', '-1', '-2', '-3')
# 
#   directions <- unique(this_recentered_predictions$direction)
#   lags <- unique(this_recentered_predictions$lag)
#   for(i in directions){
#     for(j in lags){
#       this_panel_data <- this_data[(this_data$direction == i) & (this_data$filter == j),]
#       this_panel_model <- this_recentered_predictions[(this_recentered_predictions$direction == i) &
#                                                         (this_recentered_predictions$lag == j),]
#       plot.new()
#       plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
#                   ylim=c(Y.RESP.LOW, 0.4))
# 
#       ## Compute and plot the empirical histograms for response error.
#       resp.hist <- hist(this_panel_data$error,
#                         breaks=30, freq=FALSE,
#                         plot=FALSE)
#       for(b in 2:length(resp.hist$breaks)) {
#         lo.break <- resp.hist$breaks[b-1]
#         hi.break <- resp.hist$breaks[b]
#         bar.height <- resp.hist$density[b-1]
#         rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
#       }
# 
#       for(model.type in MODEL.TYPES[model_list]) {
#         model.data <- this_panel_model[this_panel_model$model == model.type, ]
#         points(model.data$value, model.data$prob, type="l", lty=2, lwd = 2.5, col=MODEL.COL[[model.type]])
#       }
#       ## Plot the participant number and data type
#       mtext(paste0("Lag", panel_labs[panel_idx]), side=3, cex=0.85, line=-2, adj=0.1)
# 
#       ## Plot the y axes (for the panels in the first col)
#       if(panel_idx %in% seq(from=1, to = 28, 3)) {
#         axis(side=2, at=c(0, 0.2, 0.4), cex.axis= AXIS.CEX)
#       }
#       panel_idx <- panel_idx + 1
#     }
#   }
#   
#   # Average
#   
#   
#   ## Plot the x axes (for the bottom row only)
#   if(panel_idx %in% c(34, 35, 36)) {
#     axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)),
#          cex.axis=AXIS.CEX)
#     mtext(paste("Response Offset (rad)"), side=1, cex=0.75, line=2.5)
#   } else {
#     axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=0.75)
#   }
#   
#   # Outer margin label
#   mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
#   
#   ## Add in legend
#   par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
#   plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
#   legend('topright',legend= MODEL.TYPES[model_list],
#          col=color_wheel[model_list], lty=2, lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
#   # xpd = TRUE makes the legend plot to the figure
#   dev.off()
# }
ggplot() +
  geom_histogram(data = recenter_data, aes(x = error, y=..density..)) +
  geom_density(data = recenter_model, aes(x = error, fill = model),
                 alpha = 0.4, position = "identity") +
  facet_wrap(participant ~ orthographic, ncol = 4)