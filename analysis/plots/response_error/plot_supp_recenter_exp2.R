setwd("~/git/sourcemem_online/analysis/models/R/experiment_2/output")
load('2022-04-25_recentered_exp2_v2.RData')

data <- recentered_all[recentered_all$type == 'data',]
model_predictions <- recentered_all[recentered_all$type != 'data',]

AXIS.CEX <- 1.2
AXIS.LABEL.CEX <- 1.2
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.1
Y.RESP.HI <- 0.35

color_wheel <- c(#'#00468BFF',
  #'#ED0000FF', no pure intrusion, maybe need to add?
  'green4',
  'darkviolet',
  'blue3',
  'chocolate1',
  'red3') #Find new colour for the recognition model maybe


models <- unique(model_predictions$type)
## Compute variables required for chart layout.
MODEL.TYPES <- unique(as.character(model_predictions$type))
MODEL.COL <- list(
  #"Pure Guess"= color_wheel[1],
  #"Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[1],
  "Temporal" = color_wheel[2],
  "Spatiotemporal" = color_wheel[3],
  "Orthographic" = color_wheel[4],
  "Semantic" = color_wheel[5]
)

line_types <- c('dotted', 
                'dashed', 
                'dotdash', 
                'twodash', 
                'longdash')

MODEL.LTY <- list(
  "Intrusion + Guess"= line_types[1],
  "Temporal" = line_types[2],
  "Spatiotemporal" = line_types[3],
  "Orthographic" = line_types[4],
  "Semantic" = line_types[5]
)

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$offset), from = -pi, to = pi, cut = FALSE, kernel = "gaussian", adjust = 1.5)
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50, smooth it out after
  # preds$y[1:125] <- mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  # preds$y[(length(preds$y)-125):length(preds$y)] <-  mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  preds$y[1:50] <- preds$y[50]
  preds$y[(length(preds$y)-50):length(preds$y)] <- preds$y[length(preds$y)-50]
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$type[1]
  return(this_predictions)
}

#The space one does have some tapering off at the edges, so i dont want the averaging at the ends
get_response_error_density_space <- function(model){
  preds <- density(as.numeric(model$offset), from = -pi, to = pi, cut = FALSE, kernel = "gaussian", adjust = 1.5)
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50, smooth it out after
  # preds$y[1:125] <- mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  # preds$y[(length(preds$y)-125):length(preds$y)] <-  mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  preds$y[1:35] <- preds$y[35]
  preds$y[(length(preds$y)-35):length(preds$y)] <- preds$y[length(preds$y)-35]
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$type[1]
  return(this_predictions)
}

get_response_error_density_sem <- function(model){
  preds <- density(as.numeric(model$offset), from = -pi, to = pi, cut = FALSE, kernel = "gaussian", adjust = 1.5)
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50, smooth it out after
  # preds$y[1:125] <- mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  # preds$y[(length(preds$y)-125):length(preds$y)] <-  mean(c(preds$y[51:100], preds$y[(length(preds$y)-100):(length(preds$y)-50)]))
  preds$y[1:150] <- preds$y[150]
  preds$y[(length(preds$y)-150):length(preds$y)] <- preds$y[150]
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$type[1]
  return(this_predictions)
}

################################### TIME ##################################
directions <- unique(recentered_all$direction)
lags <- unique(recentered_all$filter)
asymm_predictions <- data.frame(matrix(ncol = 4, nrow = 0))
for(i in 1:length(models)){
  for(j in 1:4){
    this_model <- model_predictions[(model_predictions$type == models[i])&
                                      (model_predictions$lag == j),]
    this_predictions <- get_response_error_density(this_model)
    this_predictions[,4] <- j
    # Add on tags for lag and direction
    ## HERE
    asymm_predictions <- rbind(asymm_predictions, this_predictions)
  }
}
colnames(asymm_predictions) <- c("value", "prob", "model", "lag")


################################ SPACE #####################################
space_recenter <- data.frame(matrix(ncol = 4, nrow = 0))
#quantile_label <- c(0.25, 0.5, 0.75, 1)
#space_bins <- c(0, unname(quantile(model_predictions$spatial, c(0.25, 0.5, 0.75, 1))))
space_bins <- c(0, 0.5, 1, 1.5, 2)

for(i in 1:length(models)){
  for(j in 1:4){
    this_model <- model_predictions[(model_predictions$type == models[i])&
#                                      (model_predictions$spatial >= space_bins[j])&
                                      (model_predictions$spatial < space_bins[j+1]),]
    this_predictions <- get_response_error_density_space(this_model)
    this_predictions[,4] <- space_bins[j+1]
    # Add on tags for lag and direction
    ## HERE
    space_recenter <- rbind(space_recenter, this_predictions)
  }
}
colnames(space_recenter) <- c("value", "prob", "model", "bin")

################################### PLOT #####################################
plot_recenter_space <- function(model_list, recentered_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=12 , height=4, units = "in", pointsize = 14, res = 300)
  }
  
  # Set up panels
  par(mfrow=c(1,4))
  par(mar=c(0.1, 4, 0.5, 0.1),
      oma=c(8, 4, 4, 4),
      xaxs="i")
  
  panel_idx <- 1
  
  # Custom y limit
  Y.RESP.LOW <- 0
  Y.RESP.HI <- 0.45
  
  # Define bins for data
  space_bins <- c(0.5, 1, 1.5, 2)
  for(j in 1:4){
    this_panel_data <- data[data$spatial < space_bins[j], ]
    this_panel_model <- recentered_predictions[recentered_predictions$bin == space_bins[j], ]
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, Y.RESP.HI))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(this_panel_data$offset,
                      breaks=30, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    for(model.type in models[model_list]) {
      model.data <- this_panel_model[this_panel_model$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty=MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
    }
    ## Plot the participant number and data type
    mtext(paste0("Space < ", space_bins[j]), side=3, cex= 1, line=-1, adj=0.1)
    
    ## Plot the x axes (for the bottom row only)
    if(panel_idx %in% c(1,2,3,4)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
           cex.axis=AXIS.CEX)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if(panel_idx %in% c(1)) {
      axis(side=2, at=c(0, 0.1, 0.2, 0.3, 0.4), cex.axis= AXIS.CEX)
    }
    panel_idx <- panel_idx + 1
  }
  # Outer Margin axis labels
  
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=2)
  mtext(paste("Response Offset (rad)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=4)
  ## Add in legend - dont need in multi panel
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty= line_types[model_list], lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
  xpd = TRUE #makes the legend plot to the figure
  if(filename != ""){
    dev.off()
  }
}

############################## ORTHOGRAPHY #####################################
orth_recenter <- data.frame(matrix(ncol = 4, nrow = 0))
#quantile_label <- c(0.25, 0.5, 0.75, 1)
#space_bins <- c(0, unname(quantile(model_predictions$spatial, c(0.25, 0.5, 0.75, 1))))
orth_bins <- c(0.25, 0.50, 0.75, 1.00)

for(i in 1:length(models)){
  for(j in 1:4){
    this_model <- model_predictions[(model_predictions$type == models[i])&
                                      (model_predictions$orthographic == orth_bins[j]), ]
    this_predictions <- get_response_error_density(this_model)
    this_predictions[,4] <- orth_bins[j]
    # Add on tags for lag and direction
    ## HERE
    orth_recenter <- rbind(orth_recenter, this_predictions)
  }
}
colnames(orth_recenter) <- c("value", "prob", "model", "bin")

################################### PLOT #####################################
plot_recenter_orth <- function(model_list, recentered_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=12 , height=4, units = "in", pointsize = 14, res = 300)
  }
  
  # Set up panels
  par(mfrow=c(1,4))
  par(mar=c(0.1, 4, 0.5, 0.1),
      oma=c(6, 4, 4, 4),
      xaxs="i")
  
  panel_idx <- 1
  # Define bins for data
  orth_bins <- c(0.25, 0.50, 0.75, 1.00)
  for(j in 1:4){
    this_panel_data <- data[data$orthographic == orth_bins[j], ]
    this_panel_model <- recentered_predictions[recentered_predictions$bin == orth_bins[j], ]
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, Y.RESP.HI))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(this_panel_data$offset,
                      breaks=30, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    for(model.type in models[model_list]) {
      model.data <- this_panel_model[this_panel_model$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty=MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
    }
    ## Plot the participant number and data type
    mtext(paste0("Orthographic = ", orth_bins[j]*4), side=3, cex= 1, line=-1, adj=0.1)
    
    ## Plot the x axes (for the bottom row only)
    if(panel_idx %in% c(1,2,3,4)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
           cex.axis=AXIS.CEX)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if(panel_idx %in% c(1)) {
      axis(side=2, at=c(0, 0.1, 0.2, 0.3), cex.axis= AXIS.CEX)
    }
    panel_idx <- panel_idx + 1
  }
  # Outer Margin axis labels
  
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=2)
  mtext(paste("Response Offset (rad)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=4)
  ## Add in legend - dont need in multi panel
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty= line_types[model_list], lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
  xpd = TRUE #makes the legend plot to the figure
  if(filename != ""){
    dev.off()
  }
}

############################## SEMANTICS #####################################
sem_recenter <- data.frame(matrix(ncol = 4, nrow = 0))
#quantile_label <- c(0.25, 0.5, 0.75, 1)
sem_bins <- c(0.6, 0.4, 0.2, 0)

for(i in 1:length(models)){
  for(j in 1:4){
    this_model <- model_predictions[(model_predictions$type == models[i])&
                                      (model_predictions$semantic > sem_bins[j]), ]
    this_predictions <- get_response_error_density_sem(this_model)
    this_predictions[,4] <- sem_bins[j]
    # Add on tags for lag and direction
    sem_recenter <- rbind(sem_recenter, this_predictions)
  }
}
colnames(sem_recenter) <- c("value", "prob", "model", "bin")

################################### PLOT #####################################
plot_recenter_sem <- function(model_list, recentered_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=12 , height=4, units = "in", pointsize = 14, res = 300)
  }
  
  # Set up panels
  par(mfrow=c(1,4))
  par(mar=c(0.1, 4, 0.5, 0.1),
      oma=c(8, 4, 4, 4),
      xaxs="i")
  
  panel_idx <- 1
  # Define bins for data
  sem_bins <- c(0.6, 0.4, 0.2, 0)
  for(j in 1:4){
    this_panel_data <- data[(data$semantic > sem_bins[j]), ]
    this_panel_model <- recentered_predictions[recentered_predictions$bin == sem_bins[j], ]
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, Y.RESP.HI))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(this_panel_data$offset,
                      breaks=30, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    for(model.type in models[model_list]) {
      model.data <- this_panel_model[this_panel_model$model == model.type, ]
      points(model.data$value, model.data$prob, type="l", lty=MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
    }
    ## Plot the participant number and data type
    mtext(paste0("Semantic > ", sem_bins[j]), side=3, cex= 1, line=-1, adj=0.1)
    
    ## Plot the x axes (for the bottom row only)
    if(panel_idx %in% c(1,2,3,4)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
           cex.axis=AXIS.CEX)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if(panel_idx %in% c(1)) {
      axis(side=2, at=c(0, 0.1, 0.2, 0.3), cex.axis= AXIS.CEX)
    }
    panel_idx <- panel_idx + 1
  }
  # Outer Margin axis labels
  
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=2)
  mtext(paste("Response Offset (rad)"), side=1, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, outer=T, line=4)
  ## Add in legend - dont need in multi panel
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty= line_types[model_list], lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
  xpd = TRUE #makes the legend plot to the figure
  if(filename != ""){
    dev.off()
  }
}
setwd("~/git/sourcemem_online/analysis/plots/output/recenter")
plot_fits <- function(){
  plot_recenter_space(c(2,3), space_recenter, "exp2_space_recenter.png")
  plot_recenter_orth(c(2,4), orth_recenter, "exp2_orth_recenter.png")
  plot_recenter_sem(c(2,5), sem_recenter, "exp2_semantic_recenter.png")
}