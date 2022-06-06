# Plot Experiment 2 Diffusion
library(ggplot2)
library(tidyr)
## Read in, and transform data as required. ##

# Plot marginal response error and time model predictions against data
setwd("~/git/sourcemem_online/analysis/models/MATLAB/")
data <- read.csv('experiment_2_cutoff.csv')
# Exclude practice block
data <- data[data$block!= 0,]

# Exclude invalid RTs
data <- data[data$valid_RT==TRUE,]

setwd("~/git/sourcemem_online/analysis/models/MATLAB/experiment_2/old")

pure_guess <- read.csv('sim_pure_guess.csv', header = FALSE)
pure_guess$model <- 'Pure Guess'

pure_intrusion <- read.csv('sim_pure_intrusion.csv', header = FALSE)
pure_intrusion$model <- 'Pure Intrusion'

intrusion <- read.csv('sim_flat.csv', header = FALSE)
intrusion$model <- 'Intrusion + Guess'

temporal <- read.csv('sim_temporal.csv', header = FALSE)
temporal$model <- 'Temporal'

spatiotemporal <- read.csv('sim_spatiotemporal.csv', header = FALSE)
spatiotemporal$model <- 'Spatiotemporal'

orthographic <- read.csv('sim_ortho.csv', header = FALSE)
orthographic$model <- 'Orthographic'

semantic <- read.csv('sim_semantic.csv', header = FALSE)
semantic$model <- 'Semantic'

# add <- read.csv('sim_add.csv', header = FALSE)
# add$model <- 'Four Factor (Additive)'

multi <- read.csv('sim_multi.csv', header = FALSE)
multi$model <- "Four Factor"

# spatiotemporal_no_eta <- read.csv('sim_spatiotemporal_no_eta.csv', header = FALSE)
# spatiotemporal_no_eta$model <- "Spatiotemporal (no eta)"


model_predictions <- rbind(pure_guess, pure_intrusion, intrusion, temporal, spatiotemporal,
                           orthographic, semantic, multi)
colnames(model_predictions) <- c('error', 'time', 'participant', 'model')

MODEL.TYPES <- unique(model_predictions$model)

#temporarily exclude p5
# data <- data[data$participant != 5,]
# model_predictions <- model_predictions[model_predictions$participant != 5,]


# Assign models to colours
color_wheel <- c('#00468BFF',
                 '#009E73',
                 '#ED0000FF',
                 '#0099B4FF',
                 '#925E9FFF',
                 '#009E73',
                 '#D55E00',
                 '#CC79A7')

MODEL.COL <- list(
  "Pure Guess"= color_wheel[1],
  "Pure Intrusion"= color_wheel[2],
  "Intrusion + Guess"= color_wheel[3],
  "Temporal" = color_wheel[4],
  "Spatiotemporal" = color_wheel[5],
  "Orthographic" = color_wheel[6],
  "Semantic" = color_wheel[7],
  "Four Factor" = color_wheel[8]
)

line_types <- c('solid', 
                'longdash', 
                'dotted', 
                'dashed', 
                'dotdash', 
                'twodash', 
                'longdash', 
                'dotted')

MODEL.LTY <- list(
  "Pure Guess"= line_types[1],
  "Pure Intrusion"= line_types[2],
  "Intrusion + Guess"= line_types[3],
  "Temporal" = line_types[4],
  "Spatiotemporal" = line_types[5],
  "Orthographic" = line_types[6],
  "Semantic" = line_types[7],
  "Four Factor" = line_types[8]
)

# Functions to get wrapped densities from simulated data
get_response_error_density <- function(model){
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "epanechnikov")
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:50] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-50):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  colnames(this_predictions) <- c("value", "prob", "model")
  return(this_predictions)
}

get_response_time_density <- function(model){
  preds <- density(as.numeric(model$time), from = 0, to = 7, cut = FALSE, kernel = "epanechnikov", adjust = 2)
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  colnames(this_predictions) <- c("value", "prob", "model")
  return(this_predictions)
}

exp2_plot <- function(model_list, data, model_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 14, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  
  ## Compute variables required for chart layout.
  
  AXIS.CEX <- 1
  AXIS.LABEL.CEX <- 1.2
  
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS) #Adding one for the average plot
  PARTICIPANTS.PER.ROW <- 1
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW) + 1
  NUM.COLS <- PARTICIPANTS.PER.ROW * 2
  NUM.BINS <- 50
  
  X.RESP.LOW <- -pi - 0.01
  X.RESP.HI <- pi + 0.01
  Y.RESP.LOW <- 0.0
  Y.RESP.HI <- 2
  
  X.RT.LOW <- 0.0
  X.RT.HI <- 4
  Y.RT.LOW <- 0.0
  Y.RT.HI <- 3
  
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
    
    ## Plot Response Error
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
    
    # Plot model predictions
    for(model.type in MODEL.TYPES[model_list]) {
      model.data <- get_response_error_density(p.model[p.model$model == model.type, ])
      points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
    }
    
    ## Plot the participant number and data type
    mtext(paste0("P", p), side=3, cex=1, line=-2, adj=0.2)
    
    ## Plot the x axes
    axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    
    
    ## Plot the y axes (for the participants in the first col)
    axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
    
    ## Plot Response Time
    plot.new()
    plot.window(xlim=c(X.RT.LOW, X.RT.HI),
                ylim=c(Y.RT.LOW, Y.RT.HI))
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(p.data$source_RT/1000,
                      breaks=NUM.BINS, freq=FALSE,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
    }
    
    
    for(model.type in MODEL.TYPES[model_list]) {
      model.data <- get_response_time_density(p.model[p.model$model == model.type, ])
      points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
    }
    
    axis(side=1, at=c(0, 7), lwd.ticks=0, labels=FALSE, cex.axis=AXIS.CEX)
    
    if((p.idx %% PARTICIPANTS.PER.ROW) == 0) {
      axis(side=4, at=c(0, 3), cex.axis=AXIS.CEX)
    }
  }
  # Plot Group Resonse Error
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, Y.RESP.HI))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(data$response_error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  # Plot model predictions
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- get_response_error_density(model_predictions[model_predictions$model == model.type, ])
    points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]])
  }
  
  ## Label this plot as the group plot
  mtext(paste0("Group"), side=3, cex=1, line=-2, adj=0.2)
  
  # Plot the x axis
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
       cex.axis= AXIS.CEX)
  mtext(paste("Response error (rads)"), side=1, cex=AXIS.LABEL.CEX, line=2.5)
  # Plot y axis
  axis(side=2, at=c(0, 1, 2), cex.axis=AXIS.CEX)
  # Plot Group Response Time
  plot.new()
  plot.window(xlim=c(X.RT.LOW, X.RT.HI),
              ylim=c(Y.RT.LOW, Y.RT.HI))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(data$source_RT/1000,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  for(model.type in MODEL.TYPES[model_list]) {
    model.data <- get_response_time_density(model_predictions[model_predictions$model == model.type, ])
    points(model.data$value, model.data$prob, type="l", lty= MODEL.LTY[[model.type]], lwd = 2, col=MODEL.COL[[model.type]], alpha = 0.5)
  }
  # Plot x axis and label
  axis(side=1, cex.axis=AXIS.CEX)
  mtext(paste("Response time (s)"), side=1, cex=AXIS.LABEL.CEX, line=2.5)
  
  # plot y axis
  axis(side=4, at=c(0, 3), cex.axis=AXIS.CEX)
  
  # Add Legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend("top", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=line_types[model_list], 
         lwd = 3, bty = "n",cex=AXIS.LABEL.CEX, 
         seg.len=5, title="Models")
  
  ## If we're writing to a file (i.e. a PDF), close the device.
  if(filename != "") {
    dev.off()
  }
}

rt_quantiles <- c(0.1, 0.5, 0.9)
error_quantiles <- c(0.1, 0.3, 0.5, 0.9)
Q_SYMBOLS <- c(25, 23, 24, 22)
# Joint Q-Q Plot
source('~/git/sourcemem_online/analysis/plots/diffusion/qxq.R')
source('~/git/sourcemem_online/analysis/plots/diffusion/RT_confidence_interval.R')

plot_individual_qq <- function(model_list, data, model, filename, confidence){
  if(missing(confidence)){
    confidence <- FALSE
  }
  # Covert from ms to s
  data$source_RT <- data$source_RT/1000
  # Get data quantiles
  data_qq <- qq_CI(rt_quantiles, error_quantiles, data, 'data')
  model_qq <- data.frame()
  # For the model predictions, need to rename columns of simulated data to keep everything consistent
  colnames(model) <- c('response_error', 'source_RT', 'participant', 'model')
  for(model.type in MODEL.TYPES[model_list]){
    this_model <- model[model$model == model.type,]
    this_qq <- all_qq_points(rt_quantiles, error_quantiles, this_model, model.type)
    model_qq <- rbind(model_qq, this_qq)
  }
  
  model_qq$model_f <- factor(model_qq$model, c('Pure Guess', 'Pure Intrusion', 'Intrusion + Guess',
                                               'Temporal', 'Spatiotemporal', 'Orthographic', 'Semantic',
                                               'Four Factor'))
  
  # Plot (ggplot) - add switch here for if we want RT confidence plotted or not
  if(confidence == TRUE){
    plot <- ggplot() +
      geom_point(data = data_qq, size = 3, aes(x= theta, y = rt, shape = factor(rt_q))) +
      geom_segment(data = data_qq, linetype = "solid", size = 1, alpha = 0.4, 
                   aes(x = theta, xend = theta, y = rt_lower, yend = rt_upper, group = rt_q)) +
      geom_point(data=model_qq, size = 3, alpha = 1, aes(x= theta, y = rt, shape = factor(rt_q), color = model_f)) +
      geom_line(data = model_qq, alpha = 0.5, size = 1.5, aes(x = theta, y = rt, linetype= model_f,
                                                              color = model_f, group = interaction(model_f, rt_q))) +
      scale_linetype_manual(values=c(#'solid', 
                                     #'longdash', 
                                     #'dotted', 
                                     'dashed', 
                                     'dotdash', 
                                     'twodash', 
                                     'longdash')) + 
                                     #'dotted' + 
      scale_color_manual(values=c(#'#00468BFF',
                                  #'#009E73',
                                  #'#ED0000FF',
                                  '#0099B4FF',
                                  '#925E9FFF',
                                  '#009E73',
                                  '#D55E00')) +
                                  #'#CC79A7' +
      scale_x_continuous(name = 'Absolute Error (rads)', breaks = c(0, pi), limits = c(0, pi),
                         labels = c(0, expression(pi))) +
      scale_y_continuous(name = 'Response Time (s)', breaks = c(0.5, 1.0, 1.5, 2.0)) +
      # Use the same colour mapping as other plot
      guides(size = "none",
             color= "none",
             linetype = "none",
             shape= guide_legend(title="Response Time Quantile")) +
      theme(
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 14),
        plot.title = element_blank(),
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text=element_text(size= 14),
        axis.line = element_line(colour = "black")
      )
  }
  else{
    plot <- ggplot() +
      geom_point(data=data_qq, size = 3, aes(x= theta, y = rt, shape = factor(rt_q))) +
      geom_point(data=model_qq, size = 3, alpha = 0.7, aes(x= theta, y = rt, shape = factor(rt_q), color = model)) +
      geom_line(data = model_qq, linetype="dashed", alpha = 0.5, size = 1, aes(x = theta, y = rt,
                                                                               color = model, group = interaction(model, rt_q))) +
      scale_x_continuous(name = 'Absolute Error (rads)', breaks = c(0, pi), limits = c(0, pi),
                         labels = c(0, expression(pi))) +
      scale_y_continuous(name = 'Response Time (s)', breaks = c(0.5, 1.0, 1.5, 2.0)) +
      # Use the same colour mapping as other plot
      scale_color_manual(values= unlist(MODEL.COL[model_list], use.names = FALSE)) +
      guides(size = "none",
             linetype = "none",
             color= "none",
             shape= guide_legend(title="Response Time Quantile")) +
      theme(
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 14),
        plot.title = element_blank(),
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.text=element_text(size= 14),
        axis.line = element_line(colour = "black")
      )
  }
  if(filename == ""){
    return(plot)
  } else {
    ggsave(filename, width=10.7, height=4, units = "in")
    return(plot)
  }
}
setwd("~/git/sourcemem_online/analysis/plots/diffusion")
individual_qq <- function(confidence){
  if(missing(confidence)){
    confidence <- FALSE
  }
  for (i in unique(data$participant)){
    filename = sprintf('exp2_p_%i_qxq2.png', i)
    plot_individual_qq(c(4:7),data[data$participant == i,], model_predictions[model_predictions$participant == i,], filename, confidence)
  }
  filename = sprintf('exp2_group_qxq.png')
  plot_individual_qq(c(4:7),data, model_predictions, filename, confidence)
}

# Plot RT distributions for each error quantile

# Append error quantile labels
append_quantile_label <- function(this_data){
  this_error_quantile <- quantile(abs(this_data$response_error), probs = error_quantiles)
  this_error_quantile <- append(0, this_error_quantile)
  for (i in 1:length(error_quantiles)){
    this_data[(abs(this_data$response_error) > this_error_quantile[[i]]) &
                 (abs(this_data$response_error) < this_error_quantile[[i+1]]),'error_quantile'] <- error_quantiles[[i]] 
  }
  #this_data$error_quantile[is.na(this_data$error_quantile)] <- 1
  this_data <- na.omit(this_data)
  return(this_data)
}

plot_rt_quantiles <- function(model_list, data, model_predictions){
  # Covert from ms to s
  data$source_RT <- data$source_RT/1000
  participants <- unique(data$participant)
  models <- unique(model_predictions$model)
  model_predictions <- model_predictions[model_predictions$model %in% models[model_list],]
  colnames(model_predictions) <- c('response_error', 'source_RT', 'participant', 'model')
  # Append error quantile labels
  appended_data <- data.frame()
  for(i in participants){
    this_data <- data[data$participant == i, ]
    this_data <- append_quantile_label(this_data)
    appended_data <- rbind(appended_data, this_data)
  }
  appended_model_predictions <- data.frame()
  for(i in participants){
    for (j in unique(model_predictions$model)){
      this_model <- model_predictions[(model_predictions$participant == i) &
                                        (model_predictions$model == j),]
      this_model <- append_quantile_label(this_model)
      appended_model_predictions <- rbind(appended_model_predictions, this_model)
    }
  }
  plot <- ggplot() +
    geom_histogram(data = appended_data, aes(x = source_RT, y = ..density..)) +
    geom_density(data = appended_model_predictions, aes(x = source_RT, color = model)) + 
    facet_wrap(~participant + error_quantile, ncol = 3) +
    theme(legend.position="bottom") +
    theme(text = element_text(size = 20))
  ggsave(filename = 'participant_RTdistributions.png', plot, width = 30, height = 40, units = "cm")
}

