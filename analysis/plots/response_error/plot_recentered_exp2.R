load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/2022-02-11_recentered_exp2_updated.RData")

# Get a number of equally spaced colours
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# color_wheel <- gg_color_hue(5)

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


models <- unique(model_predictions$model)
## Compute variables required for chart layout.
MODEL.TYPES <- unique(as.character(model_predictions$model))
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

data <- recentered_all[recentered_all$model == 'data',]
model_predictions <- recentered_all[recentered_all$model != 'data',]

AXIS.CEX <- 1.2
AXIS.LABEL.CEX <- 1.5
NUM.BINS <- 50
X.RESP.LOW <- -pi - 0.01
X.RESP.HI <- pi + 0.01
Y.RESP.LOW <- 0.0
Y.RESP.HI <- 0.25

#  Function to transform data into (wrapped) density 

get_response_error_density <- function(model){
  preds <- density(as.numeric(model$error), from = -pi, to = pi, cut = FALSE, kernel = "gaussian", adjust = 1.5)
  # To counteract the smoothing to zero beyond the domain -pi, pi, replace the last 50 y co-ords 
  # with the mean of the preceeding 50
  preds$y[1:75] <- mean(preds$y[51:100])
  preds$y[(length(preds$y)-75):length(preds$y)] <- mean(preds$y[(length(preds$y)-100):(length(preds$y)-50)])
  this_predictions <- data.frame(matrix(ncol = 3, nrow = 512))
  this_predictions[1] <- preds$x
  this_predictions[2] <- preds$y
  this_predictions[3] <- model$model[1]
  return(this_predictions)
}

recentered_predictions <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in 1:length(models)){
  this_model <- model_predictions[model_predictions$model == models[i],]
  this_predictions <- get_response_error_density(this_model)
  recentered_predictions <- rbind(recentered_predictions, this_predictions)
}
colnames(recentered_predictions) <- c("value", "prob", "model")

plot_recentered <- function(model_list, this_recentered_predictions, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=8.3, units = "in", pointsize = 12, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, Y.RESP.HI))
  
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
  axis(side=2, at=c(0, 0.1, 0.2, 0.3), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Add in legend
  legend("topright", legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, bty = "n",cex=AXIS.CEX, title="Models")
  
  # Close the plotting device
  dev.off()
}

## Transform asymmetric intrusion histograms
directions <- unique(recentered_all$direction)
lags <- unique(recentered_all$filter)
asymm_predictions <- data.frame(matrix(ncol = 5, nrow = 0))
for(i in 1:length(models)){
  for(j in directions){
    for(k in lags){
      this_model <- model_predictions[(model_predictions$model == models[i])&
                                        (model_predictions$direction == j)&
                                        (model_predictions$filter == k),]
      this_predictions <- get_response_error_density(this_model)
      this_predictions[,4] <- j
      this_predictions[,5] <- k
      # Add on tags for lag and direction
      ## HERE
      asymm_predictions <- rbind(asymm_predictions, this_predictions)
    }
  }
}
colnames(asymm_predictions) <- c("value", "prob", "model", "direction", "lag")

plot_asymm_recenter <- function(model_list, this_asymm_predictions, filename){
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
  for(i in directions){
    for(j in lags){
      this_panel_data <- data[(data$direction == i) & (data$filter == j),]
      this_panel_model <- asymm_predictions[(asymm_predictions$direction == i) & 
                                              (asymm_predictions$lag == j),]
      plot.new()
      plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                  ylim=c(Y.RESP.LOW, Y.RESP.HI))
      
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
        mtext(paste("Response Offset (rad)"), side=1, cex=0.75, line=2.5)
      } else {
        axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=0.75)
      }
      
      ## Plot the y axes (for the participants in the first col)
      if(panel_idx %in% c(1,4)) {
        axis(side=2, at=c(0, 0.2), cex.axis= AXIS.CEX)
        mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
      }
      panel_idx <- panel_idx + 1
    }
  }
  ## Add in legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topright',legend= MODEL.TYPES[model_list],
         col=color_wheel[model_list], lty=2, lwd = 2, xpd = TRUE, horiz = TRUE, cex = AXIS.CEX, seg.len=1, bty = 'n')
  # xpd = TRUE makes the legend plot to the figure
  dev.off()
}

