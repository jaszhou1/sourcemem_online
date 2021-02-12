## publicationfigure.R
##
## Procedures for generating publication-quality figures 
## from the marginal data and model fits
##
## Jason Zhou <jzhou AT unimelb DOT edu DOT au>

## Construct a figure based on the concatenated marginal 
## response proportions and response times from both the 
## empirical data and the circular diffusion predictions.
## This is a B.A.F = Big a** function
marginal.publication.figure <- function(data, empirical.data,
                                        filename="") {
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=8.3, height=10.7, units = "in", pointsize = 12, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  
  ## Use only the model predictions from the density data frame.
  data <- data[data$is_model, ]
  
  ## Overall charting parameters.
  NUM.BINS <- 50
  MODEL.LTY <- list(
    "Cont"=1,
    "Thresh"=2
    )
  
  MODEL.COL <- list(
    "Cont"="#CC79A7",
    "Thresh"="#009E73"
  )
  
  
  X.AXIS.CEX <- 1.5
  Y.AXIS.CEX <- 1.5
  
  
  ## Get the summary variables from the data.
  PARTICIPANTS <- unique(data$participant)
  NUM.PARTICIPANTS <- length(PARTICIPANTS)
  PARTICIPANTS.PER.ROW <- 5
  MODEL.TYPES <- unique(as.character(data$model_name))
  
  ## Compute variables required for chart layout.
  NUM.ROWS <- ceiling(NUM.PARTICIPANTS / PARTICIPANTS.PER.ROW)
  NUM.COLS <- PARTICIPANTS.PER.ROW * 2
  
  X.RESP.LOW <- -pi - 0.01
  X.RESP.HI <- pi + 0.01
  Y.RESP.LOW <- 0.0
  Y.RESP.HI <- max(data[data$is_theta, "prob"]) + 0.01
  
  X.RT.LOW <- 0.0
  X.RT.HI <- min(max(data[!data$is_theta, "value"]), 2.0) + 0.01
  Y.RT.LOW <- 0.0
  Y.RT.HI <- max(data[!data$is_theta, "prob"]) + 0.001
  
  ## Set up the global presentation parameters for the plot.
  par(mfrow=c(NUM.ROWS, NUM.COLS))
  par(mar=c(0.1, 0.1, 0.1, 0.1),
      oma=c(4, 4, 3, 4),
      xaxs="i")
  
  ## Iterate through each participant...
  for(p.idx in 1:NUM.PARTICIPANTS) {
    p <- PARTICIPANTS[p.idx]
    
    ## Get the participant's data.
    p.model <- data[(data$participant==p), ]
    p.rt.model <- p.model[(!p.model$is_theta) & (p.model$value > 0), ]
    p.resp.model <- p.model[(p.model$is_theta), ]
    p.data <- empirical.data[empirical.data$participant==p, ]
    
    #p.resp.hist <- hist(p.data$response_error, plot=FALSE)
    #p.rt.hist <- hist(p.data$response_RT, plot=FALSE)
    
    ## Plot marginal response proportion for participant.
    par(mar=c(0.1, 0.1, 0.1, 0.5))
    plot.new()
    plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
                ylim=c(Y.RESP.LOW, Y.RESP.HI))
    
    
    
    ## If this is the first row, indicate the column plot type.
    if(p %in% 1:PARTICIPANTS.PER.ROW) {
      mtext("Response outcome", side=3, line=1)
    }
    
    ## Compute and plot the empirical histograms for response error.
    resp.hist <- hist(p.data$response_error,
                      breaks=NUM.BINS,
                      plot=FALSE)
    for(b in 2:length(resp.hist$breaks)) {
      lo.break <- resp.hist$breaks[b-1]
      hi.break <- resp.hist$breaks[b]
      bar.height <- resp.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey80")
    }
    
    ## Plot the predicted density of the models.
    for(model.type in MODEL.TYPES) {
      model.data <- p.resp.model[p.resp.model$model_name == model.type, ]
      points(model.data$value, model.data$prob, type="l", lwd = 2, 
             #lty=MODEL.LTY[[model.type]], 
             # I don't think the line types help differentiate models visually
             # I have them all dashed so thresh and hybrid are both visible
             lty=2,
             col=MODEL.COL[[model.type]])
    }
    
    ## Plot the participant number and data type
    mtext(paste0("P", p), side=3, cex=0.85, line=-2, adj=0.1)
    
    ## Plot the x axes (for the last two participants only)
    if(p %in% tail(PARTICIPANTS, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), 
           cex.axis=X.AXIS.CEX)
      mtext(paste("Response outcome (rads)"), side=1, cex=0.75, line=2.5)
    } else {
      axis(side=1, at=c(-pi, pi), lwd.ticks=0, labels=FALSE, cex.axis=0.75)
    }
    
    ## Plot the y axes (for the participants in the first col)
    if((p.idx %% PARTICIPANTS.PER.ROW) == 1) {
      axis(side=2, at=c(0, 1), cex.axis=Y.AXIS.CEX)
    }
    
    ## Plot marginal response time for participant.
    par(mar=c(0.1, 0.7, 0.1, 1.5))
    plot.new()
    plot.window(xlim=c(X.RT.LOW, X.RT.HI),
                ylim=c(Y.RT.LOW, Y.RT.HI))
    
    ## If this is the first row, indicate the column plot type.
    if(p %in% 1:PARTICIPANTS.PER.ROW) {
      mtext("Response time", side=3, line=1)
    }
    
    ## Compute and plot the empirical histograms for response time.
    p.data <- p.data[p.data$response_RT < 1.95,]
    
    rt.hist <- hist(p.data$response_RT, 
                    breaks=NUM.BINS,
                    plot=FALSE)
    for(b in 2:length(rt.hist$breaks)) {
      lo.break <- rt.hist$breaks[b-1]
      hi.break <- rt.hist$breaks[b]
      bar.height <- rt.hist$density[b-1]
      rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey80")
    }
    
    ## Plot the predicted density of the models.
    p.rt.model <- p.rt.model[p.rt.model$value < 1.98,]
    
    for(model.type in MODEL.TYPES) {
      model.data <- p.rt.model[p.rt.model$model_name == model.type, ]
      points(model.data$value, model.data$prob, type="l" ,  lwd = 2,
             #lty=MODEL.LTY[[model.type]],
             lty=2,
             col=MODEL.COL[[model.type]])
    }
    
    ## Plot the participant number 
    ## mtext(paste0("P", p), side=3, cex=0.5, line=-2, adj=0.8)
    
    if(p %in% tail(PARTICIPANTS, n=PARTICIPANTS.PER.ROW)) {
      axis(side=1, cex.axis=X.AXIS.CEX)
      mtext(paste("Response time (s)"), side=1, cex=0.75, line=2.5)
    } else {
      axis(side=1, at=c(0, 2), lwd.ticks=0, labels=FALSE, cex.axis=0.75)
    }
    
    if((p.idx %% PARTICIPANTS.PER.ROW) == 0) {
      axis(side=4, at=c(0, 2), cex.axis=X.AXIS.CEX)
    }
  }
  
  #blank plots to fill in the space where I want the legend to go
  plot.new()
  plot.new()
  
  ## Put the outer margin axis labels.
  mtext("Error density", side=2, line=2,outer=TRUE, padj = -0.5)
  mtext("RT density", side=4, line=2, outer=TRUE)
  ##mtext("Response time/response error", side=1, line=2, outer=TRUE)
  
  #allow clipping
  par(xpd = NA)
  ## Add in legend
  legend("bottomright", legend=c("Variable-Precision", "Threshold"),
         col=c("#CC79A7","#009E73"), lty=c(2,2,2), lwd = 2, bty = "n", title="Models")
  
  ## If we're writing to a file (i.e. a PDF), close the device.
  if(filename != "") {
    dev.off()
  }
}

## Read in model predictions
Cont <- read.csv('2021-01-12-13-31_Cont.csv')
Thresh <- read.csv('2021-01-12-13-32_Thresh.csv')
#Hybrid <- read.csv('2020-11-12-20-56_Hybrid.csv')

models <- rbind(Cont,Thresh)
models$is_theta <- models$is_theta == ' true'
models$is_model <- models$is_model == ' true'
## Read in empirical data
dataset <- read.csv('sourcemem_data.csv')
dataset$response_RT <- dataset$source_RT/1000

marginal.publication.figure(models, dataset)
