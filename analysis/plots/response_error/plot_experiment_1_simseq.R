setwd("~/git/sourcemem_online/analysis/models/R/experiment_1/output")
load("2022-01-14.RData")

sequential <- data[data$is_sequential == TRUE,]
simultaneous <- data[data$is_sequential == FALSE,]

setwd("~/git/sourcemem_online/analysis/plots/output")

plot <- function(sequential, simultaneous, filename){
  ## Opens a drawing device (either X11 for testing or a
  ## PDF for saving).
  if(filename == "") {
    X11() # Write to the screen
  } else {
    png(file=filename, width=10.7, height=4.3, units = "in", pointsize = 16, res = 300)
    #pdf(file=filename, width=8.3, height=10.7)
  }
  
  ## Compute variables required for chart layout.
  
  AXIS.CEX <- 1
  AXIS.LABEL.CEX <- 1.2
  NUM.BINS <- 50
  X.RESP.LOW <- -pi - 0.01
  X.RESP.HI <- pi + 0.01
  Y.RESP.LOW <- 0.0
  Y.RESP.HI <- 0.8
  
  NUM.ROWS <- 1
  NUM.COLS <- 2
  NUM.BINS <- 43
  
  
  ## Set up the global presentation parameters for the plot.
  par(mfrow=c(NUM.ROWS, NUM.COLS))
  par(mar=c(0.5, 0.5, 0, 0.5),
      oma=c(4, 3.5, 3.5, 3.5))
  
  ## Plot Sequential Response Error
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, Y.RESP.HI))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(sequential$response_error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  ## Plot the participant number and data type
  mtext(paste0("Sequential"), side=3, cex=1, line=1, adj=0.2)
  
  ## Plot the x axes
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  axis(side=2, at=c(0, 0.4, 0.8), cex.axis= AXIS.CEX)
  mtext(paste("Density"), side=2, cex=AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  
  ## Plot Simultaneous Response Error
  plot.new()
  plot.window(xlim=c(X.RESP.LOW, X.RESP.HI),
              ylim=c(Y.RESP.LOW, Y.RESP.HI))
  
  ## Compute and plot the empirical histograms for response error.
  resp.hist <- hist(simultaneous$response_error,
                    breaks=NUM.BINS, freq=FALSE,
                    plot=FALSE)
  for(b in 2:length(resp.hist$breaks)) {
    lo.break <- resp.hist$breaks[b-1]
    hi.break <- resp.hist$breaks[b]
    bar.height <- resp.hist$density[b-1]
    rect(lo.break, 0.0, hi.break, bar.height, border=NA, col="grey70")
  }
  
  ## Plot the participant number and data type
  mtext(paste0("Simultaneous"), side=3, cex=1, line=1, adj=0.2)
  
  axis(side=1, at=c(-pi, 0, pi), labels=c(expression(-pi), "0", expression(pi)), cex.axis= AXIS.CEX)
  mtext(paste("Response Error (rad)"), side=1, outer = TRUE, cex= AXIS.CEX, cex.lab = AXIS.LABEL.CEX, line=2.5)
  ## If we're writing to a file (i.e. a PDF), close the device.
  if(filename != "") {
    dev.off()
  }
}