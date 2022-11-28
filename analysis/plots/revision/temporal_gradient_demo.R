library(data.table)

# Specify some numbers
kappa <- 0.5
lambda_b <- 1
lambda_f <- 1

kappa2 <- 0.75
lambda_b2 <- 2
lambda_f2 <- 0.5

# Define a vector of raw temporal similarities
temporal_similarity <- setNames(data.frame(matrix(ncol = 4, nrow = 19)), c('lag', 'equal', 'tau', 'lambda'))
# Backwards intrusion slope
temporal_similarity[1:9,1] <- -9:-1
temporal_similarity[1:9,2] <- (1-kappa)*exp(-lambda_b*(abs(-9:-1)))
temporal_similarity[1:9,3] <- (1-kappa2)*exp(-lambda_b*(abs(-9:-1)))
temporal_similarity[1:9,4] <- (1-kappa2)*exp(-lambda_b2*(abs(-9:-1)))

temporal_similarity[10,1] <- 0

# Forwards intrusion slope
temporal_similarity[11:19,1] <- 1:9
temporal_similarity[11:19,2] <- kappa*exp(-lambda_f*(abs(1:9)))
temporal_similarity[11:19,3] <- kappa2*exp(-lambda_f*(abs(1:9)))
temporal_similarity[11:19,4] <- kappa2*exp(-lambda_f2*(abs(1:9)))

# Normalise across serial positions
temporal_similarity$equal <- temporal_similarity$equal/sum(temporal_similarity[setdiff(1:18, 10),]$equal)
temporal_similarity$tau <- temporal_similarity$tau/sum(temporal_similarity[setdiff(1:18, 10),]$tau)
temporal_similarity$lambda <- temporal_similarity$lambda/sum(temporal_similarity[setdiff(1:18, 10),]$lambda)

temporal_similarity <- melt(setDT(temporal_similarity), id=c("lag"))
colnames(temporal_similarity) <- c('lag', 'cond', 'value')

ggplot(data = temporal_similarity, aes(x = lag, y = value, lty = cond, shape = cond, color = cond)) +  
  geom_point(size = 3.5) + geom_line(lwd = 1.2, alpha = 0.5) + 
  scale_color_manual(values=c("#009E73",
                              "#0072B2",
                              "#D55E00")) + 
  scale_x_continuous(name = 'Lag of Intruding Item', breaks = c(-9, -5, -1, 1, 5, 9)) +
  scale_y_continuous(name = 'Intrusion Probability') +
  theme(
    axis.text.x = element_text(color="black", size = 18),
    axis.text.y = element_text(color="black", size = 18),
    plot.title = element_blank(),
    axis.title.x = element_text(color="black", size=20),
    axis.title.y = element_text(color="black", size=20),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.text=element_text(size= 18),
    axis.line = element_line(colour = "black")
  )

# Plot the actual average values, requested by Reviewer #3
# Define a vector of raw temporal similarities
temporal_similarity_fitted <- setNames(data.frame(matrix(ncol = 3, nrow = 19)), c('lag', 'Temporal',  'Spatiotemporal'))
temp_kappa <- 0.56
temp_lambda_b <- 0.89
temp_lambda_f <- 1.08

spa_kappa <- 0.56
spa_lambda_b <- 1.69
spa_lambda_f <- 1.55


temporal_similarity_fitted[1:9,1] <- -9:-1
temporal_similarity_fitted[1:9,2] <- (1-temp_kappa)*exp(-temp_lambda_b*(abs(-9:-1)))
temporal_similarity_fitted[1:9,3] <- (1-spa_kappa)*exp(-spa_lambda_b*(abs(-9:-1)))

temporal_similarity_fitted[10,1] <- 0

# Forwards intrusion slope
temporal_similarity_fitted[11:19,1] <- 1:9
temporal_similarity_fitted[11:19,2] <- temp_kappa*exp(-temp_lambda_f*(abs(1:9)))
temporal_similarity_fitted[11:19,3] <- spa_kappa*exp(-spa_lambda_f*(abs(1:9)))

temporal_similarity_fitted <- melt(setDT(temporal_similarity_fitted), id=c("lag"))
colnames(temporal_similarity_fitted) <- c('lag', 'Model', 'value')



ggplot(data = temporal_similarity_fitted, aes(x = lag, y = value, lty = Model, shape = Model, color = Model)) +  
  geom_point(size = 3.5) + geom_line(lwd = 1.2, alpha = 0.8) + 
  scale_x_continuous(name = 'Lag of Intruding Item', breaks = c(-9, -5, -1, 1, 5, 9)) +
  scale_y_continuous(name = 'Intrusion Probability') +
  scale_color_manual(values=c('#0099B4FF',
                              '#925E9FFF')) +
  scale_linetype_manual(values=c("longdash", "twodash")) +
  theme(
    axis.text.x = element_text(color="black", size = 18),
    axis.text.y = element_text(color="black", size = 18),
    plot.title = element_blank(),
    axis.title.x = element_text(color="black", size=20),
    axis.title.y = element_text(color="black", size=20),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.text=element_text(size= 18),
    legend.title=element_text(size= 18),
    axis.line = element_line(colour = "black"),
    legend.position="none"
  )

setwd("~/git/sourcemem_online/analysis/plots/output")
ggsave('fitted_temporal.png', plot = last_plot(), width = 30, height = 10, units = "cm")