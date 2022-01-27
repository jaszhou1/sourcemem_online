## Top level script to generate plots for intrusion manuscript
# To generate each plot, the function requires an argument called "model list" which is simply a list of 
# numbers corresponding to the models required in that plot. The numbers are:
#1. 'Pure Intrusion'
#2. 'Pure Guess'
#3. 'Intrusion + Guess'
#4. 'Temporal'
#5.'Spatiotemporal'
source('~/git/sourcemem_online/analysis/plots/response_error/plot_model_predictions.R')
setwd("~/git/sourcemem_online/analysis/plots/output")
# Response error distributions of models with and without intrusions
plot_response_error(c(1,2,3), data, 'fig6.png')

# Response error with flat and similarity gradient intrusions
plot_response_error(c(3,4,5,6), data, 'fig8.png')

source('~/git/sourcemem_online/analysis/plots/response_error/plot_recentered.R')
setwd("~/git/sourcemem_online/analysis/plots/output")
# Recentered histograms 
plot_recentered(c(1,2,3), recentered_predictions, 'fig7.png')

# Asymmetric recentered histograms by lag
plot_asymm_recenter(c(3,4,5), asymm_predictions, 'fig9.png')

## Diffusion Models
source('~/git/sourcemem_online/analysis/plots/diffusion/plot_diffusion_predictions.R')
setwd("~/git/sourcemem_online/analysis/plots/output")
plot_response_error(c(1,2,3,4,5), 'fig10a.png')
plot_response_time(c(1,2,3,4,5), 'fig10b.png')