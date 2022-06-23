## Top level script to generate plots for intrusion manuscript
# To generate each plot, the function requires an argument called "model list" which is simply a list of 
# numbers corresponding to the models required in that plot. The numbers are:
#1. 'Pure Intrusion'
#2. 'Pure Guess'
#3. 'Intrusion + Guess'
#4. 'Temporal'
#5.'Spatiotemporal'
setwd("~/git/sourcemem_online/analysis/plots/response_error")
source('~/git/sourcemem_online/analysis/plots/response_error/plot_experiment_1.R')
setwd("~/git/sourcemem_online/analysis/plots/output")

# Plot Sequential vs Simultaneous data

# Response error distributions of models with and without intrusions
plot_response_error(c(1,2,3), data, 'fig5a.png')

# Response error with flat and similarity gradient intrusions
plot_response_error(c(3,4,5), data, '6a.png')

source('~/git/sourcemem_online/analysis/plots/response_error/plot_recentered.R')
setwd("~/git/sourcemem_online/analysis/plots/output")

# Recentered histograms 
plot_recentered(c(1,2,3), recentered_predictions, '5b.png')
plot_recentered(c(3,4,5), recentered_predictions, '6b.png')
# Asymmetric recentered histograms by lag
plot_asymm_recenter(c(3,4,5), asymm_predictions, '6c.png')

## Diffusion Models
source('~/git/sourcemem_online/analysis/plots/diffusion/plot_diffusion_predictions.R')
setwd("~/git/sourcemem_online/analysis/plots/output")
plot_response_error(c(1,2,3,4,5), '10a_v2.png')
plot_response_time(c(1,2,3,4,5), '10b_v2.png')


# Experiment 2
setwd("~/git/sourcemem_online/analysis/models/R/experiment_2/output")
load("~/git/sourcemem_online/analysis/models/R/experiment_2/output/2022-04-25_recentered_exp2_v2.RData")
# Non- ortho/sem models
source('~/git/sourcemem_online/analysis/plots/response_error/plot_experiment_2.R')
setwd("~/git/sourcemem_online/analysis/plots/output")
individual_combined_plot(c(2,3,4,5), data, response_error_predictions, recenter_data, 
                         individual_recentered, group_response_error_predictions, group_recentered, "fig11c.png")
plot_asymm_recenter(c(1,2,3), recenter_data, asymm_group_densities, "fig12.png")

#exp2_plot(4:7, data, model_predictions, "test.png")
## Demonstrate what's going on with the temporal model, in response to P's comments
# source('~/git/sourcemem_online/analysis/plots/temporal_gradient_demo.R')
