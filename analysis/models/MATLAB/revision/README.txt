Circular Diffusion Model, modified to handle circularly shifted intrusions.

Base circular diffusion code is vdcircle3x, or vdcircle_shiftx for the intrusions.
dependencies are besselzero, dhamana, and dirichlet. These are somewhat black boxes to me, but they handle the first passage time density

On top of that sits my 'diffusion_intrusion_model.m' function, which mixes the different diffusion components corresponding to guesses, intrusion, and target, and all the similarity stuff, and gives a log likelihood of data given the resultant joint density of the model.

the "fit_X_model.m" functions all call that function, and specify a particular set of parameter boundaries. Using the "Sel" selector vector, certain parameters are fixed or freely estimated, which gives the different versions of the circular diffusion model that express different ideas about what's going on re: intrusions, guesses.

These models have been updated to free eta_x and eta_y (the tangential and radial components of drift variability) in response to K. Oberauer's review.