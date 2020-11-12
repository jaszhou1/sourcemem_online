% ==========================================================================
%  Circular diffusion model, indepedendent Gaussian drift rates, C-version
%
%      [T, Gt, Theta, Ptheta, Mt] = vdcircle300cls(P, tmax, badix);
%      P = [v1, v2, eta1, eta2, sigma, a]
%  [-pi:pi] closed domain so don't lose last bin in interpolation.
%  Building:
%            mex vdcircle300cls.c -lgsl -lgslcblas  -lm 
% ===========================================================================

