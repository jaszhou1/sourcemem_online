function [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax)
% ======================================================================
% Simulate dirichlet problem of first exit of a Brownian motion with
% drift from a circular region.

% This is a direct simulation, prior to the correction for oscilliatory
% behaviour- is this a problem? 
% ======================================================================
mu1 = P(1); % This is the drift
mu2 = P(2);

% The diffusion coefficient scales everything else. The diffusion model
% isn't identifiable if you estimate everything (boundary, drift and coefficent are all proportion)
% Everything becomes relative to that coefficient
% p. 432 of Smith 2016

sigma1 = P(3);
sigma2 = P(4);
a = P(5);
a2 = a * a;
T = [0:h:tmax];
nmax = length(T);

% Xt is the sample path 
Xt = zeros(2, nmax);
% Sigma is diffusion coefficient (not drift variability) <- the diffusion
% part
Sigma_Wt = [sigma1 * randn(1,nmax); ...
            sigma2 * randn(1,nmax)]; 
% mu1 and mu2 is drift in x and y co-ords
Mut = [mu1 * ones(1,nmax); ...
       mu2 * ones(1, nmax)];
Dt2 = 0;
i = 2;
a2 = a * a;
while(Dt2 < a2 && i <= nmax)
    % Each time step is the drift component (Xt bit) plus change over time
    % And the diffusion (Sigma_wt) times the square root of time
    Xt(:,i) =  Xt(:,i-1) + Mut(:,i) * h + Sigma_Wt(:,i) * sqrt(h);
    % Euclidean distance of currently sample path away from the centre
    % point
    Dt2 = Xt(1, i)^2 + Xt(2,i)^2;
    i = i + 1;
end;
nonterm = i > nmax;
% ???
Ta = (i-1) * h; % Ta is time
%Ta = (i-3) * h;
theta = atan2(Xt(2, i-1), Xt(1,i-1));
       
end

