function [ll, aic, P, penalty] = threshold_model(Pvar, Pfix, Sel, Data)
% This is a mixture model (Zhang and Luck style) with uniform guessing from a
% zero-drift diffusion process and positive drift

%% Debugging
name = 'THRESHOLD: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Component weights do not sum to 1...';

%% Global variables

np = 8; % Number of parameters
epsx = 1e-9; % Small values to substitute for zeroes
cden = 0.05;  % Contaminant density.
tmax = 5.1; % Maximum response time
nt = 300; % Number of time steps
h = tmax/nt; % Size of a time step
nw = 50; % Number of angular steps on the circle.
% nw = 50 discretises the circle into 7.2 deg. steps
w = 2 * pi / nw; % size of the angular step
badix = 5; % number of bad initial values in Gt to zero
% Noise in the drift rate (infinitesimal standard deviation)
kmax = 50; %Maximum number of eigenvalues in dhamana
num_intrusions = 9;

%% Parameters

% Check to see if the parameter vector is of appropriate size
lp = length(Pvar) + length(Pfix);
if lp ~= np
    [name, errmg1], length(Pvar), length(Pfix), return;
end
if length(Sel) ~= np
    [name, errmg2], length(Sel), return;
end

% Assemble parameter vector.
P = zeros(1,np);
P(Sel==1) = Pvar;
P(Sel==0) = Pfix;

% Save on each iteration
Ptemp = P;
save Ptemp

v1 = P(1);
v2 = P(2);
eta = P(3);
a1 = P(4);
a2 = P(5);
beta = P(6);
ter = P(7);
st = P(8);

% Check to see if component weights sum to 1.
if beta > 1
    ll = 1e7;
    aic = 0;
    penalty = 1e7;
    [name, errmg3], return;
end

sigma = 1.0;

% Assume eta components in the x and y directions are the same
eta1 = eta;
eta2 = eta;
%% Parameter bounds
penalty = 0; % Set the penalty to an initial value of zero
% ---------------------------------------------------------------------------
%   [v1, v2,            eta   a1, a2,      beta,   Ter,     st]
% --------------------------------------------------------------------------
Ub= [ 8.0*ones(1,2),    2.0,  5.0, 5.0,     1.0,    0.7,    3.0];
Lb= [0*ones(1,2),       0.0,  0.5, 0.5,     0,      0,      0];
Pub=[ 2.5*ones(1,2),    2.5,  4.5, 4.5,     0.9,    0.65,   2.8];
Plb=[0*ones(1,2),       0.0,  0.7, 0.7,     0,      0.01,   0];

if any(P - Ub > 0) || any(Lb - P > 0)
    ll = 1e7 + ...
        1e3 * (sum(max(P - Ub, 0).^2) + sum(max(Lb - P, 0).^2));
    aic = 0;
    return
else
    penalty =  1e3 * (sum(max(P - Pub, 0).^2) + sum(max(Plb - P, 0).^2));
end

pest_penalty(1,:) = P;
pest_penalty(2,:) = max(P - Pub, 0).^2 + max(Plb - P, 0).^2;


P_targ = [v1, v2, eta1, eta2, sigma, a1];
[t, Gt_target, theta] = vdcircle3x(P_targ, nw, h, tmax, badix);

%Gt_targ is the exponential term for the (unshifted) target. This will
%always be the same for each trial in this iteration because it is relative
%to the target location

% Because the guessing process has a different criterion (a2) to the other
% processes, we need to calculate Gt0 separately.
P_guess = [0, 0, 0, 0, sigma, a2];
[~,Gt_guess, ~] = vdcircle3x(P_guess, nw, h, tmax, badix);


% Create mesh for interpolation
[angle,time]=meshgrid(t, theta);

% Empty structure to put likelihood of each data point
like = zeros(length(Data),1);

% This is implemented in a loop in the same way that the intrusion model is
% so I can test and debug. To be more efficient, this can be vectorised.
for i = 1:length(Data)
    this_phi = Data(i,1);
    this_rt = Data(i,2);
    % Find the likelihood of this data point by interpolating in the joint
    % density of the target diffusion process
    target_likelihood = interp2(angle, time, Gt_target, this_rt, this_phi, 'linear');
    guess_likelihood = interp2(angle, time, Gt_guess, this_rt, this_phi, 'linear');
    % Generate a canonically oriented distribution for the intrusion drift
    % rate, and then circularly shift the exponential term (Gt) by the number
    % of angular steps it takes to each the offset of the intrusion in question
    % and the target angle.
    
    like(i) = (1 - beta)*target_likelihood + beta*guess_likelihood;
end

% Out of range values returned as NaN's. Treat as contaminants - set small.
idx = isnan(like);
like(idx) = cden;

% Log-likelihoods.
log_like = log(like);

% Minimize sum of minus LL's across two conditions.
ll = sum(-log_like) + penalty;

% Calculate the aic
aic = 2 * ll + 2* sum(Sel);
end