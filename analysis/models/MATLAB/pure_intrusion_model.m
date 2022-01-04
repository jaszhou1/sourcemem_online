function [ll, aic, P, pest_penalty] = pure_intrusion_model(Pvar, Pfix, Sel, Data, badix)
% Intrusions will be handled by generating the
% exponential term of the Girsanov transformation, in which the target
% stimulus angle is represented in a canonical orietnation, and then making
% circularly shifted copies of the exponential term for each intrusion
% (shifting by the offset between each intrusion and the target angle).

% The likelihood of each trial is evaluated one by one because each trial
% has a unique mix of distractors that may intrude. These likelihoods are
% then multiplied (well, the logs are summed) at the end.

% At the moment, v_targ = v_int, so they share the same v1 and v2
% components. This can be changed if necesarry

% Assumes the eta components in the x and y directions are the
% same.

% Each participant's data has 13 columns:
% 1. response error
% 2. response time
% 3. target angle
% 4. response angle
% 5. non-target angle 1
% ...
% 13. non-target angle 9

%% Debugging
name = 'PURE_INTRUSION_MODEL: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Component weights do not sum to 1...';

%% Global variables

np = 10; % Number of parameters
epsx = 1e-9; % Small values to substitute for zeroes
cden = 0.05;  % Contaminant density.
tmax = 5.1; % Maximum response time
nt = 300; % Number of time steps
h = tmax/nt; % Size of a time step
nw = 50; % Number of angular steps on the circle.
% nw = 50 discretises the circle into 7.2 deg. steps
w = 2 * pi / nw; % size of the angular step
% Noise in the drift rate (infinitesimal standard deviation)
kmax = 50; %Maximum number of eigenvalues in dhamana
num_intrusions = 10;

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
% Ptemp = P;
% save Ptemp

v1_targ = P(1);
v2_targ = P(2);
v1_int = P(3);
v2_int = P(4);
eta_targ = P(5);
eta_int = P(6);
a_targ = P(7);
a_int = a_targ;
gamma = P(8);
ter = P(9);
st = P(10);

% Check to see if component weights sum to 1.
if gamma > 1
    ll = 1e7;
    aic = 0;
    penalty = 1e7;
    pest_penalty(1,:) = P;
    [name, errmg3], return;
end

sigma = 1.0;

% Assume eta components in the x and y directions are the same
eta1_targ = eta_targ;
eta2_targ = eta_targ;
eta1_int = eta_int;
eta2_int = eta_int;
%% Parameter bounds
penalty = 0; % Set the penalty to an initial value of zero
pest_penalty(1,:) = P;
% ----------------------------------------------------------------------------
%   [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  gamma, Ter, st]
% ----------------------------------------------------------------------------
Ub= [ 8,   0,    8,   0,   1,    1,       4.5, 1.0,   0.3,  0.2];
Lb= [ 0,   0,    0,   0,   0,    0,       0.1, 0,     0,    0];
Pub=[ 7.5, 0,    7.5, 0,   0.9,  0.9,     4.0, 0.99,  0.25, 0.15];
Plb=[ 0,   0,    0,   0,   0,    0,       0.7, 0.01,  0.01, 0.01];

if any(P - Ub > 0) || any(Lb - P > 0)
    ll = 1e7 + ...
        1e3 * (sum(max(P - Ub, 0).^2) + sum(max(Lb - P, 0).^2));
    aic = 0;
    return
else
    penalty =  1e3 * (sum(max(P - Pub, 0).^2) + sum(max(Plb - P, 0).^2));
end


pest_penalty(2,:) = max(P - Pub, 0).^2 + max(Plb - P, 0).^2;


%% On-target retrieval likelihoods
P_targ = [v1_targ, v2_targ, eta1_targ, eta2_targ, sigma, a_targ];
[t, Gt_target, theta] = vdcircle3x(P_targ, nw, h, tmax, badix);

%% Intrusion processes
% Zero-Drift component shared between intrusions
% kmax controls truncation of series
% badix is number of bad initial values in Gt to zero.
P0 = [a_int, sigma];
[~, Gt0] = dhamana(P0, kmax, h, tmax, badix);

% Exponential term
P_intrusion = [v1_int, v2_int, eta1_int, eta2_int, sigma, a_int];
%Gt_targ is the exponential term for the (unshifted) target. This will
%always be the same for each trial in this iteration because it is relative
%to the target location

% Empty structure to put likelihood of each data point
like = zeros(length(Data),1);
for i = 1:length(Data)
    % Find the intrusions for this trial
    this_trial_intrusions = Data(i, 5:end);
    this_phi = Data(i,1);
    this_rt = Data(i,2);
    
    % Generate a canonically oriented distribution for the intrusion drift
    % rate, and then circularly shift the exponential term (Gt) by the number
    % of angular steps it takes to each the offset of the intrusion in question
    % and the target angle.
    intrusion_densities = cell(num_intrusions,1);
    for j = 1:num_intrusions
        % Find the number of angular steps to offset the exponential term by
        
        this_offset = this_trial_intrusions(j);
        num_steps_offset = round(this_offset/w);
        
        [this_intrusion_Gt, ~] = vdcircle3_shiftx(P_intrusion, t, Gt0, num_steps_offset);
        
        % Store in likelihood structure
        intrusion_densities{j} = this_intrusion_Gt;
        
    end
    % Even weights for each intrusion
    Gt_int = (intrusion_densities{1} + intrusion_densities{2} ...
        + intrusion_densities{3} + intrusion_densities{4} ...
        + intrusion_densities{5} + intrusion_densities{6} ...
        + intrusion_densities{7} + intrusion_densities{8} ...
        + intrusion_densities{9})/9;
    
    % Mix the memory-based processes, intrusions, and guesses
    Gt =  (1-gamma) * Gt_target + gamma * Gt_int;
    
    % Filter zeros
    Gt = max(Gt, epsx);
    % gtshort = max(gtshort, epsx);
    % Add nondecision times
    this_t = t + ter;
    
    % Convolve with Ter
    
    h = t(2) - t(1);
    if st > 2 * h
        m = round(st/h);
        n = length(t);
        fe = ones(1, m) / m;
        for k = 1:nw + 1
            gti = conv(Gt(k, :), fe);
            Gt(k,:) = gti(1:n);
        end
    end
    
    % Create mesh for interpolation
    [angle,time]=meshgrid(this_t, theta);
    like(i) = interp2(angle, time, Gt, this_rt, this_phi, 'linear');
end

% Out of range values returned as NaN's. Treat as contaminants - set small.
idx = isnan(like);
like(idx) = cden;

% Log-likelihoods.
log_like = log(like);

% Minimize sum of minus LL's across two conditions.
ll = sum(-log_like) + penalty;

% Calculate the aic
aic = 2 * ll + 2 * sum(Sel);
end