function [ll, aic, P, pest_penalty] = temporal_gradient_flat_guess(Pvar, Pfix, Sel, Data, badix)
% TEMPORAL_GRADIENT_FLAT_GUESS

% This is the temporal gradient intrusion model, where trial-to-trial
% variability in component weights affects ONLY memory, so guesses are flat

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
name = 'TEMPORAL_GRADIENT_MODEL: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Component weights do not sum to 1...';

%% Global variables

np = 15; % Number of parameters
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
% Ptemp = P;
% save Ptemp

% Drift norms
v1_targ = P(1);
v2_targ = P(2);
v1_int = P(3);
v2_int = P(4);
% Trial-trial drift variability
eta_targ = P(5);
eta_int = P(6);
% Decision Criteria
a_targ = P(7);
a_int = a_targ;
a_guess = P(8);
% Component Proportions
gamma = P(9);
beta = P(10);
% Temporal Gradient
kappa = P(11); %Scaling parameter for forwards vs backwards intrusion decay slope
lambda_b = P(12); % Decay of the backwards slope
lambda_f = P(13); % Decay of the forwards slope
% Nondecision Time
ter = P(14);
st = P(15);

% Check to see if component weights sum to 1.
if gamma + beta > 1
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
%   [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  ag,  gamma, beta, kappa, l_b, l_f, Ter, st]
% ----------------------------------------------------------------------------
Ub= [ 8,   0,    8,   0,   1,    1,       4.5, 4.5,  1.0,  1.0,    1.0,  5,   5, 0.3,  0.2];
Lb= [ 1,   0,    0,   0,   0,    0,       0.1, 0.5,  0,    0,      0,    0,   0,  0,    0];
Pub=[ 7.5, 0,    7.5, 0,   0.9,  0.9,     4.0, 4.0,  0.99, 0.8,    0.9,  4.5, 4.5,0.25, 0.15];
Plb=[ 1.5,   0,    0,   0,   0,    0,       0.7, 0.7,  0.01, 0.01,   0.01, 0.01,0.01,0.01, 0.01];

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


P_guess = [0, 0, 0, 0, sigma, a_guess];
[T_guess, Gt_guess, theta_guess] = vdcircle3x(P_guess, nw, h, tmax, badix);

%% Temporal gradient for intrusion process
% Normalise temporal similarity for all possible lag positions across all trials

% Raw temporal similarity values from the lags -9 to 9, skipping 0 (in a
% list of 10)
backwards_similarity = (1-kappa)*exp(-lambda_b*(abs(-num_intrusions:-1)));
forwards_similarity = kappa*exp(-lambda_f*(abs(1:num_intrusions)));

% Concatenate, and normalise
temporal_similarity_values = [backwards_similarity, forwards_similarity];
temporal_similarity_values = (temporal_similarity_values/sum(temporal_similarity_values))';

% Scale temporal similarity values by gamma
temporal_similarity_values = temporal_similarity_values * gamma;

% Replace all raw lags with the corresponding normalised temporal
% similarity
lags = Data(:,14:22);
[lag_val, ~, lag_index] = unique(lags);

temporal_similarities = temporal_similarity_values(lag_index);
temporal_similarities = reshape(temporal_similarities, size(lags));



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
    this_trial_intrusions = Data(i, 5:13);
    % Get the intrusion weights, based on temporal similarities, for this
    % trial. Scale these by (1-beta)
    this_intrusion_weights = temporal_similarities(i, :) * (1 - beta);
    target_weight = (1 - sum(temporal_similarities(i, :))) * (1 - beta);
    
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
    % Weight each intrusion joint distribution with the matching normalised
    % temporal similarity
    Gt_int = (intrusion_densities{1} * this_intrusion_weights(1)...
        + intrusion_densities{2} * this_intrusion_weights(2)...
        + intrusion_densities{3} * this_intrusion_weights(3)...
        + intrusion_densities{4} * this_intrusion_weights(4)...
        + intrusion_densities{5} * this_intrusion_weights(5)...
        + intrusion_densities{6} * this_intrusion_weights(6)...
        + intrusion_densities{7} * this_intrusion_weights(7)...
        + intrusion_densities{8} * this_intrusion_weights(8)...
        + intrusion_densities{9} * this_intrusion_weights(9));
    
    % Mix the memory-based processes, intrusions, and guesses
    Gt =  (target_weight * Gt_target) ...
        + Gt_int + (beta * Gt_guess);
    
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

% Calculate the AIC
aic = 2 * ll + 2*sum(Sel);
end
