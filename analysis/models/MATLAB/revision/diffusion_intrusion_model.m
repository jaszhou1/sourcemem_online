function [ll, aic, P, pest_penalty] = diffusion_intrusion_model(Pvar, Pfix, Pbounds, Sel, Data, badix)

% Allows for different parameters for drift, intrusion, guess across the
% three conditions
%% Debugging
name = 'DIFFUSION_INTRUSION: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Negative trial weight, exiting...';

%% Global variables

np = 23; % Number of parameters
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
eta1_targ = P(5);
eta2_targ = P(6);
eta1_int = P(7);
eta2_int = P(8);

% Decision Criteria
a_targ = P(9);
a_int = a_targ;
a_guess = P(10);

% Guess proportion
beta = P(11);

% Intrusion weighting
gamma = P(12);

% Temporal Gradient
tau = P(13); %Weight forwards vs backwards intrusion decay slope
lambda_b = P(14); % Decay of the backwards slope
lambda_f = P(15); % Decay of the forwards slope

% Spatial Gradient
zeta = P(16); %precision for Shepard similarity function (perceived spatial distance)
rho = P(17); % Spatial component weight in intrusion probability calculation

% Similarity component weighting
chi = P(18); % Item v Context
psi = P(19); % Semantic v Ortho, Low
iota = P(20); % Ortho decay, Low
upsilon = P(21); % Semantic decay, Low

% Nondecision Time
ter = P(22);
st = P(23);

% The reason Pbound is passed in as an argument rather than being defined
% in the model code like Philip usually does is so I can set the lowerbound
% to 0 when I want parameters to be equal across conditions, but be higher
% in the freer version of the model.

sigma = 1.0;

% If a condition-dependant parameter is missing, set it to the default
% (primary) value, which is the base value. The model assumes that there is
% no difference across conditions

%% Parameter bounds
penalty = 0; % Set the penalty to an initial value of zero
pest_penalty(1,:) = P;

Ub = Pbounds(1,:);
Lb = Pbounds(2,:);
Pub = Pbounds(3,:);
Plb = Pbounds(4,:);

if any(P - Ub > 0) || any(Lb - P > 0)
    ll = 1e7 + ...
        1e3 * (sum(max(P - Ub, 0).^2) + sum(max(Lb - P, 0).^2));
    aic = 0;
    return
else
    penalty =  1e3 * (sum(max(P - Pub, 0).^2) + sum(max(Plb - P, 0).^2));
end


pest_penalty(2,:) = max(P - Pub, 0).^2 + max(Plb - P, 0).^2;


%% Find likelihood of each observation


%% On-target retrieval likelihoods
P_targ = [v1_targ, v2_targ, eta1_targ, eta2_targ, sigma, a_targ];
[t, Gt_target, theta] = vdcircle3x(P_targ, nw, h, tmax, badix);


P_guess = [0, 0, 0, 0, sigma, a_guess];
[T_guess, Gt_guess, theta_guess] = vdcircle3x(P_guess, nw, h, tmax, badix);

%% Temporal gradient for intrusion process
% Raw temporal similarity values from the lags -9 to 9, skipping 0 (in a
% list of 10, normalise by dividing by 10)

backwards_similarity = (1-tau)*exp(-lambda_b*(abs((-num_intrusions:-1)/10)));
forwards_similarity = tau*exp(-lambda_f*((abs(1:num_intrusions)/10)));

% Concatenate, and normalise
temporal_similarity_values = [backwards_similarity, forwards_similarity]';
temporal_similarity_values = temporal_similarity_values/max(temporal_similarity_values);
%temporal_similarity_values = (temporal_similarity_values/sum(temporal_similarity_values))';

% Replace all raw lags with the corresponding normalised temporal
% similarity
lags = Data(:,14:22);
[lag_val, ~, lag_index] = unique(lags);

temporal_similarities = temporal_similarity_values(lag_index);
temporal_similarities = reshape(temporal_similarities, size(lags));

% Spatial gradient
spatial_distances = Data(:,23:31)/2; %2 is maximum cosine distance

spatial_similarities = shepard(spatial_distances, zeta);

% Orthographic similarity
orthographic_distances = 1- Data(:, 33:41);
orthographic_similarities = shepard(orthographic_distances, iota);

% Semantic similarity
semantic_similarities = shepard(Data(:, 42:50), upsilon);

% Normalise all components
temporal_similarities = temporal_similarities./(max(temporal_similarities(:)));
spatial_similarities = spatial_similarities./(max(spatial_similarities(:)));
orthographic_similarities = orthographic_similarities./(max(orthographic_similarities(:)));
semantic_similarities = semantic_similarities./(max(semantic_similarities(:)));

intrusion_similarities = ((1-chi) .* ((temporal_similarities.^(1-rho)) .* (spatial_similarities.^rho)))...
    + (chi .* ((orthographic_similarities.^(1-psi)) .* (semantic_similarities.^psi)));

% Scale spatiotemporal similarity values by gamma, the overall intrusion
% scaling parameter
intrusion_similarities = intrusion_similarities * gamma;

target_weights = 1- sum(intrusion_similarities,2);
weights = horzcat(target_weights, intrusion_similarities) * (1-beta);
guess_weights = 1- sum(weights,2);
all_weights = horzcat(weights, guess_weights);

% Check no weights are negative
if any(all_weights(:) < 0)
    ll = 1e7;
    aic = 0;
    penalty = 1e7;
    pest_penalty(1,:) = P;
    [name, errmg3], return;
end

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
    % Get the serial position of this trial
    this_trial_pos = Data(i, 32);
    
    % Determine which beta value should be used based on the serial
    % position of the trial
%     if this_trial_pos == 1
%         this_beta = beta_primacy;
%     elseif this_trial_pos == 10
%         this_beta = beta_recency;
%     else
%         this_beta = beta;
%     end
%    

%    this_beta = beta;
    
    % Find the intrusions for this trial
    this_trial_intrusions = Data(i, 5:13);
    
    % Get the intrusion weights, based on temporal similarities, for this
    % trial. Scale these by (1-beta)
%     this_intrusion_weights = spatiotemporal_similarities(i, :) * (1 - this_beta);
%     target_weight = (1 - sum(spatiotemporal_similarities(i, :))) * (1 - this_beta);

% Instead, get the vector of all weights from the matrix of weights across
% all trials
    this_trial_weights = all_weights(i, :);
    this_target_weight = this_trial_weights(1);
    this_intrusion_weights = this_trial_weights(2:10);
    this_guess_weight = this_trial_weights(11);
    
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
    Gt =  (this_target_weight * Gt_target) ...
        + Gt_int + (this_guess_weight * Gt_guess);
    
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

% Calculate the BIC
aic = 2 * ll + 2 * sum(Sel);

end

function [similarity] = shepard(distance, k)
similarity = exp(-k * distance);
end
