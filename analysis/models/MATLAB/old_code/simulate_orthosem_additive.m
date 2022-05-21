function[simulated_data] = simulate_orthosem_additive(data, pest)

%% Debugging
name = 'SIMULATE_ORTHOSEM_ADDITIVE: ';
errmg1 = 'Incorrect number of parameters, exiting...';
%%
% Number of trials to simulate (should be the same as the actual dataset)
n_trials = length(data);

% Default number of simulations
n_sims = 50; % The number of times to simulate each trial

% Number of intrusions
num_intrusions = 9;

% Expected number of parameters
n_params = 19;
% Check the length of the parameter vector
if length(pest) ~= n_params
    [name, errmg1], length(pest), return;
end

P = pest;
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
% beta_primacy = P(11);
% beta_recency = P(12);
% Temporal Gradient
kappa = P(11); %Scaling parameter for forwards vs backwards intrusion decay slope
lambda_b = P(12); % Decay of the backwards slope
lambda_f = P(13); % Decay of the forwards slope
zeta = P(14); %precision for Shepard similarity function (perceived spatial distance)
rho = P(15); % Spatial component weight in intrusion probability calculation
chi = P(16); %Spatiotemporal vs Word feature weighting for intrusion
psi = P(17); %Weighting of semantic component within word features (chi)
% Nondecision Time
ter = P(18);
st = P(19);


% Assume eta components in the x and y directions are the same
eta1_targ = eta_targ;
eta2_targ = eta_targ;
eta1_int = eta_int;
eta2_int = eta_int;

% Arguments for the simulation function
tmax = 5.1;
nt = 301;
h = tmax / nt;
% Each trial must be simulated one by one, as there is a unique
% configuration of n-1 intrusions for each trial (where n = number of
% trials)

%% Temporal gradient for intrusion process
% Normalise temporal similarity for all possible lag positions across all trials

% Raw temporal similarity values from the lags -9 to 9, skipping 0 (in a
% list of 10)
backwards_similarity = (1-kappa)*exp(-lambda_b*(abs(-num_intrusions:-1)));
forwards_similarity = kappa*exp(-lambda_f*(abs(1:num_intrusions)));

% Concatenate, and normalise
temporal_similarity_values = [backwards_similarity, forwards_similarity];
temporal_similarity_values = (temporal_similarity_values/sum(temporal_similarity_values))';

% Replace all raw lags with the corresponding normalised temporal
% similarity
lags = data(:,14:22);
[lag_val, ~, lag_index] = unique(lags);

temporal_similarities = temporal_similarity_values(lag_index);
temporal_similarities = reshape(temporal_similarities, size(lags));

% Multiply temporal similarity with spatial similarity
spatial_distances = data(:,23:31);
spatial_similarities = shepard(spatial_distances, zeta);

% Orthographic similarity
orthographic_similarities = data(:, 33:41);

% Semantic similarity
semantic_similarities = data(:, 42:50);

intrusion_similarities = ((temporal_similarities.^(1-rho)) .* (spatial_similarities.^rho)).*(1-chi)...
    + ((orthographic_similarities.^(1-psi)) .* (semantic_similarities.^psi)).*chi;

% Scale spatiotemporal similarity values by gamma, the overall intrusion
% scaling parameter
intrusion_similarities = intrusion_similarities * gamma;

% Scale spatiotemporal similarity values by gamma, the overall intrusion
% scaling parameter
intrusion_similarities = intrusion_similarities * gamma;

target_weights = 1- sum(intrusion_similarities,2);
weights = horzcat(target_weights, intrusion_similarities) * (1-beta);
guess_weights = 1- sum(weights,2);
weights = horzcat(weights, guess_weights);

%% Simulate trials
% This is the data structure that will contain the simulated
% observations for this participant
simulated_data = zeros(n_trials*n_sims, 2);
idx = 1;

for i = 1:n_trials
    this_intrusions = data(i,5:13);
    this_weights = weights(i,:);
    for j = 1:n_sims
        % Determine if this trial is an intrusion, and shift the theta by
        % a random offset from the set of possible non-targets for this
        % trial if so.
        trial_type = mnrnd(1,this_weights);
        
        if trial_type(1) % This is the target
            mu1 = max(0,normrnd(v1_targ, eta1_targ));
            mu2 = max(0,normrnd(v2_targ, eta2_targ));
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a_targ;
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial using dirisim
            [~, ~, T, theta, ~] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            T = T + normrnd(ter, st);
            
            % Store response error and response time of this trial
            simulated_data(idx, 1) = theta;
            simulated_data(idx, 2) = T;
            
            % Add one to index
            idx = idx + 1;
            
        elseif trial_type(length(trial_type)) % This is a guess
            mu1 = 0;
            mu2 = 0;
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a_guess;
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial using dirisim
            [~, ~, T, theta, ~] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            T = T + normrnd(ter, st);
            
            % Store response error and response time of this trial
            simulated_data(idx, 1) = theta;
            simulated_data(idx, 2) = T;
            
            % Add one to index
            idx = idx + 1;
        else % This is an intrusion
            mu1 = max(0,normrnd(v1_int, eta1_int));
            mu2 = max(0,normrnd(v2_int, eta2_int));
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a_int;
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial using dirisim
            [~,~, T, theta,~] = dirisim(P, h, tmax);
            % Shift the generated theta by the offset between target and chosen non-target
            intrusion_idx = trial_type(2:end-1);
            this_offset = this_intrusions(intrusion_idx == 1);
            theta = theta + this_offset;
            
            % If this shift exceeds the range of -pi to pi, then convert it
            % to be within that range
            theta = wrapToPi(theta);
            
            % Add non-decision time to response time
            T = T + normrnd(ter, st);
            
            % Store response error and response time of this trial
            simulated_data(idx, 1) = theta;
            simulated_data(idx, 2) = T;
            
            % Add one to index
            idx = idx + 1;
        end
    end
end
end

function [spatial_similarity] = shepard(distance, k)
spatial_similarity = exp(-k * distance);
end
