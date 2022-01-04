function[simulated_data] = simulate_temporal_flat_guess(data, pest)

%% Debugging
name = 'SIMULATE_TEMPORAL_FLAT_GUESS: ';
errmg1 = 'Incorrect number of parameters, exiting...';
%%
% Number of trials to simulate (should be the same as the actual dataset)
n_trials = length(data);

% Default number of simulations
n_sims = 50; % The number of times to simulate each trial

% Number of intrusions
num_intrusions = 9;

% Expected number of parameters
n_params = 15;
% Check the length of the parameter vector
if length(pest) ~= n_params
    [name, errmg1], length(pest), return;
end

P = pest;
% Assign parameter values
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

%% Assemble structure containing the weights of each component for each trial
backwards_similarity = (1-kappa)*exp(-lambda_b*(abs(-num_intrusions:-1)));
forwards_similarity = kappa*exp(-lambda_f*(abs(1:num_intrusions)));

% Concatenate, and normalise
temporal_similarity_values = [backwards_similarity, forwards_similarity];
temporal_similarity_values = (temporal_similarity_values/sum(temporal_similarity_values))';

% Scale temporal similarity values by gamma
temporal_similarity_values = temporal_similarity_values * gamma;

% Replace all raw lags with the corresponding normalised temporal
% similarity
lags = data(:,14:22);
[lag_val, ~, lag_index] = unique(lags);

temporal_similarities = temporal_similarity_values(lag_index);
temporal_similarities = reshape(temporal_similarities, size(lags));

target_weights = 1- sum(temporal_similarities,2);

weights = [target_weights, temporal_similarities];
weights = weights*(1-beta);

guess_weights = repmat(beta, n_trials, 1);
weights = [weights, guess_weights];

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