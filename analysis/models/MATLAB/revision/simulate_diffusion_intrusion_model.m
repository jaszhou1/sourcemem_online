function[simulated_data] = simulate_diffusion_intrusion_model(Data, pest, participant)
%% Error Messages
name = 'SIMULATE_DIFFUSION_INTRUSION_MODEL: ';
errmg1 = 'Incorrect number of parameters, exiting...';

% Arguments for the simulation function
tmax = 10;
nt = 301;
h = tmax / nt;

% Default number of simulations
n_sims = 1; % The number of times to simulate each trial

% Number of intrusions
num_intrusions = 7;

% Expected number of parameters
n_params = 37;
% Check the length of the parameter vector
if length(pest) ~= n_params
    [name, errmg1], length(pest), return;
end

% Drift norms
v1_targ = pest(1);
v2_targ = pest(2);
v1_int = pest(3);
v2_int = pest(4);

% Trial-trial drift variability
eta1_targ = pest(5);
eta2_targ = pest(6);
eta1_int = pest(7);
eta2_int = pest(8);

% Decision Criteria
a_targ = pest(9);
a_int = a_targ;
a_guess = pest(10);
% Component Proportions
beta = pest(11);
gamma = pest(12);

% beta_primacy = P(11);
% beta_recency = P(12);
% Temporal Gradient
tau = pest(13); %Weight forwards vs backwards intrusion decay slope
lambda_b = pest(14); % Decay of the backwards slope
lambda_f = pest(15); % Decay of the forwards slope
zeta = pest(16); %precision for Shepard similarity function (perceived spatial distance)
rho = pest(17); % Spatial component weight in intrusion probability calculation
chi = pest(18); % Item v Context, Low
psi = pest(19); % Semantic v Ortho, Low
iota = pest(20); % Ortho decay, Low
upsilon = pest(21); % Semantic decay, Low
% Nondecision Time
ter = pest(22);
st = pest(23);


% Initialise empty dataset to store simulated data
simulated_data = [];

% Raw temporal similarity values from the lags -9 to 9, skipping 0 (in a
% list of 10)
backwards_similarity = (1-tau)*exp(-lambda_b*(abs(-num_intrusions:-1)));
forwards_similarity = tau*exp(-lambda_f*(abs(1:num_intrusions)));

% Concatenate, and normalise
temporal_similarity_values = [backwards_similarity, forwards_similarity];
temporal_similarity_values = (temporal_similarity_values/sum(temporal_similarity_values))';

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

intrusion_similarities = ((temporal_similarities.^(1-rho)) .* (spatial_similarities.^rho)).^(1-chi)...
    .* ((orthographic_similarities.^(1-psi)) .* (semantic_similarities.^psi)).^chi;

% Scale spatiotemporal similarity values by gamma, the overall intrusion
% scaling parameter
intrusion_similarities = intrusion_similarities * gamma;

target_weights = 1- sum(intrusion_similarities,2);
weights = horzcat(target_weights, intrusion_similarities) * (1-beta);
guess_weights = 1- sum(weights,2);
weights = horzcat(weights, guess_weights);

% Now that weights are done, simulate each trial n times
n_trials = size(Data, 1);

% This is the data structure that will contain the simulated
% observations for this participant
this_cond_data = zeros(n_trials*n_sims, 49);
idx = 1;

for i = 1:n_trials
    this_intrusions = Data(i, 5:13);
    this_weights = weights(i, :);
    for j = 1:n_sims
        % Determine if this trial is an intrusion, memory, or guess
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
            this_cond_data(idx, 1) = theta;
            this_cond_data(idx, 2) = T;

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
            this_cond_data(idx, 1) = theta;
            this_cond_data(idx, 2) = T;

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
            this_cond_data(idx, 1) = theta;
            this_cond_data(idx, 2) = T;

        end
        % Add the rest of the data structure to the simulated data
        this_cond_data(idx, 3) = Data(i, 3); % response angle
        this_cond_data(idx, 4) = Data(i, 4); % target angle
        this_cond_data(idx, 5) = Data(i, 32); % trial number (in block)
        this_cond_data(idx, 6:14) = Data(i, 5:13); % intrusion offsets
        this_cond_data(idx, 15:23) = Data(i, 14:22); % intrusion lags
        this_cond_data(idx, 24:32) = Data(i, 23:31); % intrusion spatial distances
        this_cond_data(idx, 33:41) = Data(i, 33:41); % Orthographic
        this_cond_data(idx, 42:50) = Data(i, 42:50); % Semantic
        this_cond_data(idx, 51) = participant;
        % Add one to index
        idx = idx + 1;
    end
end
simulated_data = vertcat(simulated_data, this_cond_data);
end


function [similarity] = shepard(distance, k)
similarity = exp(-k * distance);
end
