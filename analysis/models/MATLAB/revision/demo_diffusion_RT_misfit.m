% So the question is why does the diffusion model qualitatively misfit the
% distribution of response errors so badly.
%
% "Is it that the model can't fit a distribution with the observed shape,
% and this is it doing the best it can? Or like if the model could fit the
% response distribution but this would cause a specific worsening of the RT fit.
% Or whether there are any promising modifications or additional parameters
% for future work that could increase the model's flexibility and help it
% improve the misfit.
%
% I want to hand pick some parameters that help with the marginal response
% error and show that it causes misses by exaggerating a slow error pattern
% that does not exist in the data. Basically, p. 573 of Smith et al. (2020)

load('exp2_data.mat')
close all
% Participant 2 is probably the best example of a case where the simple
% response error model does a good job of capturing the response error
% distribution, but the diffusion model just doesn't do a good job.

this_data = vertcat(data{:});

% original estimated parameters
% v1_targ = 4.71;
% v2_targ = 0;
% v1_int = 1.46;
% v2_int = 0;
% eta1_targ = 0.6;
% eta2_targ = 0.6;
% eta1_int = 0.08;
% eta2_int = 0.08;
% a_targ = 3.03;
% a_guess = 1.34;
% gamma = 0.07;
% beta = 0.27;
% kappa = 0.72;
% lambda_b = 0.17;
% lambda_f = 0.62;
% zeta = 0.78;
% rho = 0.86;
% Ter = 0.07;
% st = 0;

% Free eta1 and eta2
v1_targ = 4.71;
v2_targ = 0;
v1_int = 0.94;
v2_int = 0;
eta1_targ = 0.91;
eta2_targ = 0.04;
eta1_int = 0.50;
eta2_int = 0.05;
a_targ = 2.58;
a_guess = 1.06;
gamma = 0.06;
beta = 0.25;
kappa = 0.59;
lambda_b = 1.11;
lambda_f = 1.05;
zeta = 0.39;
rho = 0.28;
Ter = 0.15;
st = 0;


% Bonus params to try and get the 0.9 RTs for accurate responses
% Idea is that target responses are composed of a mix of normal memory
% responses, and some that take a long time to terminate, with a big
% criterion or maybe a big nondecision time. Possibly need to try a lower
% diffusion coefficient (basically, slower clock), but this would have a
% similar effect, just different psychological interpretation, I think...
targ2 = 0.10;
targ3 = 0.20;
v1_targ_2 = v1_targ;
v2_targ_2 = v2_targ;
Ter_2 = Ter;
a2_targ = a_targ + 5;


params = [v1_targ, v2_targ, v1_int, v2_int, eta1_targ, eta2_targ, eta1_int, eta2_int,...
    a_targ,	 a_guess, gamma, beta, kappa, lambda_b, lambda_f, ...
    zeta, rho, Ter, st, targ2, v1_targ_2, v2_targ_2, a2_targ, Ter_2];


params_3 = [v1_targ, v2_targ, v1_int, v2_int, eta1_targ, eta2_targ, eta1_int, eta2_int,...
    a_targ,	 a_guess, gamma, beta, kappa, lambda_b, lambda_f, ...
    zeta, rho, Ter, st, targ3, v1_targ_2, v2_targ_2, a2_targ, Ter_2];

% sim_data = simulate_spatiotemporal_eta(this_data, params, 0);

plot_simulation(this_data, sim_data)
% 
sim_data2 = simulate_spatiotemporal_eta(this_data, params, 1);
plot_simulation(this_data, sim_data2)

sim_data3 = simulate_spatiotemporal_eta(this_data, params_3, 1);
plot_simulation(this_data, sim_data3)
function[simulated_data] = simulate_spatiotemporal_eta(data, pest, second_target_process)
%%
% Number of trials to simulate (should be the same as the actual dataset)
n_trials = length(data);

% Default number of simulations
n_sims = 50; % The number of times to simulate each trial

% Number of intrusions
num_intrusions = 9;
P = pest;

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
% Component Proportions
gamma = P(11);
beta = P(12);
% beta_primacy = P(11);
% beta_recency = P(12);
% Temporal Gradient
kappa = P(13); %Scaling parameter for forwards vs backwards intrusion decay slope
lambda_b = P(14); % Decay of the backwards slope
lambda_f = P(15); % Decay of the forwards slope
zeta = P(16); %precision for Shepard similarity function (perceived spatial distance)
rho = P(17); % Spatial component weight in intrusion probability calculation
% Nondecision Time
Ter = P(18);
st = P(19);

% Try an extra targ criterion parameter, see if it gets the 0.9 RT quantile
% on accurate responses
if second_target_process
    targ2 = P(20);
    v1_targ_2 = P(21);
    v2_targ_2 = P(22);
    a2_targ = P(23);
    Ter_2 = P(24);
else
    targ2 = 0;
    % Just in case
    v1_targ_2 = v1_targ;
    v2_targ_2 = v2_targ;
    a2_targ = a_targ;
end


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

spatiotemporal_similarities = (temporal_similarities.^(1-rho)) .* (spatial_similarities.^rho);

% Scale spatiotemporal similarity values by gamma, the overall intrusion
% scaling parameter
spatiotemporal_similarities = spatiotemporal_similarities * gamma;

target_weights = 1- sum(spatiotemporal_similarities,2);
weights = horzcat(target_weights, spatiotemporal_similarities) * (1-beta);
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
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            if binornd(1, targ2)
                mu1 = normrnd(v1_targ_2, eta1_targ);
                mu2 = normrnd(v2_targ_2, eta2_targ);
                a = a2_targ;
                ter = Ter_2;
            else
                mu1 = normrnd(v1_targ, eta1_targ);
                mu2 = normrnd(v2_targ, eta2_targ);
                a = a_targ;
                ter = Ter;
            end

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
            ter = Ter;

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
            mu1 = normrnd(v1_int, eta1_int);
            mu2 = normrnd(v2_int, eta2_int);
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a_int;
            ter = Ter;

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
