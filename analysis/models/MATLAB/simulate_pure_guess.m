function[simulated_data] = simulate_pure_guess(data, pest)

%% Debugging
name = 'SIMULATE_PURE_GUESS: ';
errmg1 = 'Incorrect number of parameters, exiting...';
%%
% Number of trials to simulate (should be the same as the actual dataset)
n_trials = length(data);

% Default number of simulations
n_sims = 50; % The number of times to simulate each trial

% Number of intrusions
n_intrusion = 9;

% Expected number of parameters
n_params = 8;
% Check the length of the parameter vector
if length(pest) ~= n_params
    [name, errmg1], length(pest), return;
end

P = pest;
% Assign parameter values
% Drift norm
v1_targ = P(1);
v2_targ = P(2);
eta = P(3);
a1 = P(4);
a2 = P(5);
beta = P(6);
ter = P(7);
st = P(8);

% Assume eta components in the x and y directions are the same
eta1_targ = eta;
eta2_targ = eta;

% Arguments for the simulation function
tmax = 5.1;
nt = 301;
h = tmax / nt;
% Each trial must be simulated one by one, as there is a unique
% configuration of n-1 intrusions for each trial (where n = number of
% trials)

% Assemble vector of proportions of the components
p = [(1-beta), beta];

% This is the data structure that will contain the simulated
% observations for this participant
simulated_data = zeros(n_trials*n_sims, 2);
idx = 1;

for i = 1:n_trials
    for j = 1:n_sims
        % Determine if this trial is an intrusion, and shift the theta by
        % a random offset from the set of possible non-targets for this
        % trial if so.
        trial_type = mnrnd(1,p);
        if  trial_type(1)
            mu1 = max(0,normrnd(v1_targ, eta1_targ));
            mu2 = max(0,normrnd(v2_targ, eta2_targ));
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            
            P = [mu1, mu2, sigma1, sigma2, a1];
            % Simulate a trial using dirisim
            [~, ~, T, theta, ~] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            T = T + normrnd(ter, st);
            
            % Store response error and response time of this trial
            simulated_data(idx, 1) = theta;
            simulated_data(idx, 2) = T;
            
            % Add one to index
            idx = idx + 1;
        elseif trial_type(2)
            mu1 = 0;
            mu2 = 0;
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            
            P = [mu1, mu2, sigma1, sigma2, a2];
            % Simulate a trial using dirisim
            [~, ~, T, theta, ~] = dirisim(P, h, tmax);
            
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