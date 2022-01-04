function[simulated_data] = simulate_threshold(data, pest)

%% Debugging
name = 'SIMULATE_INTRUSION: ';
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

% Assign parameter values
% Drift norm
v1 = pest(1);
v2 = pest(2);
eta1 = pest(3);
eta2 = pest(3);
% Criterion
a1 = pest(4);
a2 = pest(5);
% Proportion of Intrusions
beta = pest(6);
% Nondecision time and variability
ter = pest(7);
st = pest(8);

% Arguments for the simulation function
tmax = 5.1;
nt = 301;
h = tmax / nt;
% Each trial must be simulated one by one, as there is a unique
% configuration of n-1 intrusions for each trial (where n = number of
% trials)

% This is the data structure that will contain the simulated
% observations for this participant
simulated_data = zeros(n_trials*n_sims, 2);
idx = 1;

for i = 1:n_trials
    this_intrusions = data(i,4:end);
    for j = 1:n_sims
        % Determine if this trial is an intrusion, and shift the theta by
        % a random offset from the set of possible non-targets for this
        % trial if so.
        if binornd(1, beta)
            mu1 = 0;
            mu2 = 0;
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a2; % not necesarry, but change this if for some reason, intrusions have different a
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial using dirisim
            [~,~, T, theta,~] = dirisim(P, h, tmax);
            % Shift the generated theta by the offset between target and chosen non-target
            intrusion_idx = randi(n_intrusion);
            this_offset = this_intrusions(intrusion_idx);
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
        else
            mu1 = max(0,normrnd(v1, eta1));
            mu2 = max(0,normrnd(v2, eta2));
            sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
            sigma2 = 1;
            a = a1; % not necesarry, but change this if for some reason, intrusions have different a
            
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
        end
    end
end