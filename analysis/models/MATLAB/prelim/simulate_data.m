function [simdata_cont, simdata_thresh] = simulate_data(cond)
%% simulate_data
% Generate sets of simulated data for each model using parameters that provided the best
% fit for each model for each participant. Each simulated dataset should be
% based on the same number of observations as the actual empirical
% dataset. Then, fit the simulated data with each model to see if model
% selection correctly identifies the model that generated that simulated
% data.
%% Load in the empirical data
load('Prolific.mat');
% Participants

% indices for condition
idx = cell2mat(data(:,3));

% Select out the condition
data = data(idx == cond,:);
n_participants = size(data,1);

% Determine the number of observations per participant
nobvs_low = double.empty(n_participants, 0);
for i = 1:n_participants
    this_participant_nobvs_low = size(data{i,1},1);
    nobvs_low(i) = this_participant_nobvs_low;
end

nobvs_high = double.empty(20, 0);
for i = 1:n_participants
    this_participant_nobvs_high = size(data{i,2},1);
    nobvs_high(i) = this_participant_nobvs_high;
end

%% Load in the best fitting parameters of each model to empirical data
load('2021_01_11_09_53_cond.mat')

% Select the relevant condition
if cond == 1
    cont = VP_seq;
    thresh = MX_seq;
else
    cont = VP_sim;
    thresh = MX_sim;
end

% Arguments for the diffusion process simulation code
tmax = 5.1;
nt = 301;
h = tmax / nt;
nw = 50;
d_angle = 2*pi/nw; %Angle space
%% Simulate the continuous model
simdata_cont = cell(n_participants,2);
for i = 1:n_participants
    % For this participant:
    
    % Parameters of best fit of the continuous model to this participant
    %    P = [v1, v2, eta,  a, Ter, st, sa]
    %          1   2    3    4   5   6   7
    pest = cont{i,8}(1,:);
    
    % Get number of trials to simulate (same as empirical dataset for that
    % participant in the low imageability condition)
    ntrials = nobvs_low(i);
    
    % This is the data structure that will contain the simulated
    % observations for this participant
    this_sim_low = double.empty(0, 2);
    for j = 1:ntrials
        
        % Arrange input vector that contains the  parameter values for simulation
        
        % Determine a drift rate by drawing from a gaussian with mean as the mean
        % drift and with standard deviation eta (drift variability)
        mu1 = max(0,normrnd(pest(1), pest(3)));
        mu2 = pest(2);
        % This is always equal to zero, since there was no evidence of systematic
        % bias in response error in empirical data
        sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
        sigma2 = 1;
        a = pest(4);
        
        
        P = [mu1, mu2, sigma1, sigma2, a];
        % Simulate a trial directly using dirisim
        [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
        
        % Add non-decision time to response time
        Ta = Ta + normrnd(pest(5), pest(6));
        
        % Store response error and response time of this trial
        this_sim_low(j, 1) = theta;
        this_sim_low(j, 2) = Ta;
    end
    simdata_cont{i,1} = this_sim_low;
    
    ntrials = nobvs_high(i);
    
    % Repeat, for the high conditions
    
    this_sim_high = double.empty(0, 2);
    for j = 1:ntrials
        
        % Arrange input vector that contains the  parameter values for simulation
        
        % Determine a drift rate by drawing from a gaussian with mean as the mean
        % drift and with standard deviation eta (drift variability)
        mu1 = max(0,normrnd(pest(1), pest(3)));
        mu2 = pest(2);
        % This is always equal to zero, since there was no evidence of systematic
        % bias in response error in empirical data
        sigma1 = 1; % This is the diffusion coefficient, and is always set to 1.
        sigma2 = 1;
        a = pest(4);
        
        
        P = [mu1, mu2, sigma1, sigma2, a];
        % Simulate a trial directly using dirisim
        [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
        
        % Add non-decision time to response time
        Ta = Ta + normrnd(pest(5), pest(6));
        
        % Store response error and response time of this trial
        this_sim_high(j, 1) = theta;
        this_sim_high(j, 2) = Ta;
    end
    simdata_cont{i,2} = this_sim_high;
    
end

%% Simulate the threshold model
simdata_thresh = cell(n_participants,2);
for i = 1:n_participants
    % Parameters of best fit of the continuous model to this participant
    %    P = [v1, v2, eta,   a1, a2, pi, Ter  st, sa]
    %          1   2    3    4   5   6    7   8   9
    pest = thresh{i,8}(1,:);
    
    % Get number of trials to simulate (same as empirical dataset for that
    % participant)
    ntrials = nobvs_low(i);
    
    % This is the data structure that will contain the simulated
    % observations for this participant
    this_sim_low = double.empty(0, 2);
    for j = 1:ntrials
        
        % Draw a random number and compare it to the mixing proportion (pi)
        % to determine if we sample from a positive drift or a zero drift
        % guessing process. pi, which is pest(6) represents the proportion
        % of trials that are driven by memory.
        
        if rand < pest(6) % Positive Drift
            
            % Arrange input vector that contains the  parameter values for simulation
            mu1 = pest(1);
            mu2 = pest(2);
            sigma1 = 1;
            sigma2 = 1;
            a = pest(4);
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial directly using dirisim
            [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            Ta = Ta + normrnd(pest(7), pest(8));
            
            % Store response error and response time of this trial
            this_sim_low(j, 1) = theta;
            this_sim_low(j, 2) = Ta;
            
        else % Zero Drift
            
            % Arrange input vector that contains the  parameter values for simulation
            mu1 = 0;
            mu2 = 0;
            sigma1 = 1;
            sigma2 = 1;
            a = pest(5);
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial directly using dirisim
            [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            Ta = Ta + normrnd(pest(7), pest(8));
            
            % Store response error and response time of this trial
            this_sim_low(j, 1) = theta;
            this_sim_low(j, 2) = Ta;
        end
    end
    simdata_thresh{i,1} = this_sim_low;
    
    
    % Repeat, for high condition
    ntrials = nobvs_high(i);
    % This is the data structure that will contain the simulated
    % observations for this participant
    this_sim_high = double.empty(0, 2);
    
    for j = 1:ntrials
        
        % Draw a random number and compare it to the mixing proportion (pi)
        % to determine if we sample from a positive drift or a zero drift
        % guessing process.
        
        if rand < pest(6) % Positive Drift
            
            % Arrange input vector that contains the  parameter values for simulation
            mu1 = pest(1);
            mu2 = pest(2);
            sigma1 = 1;
            sigma2 = 1;
            a = pest(4);
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial directly using dirisim
            [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            Ta = Ta + normrnd(pest(7), pest(8));
            
            % Store response error and response time of this trial
            this_sim_high(j, 1) = theta;
            this_sim_high(j, 2) = Ta;
            
        else % Zero Drift
            
            % Arrange input vector that contains the  parameter values for simulation
            mu1 = 0;
            mu2 = 0;
            sigma1 = 1;
            sigma2 = 1;
            a = pest(5);
            
            P = [mu1, mu2, sigma1, sigma2, a];
            % Simulate a trial directly using dirisim
            [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax);
            
            % Add non-decision time to response time
            Ta = Ta + normrnd(pest(7), pest(8));
            
            % Store response error and response time of this trial
            this_sim_high(j, 1) = theta;
            this_sim_high(j, 2) = Ta;
        end
    end
    simdata_thresh{i,2} = this_sim_high;
end
end