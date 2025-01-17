%% RUN_FITS_EXP2.M
% This is the top-level script to fit diffusion models to Experiment 1 data
% and save simulated data from estimated parameters for plotting in R.

% Read in the data
% data = read_sourcemem_data();
load('exp2_data.mat')
n_participants = length(data);
n_runs = 3;
num_workers = maxNumCompThreads/2 - 1; % Maximum number of workers

%% Threshold Model
pure_guess = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_pure_guess(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            pure_guess(i,:) = this_fit;
        end
    end
end
filename = [datestr(now,'yyyy_mm_dd_HH'),'_Experiment2_diffusion_temp'];
save(filename)

% Simulate data, concatenate participants, and save simulated dataset
simulated_pure_guess = [];
for i = 1:n_participants
    % Add zeros to estimated parameters to make it play nice with
    % simulation of more complex model.
    P = pure_guess{i,3};
    this_pest = [P(1), P(2), 0, 0, P(3), 0, P(4), P(5), 0, P(6), P(7), P(8)];

    this_simulated_data = simulate_three_component(data{i}, this_pest);
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_pure_guess = vertcat(simulated_pure_guess, this_simulated_data);
end
save(filename)

%% Pure Intrusion Model

pure_intrusion = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_pure_intrusion_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            pure_intrusion(i,:) = this_fit;
        end
    end
end

% Simulate data, concatenate participants, and save simulated dataset
simulated_pure_intrusion = [];
for i = 1:n_participants
    this_simulated_data = simulate_pure_intrusion(data{i}, pure_intrusion{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_pure_intrusion = vertcat(simulated_pure_intrusion, this_simulated_data);
end

save(filename)
% %% Three-Component Model
% % i.e. Memory + Guess + Flat Intrusions
% 
% flat_intrusion = cell(n_participants,4);
% 
% parfor (i = 1:n_participants, num_workers)
%     % Initial log likelihood value
%     ll = 1e7;
%     % Run each participant nrun times
%     this_fit = cell(1,4);
%     for j = 1:n_runs
%         this_participant_data = data{i};
%         [ll_new, aic, pest, pest_penalty] = fit_three_component_model(this_participant_data);
%         % If this ll is better than the last one, replace it in the saved
%         % structure
%         if (ll_new < ll)
%             ll = ll_new;
%             this_fit{1} = ll_new;
%             this_fit{2} = aic;
%             this_fit{3} = pest;
%             this_fit{4} = pest_penalty;
%             flat_intrusion(i,:) = this_fit;
%         end
%     end
% end
% 
% % Simulate data, concatenate participants, and save simulated dataset
% simulated_flat_intrusion = [];
% for i = 1:n_participants
%     this_simulated_data = simulate_three_component(data{i}, flat_intrusion{i,3});
%     % Label this dataset with participant number
%     this_simulated_data(:,3) = i; 
%     simulated_flat_intrusion = vertcat(simulated_flat_intrusion, this_simulated_data);
% end
% save(filename)
%% Three Component Model
flat_intrusion_eta = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_three_component_model_eta(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            flat_intrusion_eta(i,:) = this_fit;
        end
    end
end

% Simulate data, concatenate participants, and save simulated dataset
simulated_flat_intrusion_eta = [];
for i = 1:n_participants
    this_simulated_data = simulate_three_component(data{i}, flat_intrusion_eta{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_flat_intrusion_eta = vertcat(simulated_flat_intrusion_eta, this_simulated_data);
end
save(filename)
%% Temporal Gradient Model
temporal = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_temporal_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            temporal(i,:) = this_fit;
        end
    end
end

% Simulate data, concatenate participants, and save simulated dataset
simulated_temporal = [];
for i = 1:n_participants
    this_simulated_data = simulate_intrusion_gradient_model(data{i}, temporal{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_temporal = vertcat(simulated_temporal, this_simulated_data);
end
save(filename)
%% Spatiotemporal Model
spatiotemporal = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_spatiotemporal_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            spatiotemporal(i,:) = this_fit;
        end
    end
end

filename = [datestr(now,'yyyy_mm_dd_HH'),'_temp'];
save(filename)

% Simulate data, concatenate participants, and save simulated dataset
simulated_spatiotemporal = [];
for i = 1:n_participants
    this_simulated_data = simulate_intrusion_gradient_model(data{i}, spatiotemporal{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_spatiotemporal = vertcat(simulated_spatiotemporal, this_simulated_data);
end

%% Spatiotemporal * Orthography Model
ortho = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_ortho_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            ortho(i,:) = this_fit;
        end
    end
end

filename = [datestr(now,'yyyy_mm_dd_HH'),'_temp'];
save(filename)

% Simulate data, concatenate participants, and save simulated dataset
simulated_ortho = [];
for i = 1:n_participants
    this_simulated_data = simulate_intrusion_gradient_model(data{i}, ortho{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_ortho = vertcat(simulated_ortho, this_simulated_data);
end

%% Spatiotemporal * Semantic Model
semantic = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_sem_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            semantic(i,:) = this_fit;
        end
    end
end

filename = [datestr(now,'yyyy_mm_dd_HH'),'_temp'];
save(filename)

% Simulate data, concatenate participants, and save simulated dataset
simulated_semantic = [];
for i = 1:n_participants
    this_simulated_data = simulate_intrusion_gradient_model(data{i}, semantic{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_semantic = vertcat(simulated_semantic, this_simulated_data);
end

%% Spatiotemporal * ortho * Semantic Model
multi = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_orthosem_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            multi(i,:) = this_fit;
        end
    end
end

filename = [datestr(now,'yyyy_mm_dd_HH'),'_temp'];
save(filename)

% Simulate data, concatenate participants, and save simulated dataset
simulated_multi = [];
for i = 1:n_participants
    this_simulated_data = simulate_intrusion_gradient_model(data{i}, multi{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_multi = vertcat(simulated_multi, this_simulated_data);
end

%% Spatiotemporal + ortho * Semantic Model
add = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_orthosem_additive_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            ll = ll_new;
            this_fit{1} = ll_new;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            add(i,:) = this_fit;
        end
    end
end

filename = [datestr(now,'yyyy_mm_dd_HH'),'_temp'];
save(filename)

%Simulate data, concatenate participants, and save simulated dataset
simulated_add = [];
for i = 1:n_participants
    this_simulated_data = simulate_orthosem_additive(data{i}, add{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_add = vertcat(simulated_add, this_simulated_data);
end


% Save workspace
filename = [datestr(now,'yyyy_mm_dd_HH'),'_Experiment2_diffusion'];
save(filename)

% Output fit statistics and parameter estimates to .csv
filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_pure_guess.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, Ter, st';
param_to_csv(filename, 1:n_participants, pure_guess, 'Pure Guess', header_line);

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_pure_intrusion.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, gamma,  Ter, st';
param_to_csv(filename, 1:n_participants, pure_intrusion, 'Pure Intrusion', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_flat_intrusion.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, Ter, st';
param_to_csv(filename, 1:n_participants, flat_intrusion_eta, 'Intrusion + Guess', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_temporal.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
param_to_csv(filename, 1:n_participants, temporal, 'Temporal Gradient', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_spatiotemporal.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
param_to_csv(filename, 1:n_participants, spatiotemporal, 'Spatiotemporal Gradient', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_ortho.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
param_to_csv(filename, 1:n_participants, ortho, 'Orthographic', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_semantic.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
param_to_csv(filename, 1:n_participants, semantic, 'Semantic', header_line)

% filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_additive.csv'];
% header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
% param_to_csv(filename, 1:n_participants, add, 'Additive', header_line)

filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_multiplicative.csv'];
header_line = 'participant, model_name, AIC, v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st';
param_to_csv(filename, 1:n_participants, multi, 'Multiplicative', header_line)

%csvwrite('sim_add.csv', simulated_add)
csvwrite('sim_flat.csv', simulated_flat_intrusion_eta)
csvwrite('sim_multi.csv', simulated_multi)
csvwrite('sim_ortho.csv', simulated_ortho)
csvwrite('sim_pure_guess.csv', simulated_pure_guess)
csvwrite('sim_pure_intrusion.csv', simulated_pure_intrusion)
csvwrite('sim_semantic.csv', simulated_semantic)
csvwrite('sim_spatiotemporal.csv', simulated_spatiotemporal)
csvwrite('sim_temporal.csv', simulated_temporal)