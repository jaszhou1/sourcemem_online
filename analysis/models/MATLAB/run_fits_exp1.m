%% RUN_FITS_EXP1.M
% This is the top-level script to fit diffusion models to Experiment 1 data
% and save simulated data from estimated parameters for plotting in R.

% Read in the data
% data = read_sourcemem_data();
load('sourcemem_data.mat')
n_participants = length(data);
n_runs = 3;
num_workers = maxNumCompThreads/2 - 1; % Maximum number of workers

% %% Threshold Model
% 
% threshold = cell(n_participants,4);
% 
% parfor (i = 1:n_participants, num_workers)
%     % Initial log likelihood value
%     ll = 1e7;
%     % Run each participant nrun times
%     this_fit = cell(1,4);
%     for j = 1:n_runs
%         this_participant_data = data{i};
%         [ll_new, aic, pest, pest_penalty] = fit_threshold_model(this_participant_data);
%         % If this ll is better than the last one, replace it in the saved
%         % structure
%         if (ll_new < ll)
%             this_fit{1} = ll_new;
%             this_fit{2} = aic;
%             this_fit{3} = pest;
%             this_fit{4} = pest_penalty;
%             threshold(i,:) = this_fit;
%         end
%     end
% end
% 
% % Simulate data, concatenate participants, and save simulated dataset
% simulated_threshold = [];
% for i = 1:n_participants
%     this_simulated_data = simulate_threshold(data{i}, threshold{i,3});
%     % Label this dataset with participant number
%     this_simulated_data(:,3) = i; 
%     simulated_threshold = vertcat(simulated_threshold, this_simulated_data);
% end
% 
% filename = [datestr(now,'yyyy_mm_dd_HH'),'_Experiment1_diffusion_temp'];
% save(filename)
% %% Pure Intrusion Model
% 
% pure_intrusion = cell(n_participants,4);
% 
% parfor (i = 1:n_participants, num_workers)
%     % Initial log likelihood value
%     ll = 1e7;
%     % Run each participant nrun times
%     this_fit = cell(1,4);
%     for j = 1:n_runs
%         this_participant_data = data{i};
%         [ll_new, aic, pest, pest_penalty] = fit_pure_intrusion_model(this_participant_data);
%         % If this ll is better than the last one, replace it in the saved
%         % structure
%         if (ll_new < ll)
%             ll = ll_new;
%             this_fit{1} = ll_new;
%             this_fit{2} = aic;
%             this_fit{3} = pest;
%             this_fit{4} = pest_penalty;
%             pure_intrusion(i,:) = this_fit;
%         end
%     end
% end
% 
% % Simulate data, concatenate participants, and save simulated dataset
% simulated_pure_intrusion = [];
% for i = 1:n_participants
%     this_simulated_data = simulate_pure_intrusion(data{i}, pure_intrusion{i,3});
%     % Label this dataset with participant number
%     this_simulated_data(:,3) = i; 
%     simulated_pure_intrusion = vertcat(simulated_pure_intrusion, this_simulated_data);
% end
% 
% save(filename)
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
% %% Same, but with eta
% flat_intrusion_eta = cell(n_participants,4);
% 
% parfor (i = 1:n_participants, num_workers)
%     % Initial log likelihood value
%     ll = 1e7;
%     % Run each participant nrun times
%     this_fit = cell(1,4);
%     for j = 1:n_runs
%         this_participant_data = data{i};
%         [ll_new, aic, pest, pest_penalty] = fit_three_component_model_eta(this_participant_data);
%         % If this ll is better than the last one, replace it in the saved
%         % structure
%         if (ll_new < ll)
%             ll = ll_new;
%             this_fit{1} = ll_new;
%             this_fit{2} = aic;
%             this_fit{3} = pest;
%             this_fit{4} = pest_penalty;
%             flat_intrusion_eta(i,:) = this_fit;
%         end
%     end
% end
% 
% % Simulate data, concatenate participants, and save simulated dataset
% simulated_flat_intrusion_eta = [];
% for i = 1:n_participants
%     this_simulated_data = simulate_three_component(data{i}, flat_intrusion_eta{i,3});
%     % Label this dataset with participant number
%     this_simulated_data(:,3) = i; 
%     simulated_flat_intrusion_eta = vertcat(simulated_flat_intrusion_eta, this_simulated_data);
% end
% save(filename)
% %% Temporal Gradient Model
% temporal = cell(n_participants,4);
% 
% parfor (i = 1:n_participants, num_workers)
%     % Initial log likelihood value
%     ll = 1e7;
%     % Run each participant nrun times
%     this_fit = cell(1,4);
%     for j = 1:n_runs
%         this_participant_data = data{i};
%         [ll_new, aic, pest, pest_penalty] = fit_temporal_gradient_model(this_participant_data);
%         % If this ll is better than the last one, replace it in the saved
%         % structure
%         if (ll_new < ll)
%             ll = ll_new;
%             this_fit{1} = ll_new;
%             this_fit{2} = aic;
%             this_fit{3} = pest;
%             this_fit{4} = pest_penalty;
%             temporal(i,:) = this_fit;
%         end
%     end
% end
% 
% % Simulate data, concatenate participants, and save simulated dataset
% simulated_temporal_flat_guess = [];
% for i = 1:n_participants
%     this_simulated_data = simulate_temporal_flat_guess(data{i}, temporal{i,3});
%     % Label this dataset with participant number
%     this_simulated_data(:,3) = i; 
%     simulated_temporal_flat_guess = vertcat(simulated_temporal_flat_guess, this_simulated_data);
% end
% save(filename)
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
    this_simulated_data = simulate_spatiotemporal(data{i}, spatiotemporal{i,3});
    % Label this dataset with participant number
    this_simulated_data(:,3) = i; 
    simulated_spatiotemporal = vertcat(simulated_spatiotemporal, this_simulated_data);
end

% Save workspace
filename = [datestr(now,'yyyy_mm_dd_HH'),'_Experiment1_diffusion'];
save(filename)