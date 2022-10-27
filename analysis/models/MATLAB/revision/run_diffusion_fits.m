%% Fit the circular diffusion model to source memory data
% This version of the model frees eta1 and eta2 as separate parameters
% Read in data
load('exp2_data.mat')

n_participants = length(data);
n_runs = 5;
num_workers = maxNumCompThreads/2; % Maximum number of workers


%% Spatiotemporal Model
% No orthographic/semantic contribution in model
spatiotemporal = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large. 
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data(i,:);
        [ll_new, aic, pest, pest_penalty] = fit_spatiotemporal_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_spatiotemporal_model(this_participant_data, pest);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            spatiotemporal(i,:) = this_fit;
        end
    end
end
save(filename)

%% Simulation of the fitted models.
 sim_spatiotemporal = [];
 for i = 1:10
     this_sim = simulate_intrusion_model(data(i,:), spatiotemporal{i,3}, i);
     sim_spatiotemporal = vertcat(sim_spatiotemporal, this_sim);
 end
 csvwrite('sim_spatiotemporal.csv', sim_spatiotemporal)
 
filename = [datestr(now,'yyyy_mm_dd_HH'),'_pest_spatiotemporal.csv'];
param_to_csv(filename, 1:n_participants, spatiotemporal, 'spatiotemporal', header_line);

 