%% Fit the circular diffusion model to source memory data
% This version of the model frees eta1 and eta2 as separate parameters
% Read in data
load('exp2_data.mat')

n_participants = length(data);
n_runs = 5;
num_workers = maxNumCompThreads/2; % Maximum number of workers


filename = 'exp2_fits.mat';

%% Pure Guess Model
% No intrusions,
pure_guess = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_pure_guess_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_pure_guess_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            pure_guess(i,:) = this_fit;
        end
    end
end

%% Pure Intrusion Model
% Two component mixture model, memory + intrusions
% No guesses, target variability and prob of intrusion is the only
% mechanism for errors in this model
pure_int = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_pure_intrusion_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_pure_intrusion_model(this_participant_data, pest);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            pure_int(i,:) = this_fit;
        end
    end
end

%% Flat Intrusion Model
% Base three component mixture model, memory + guess + intrusions
% All intrusions are equally probable, no similarity gradient.
flat_int = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_flat_intrusion_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
%             [ll_new, aic, pest, pest_penalty] = fit_flat_intrusion_model(this_participant_data, pest);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            flat_int(i,:) = this_fit;
        end
    end
end

%% Temporal Model
% Temporal gradient on intrusion probabilities
temporal = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_temporal_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_temporal_model(this_participant_data, pest);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            temporal(i,:) = this_fit;
        end
    end
end

%% Spatiotemporal Model
% No orthographic/semantic contribution in model
spatiotemporal = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_spatiotemporal_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_spatiotemporal_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            spatiotemporal(i,:) = this_fit;
        end
    end
end

%% Orthographic Model
% Spatiotemporal * Orthographic
ortho = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_ortho_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_ortho_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            ortho(i,:) = this_fit;
        end
    end
end

%% Semantic Model
% Spatiotemporal * Semantic
sem = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_sem_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_sem_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            sem(i,:) = this_fit;
        end
    end
end

%% Four Factor Model
% Structure of intrusion weights is (time*space)*(orth*sem)
four_factor = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_4F_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_4F_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            four_factor(i,:) = this_fit;
        end
    end
end

%% Pure Orthosem Model
% Just sanity check, try the model with no spatiotemporal component, but
% with orthographic and semantic gradient. This model should miss the
% important temporal and spatial recentering effects.
pure_orthosem = cell(n_participants,4);

parfor (i = 1:n_participants, num_workers)
    % Initial log likelihood value, very large.
    ll = 1e7;
    % Run each participant nrun times
    this_fit = cell(1,4);
    for j = 1:n_runs
        this_participant_data = data{i};
        [ll_new, aic, pest, pest_penalty] = fit_pure_orthosem_model(this_participant_data);
        % If this ll is better than the last one, replace it in the saved
        % structure
        if (ll_new < ll)
            % Double fit, call fminsearch again, but use the best parameter
            % estimates as starting points
            [ll_new, aic, pest, pest_penalty] = fit_pure_orthosem_model(this_participant_data, pest, 5);
            ll = ll_new;
            this_fit{1} = ll;
            this_fit{2} = aic;
            this_fit{3} = pest;
            this_fit{4} = pest_penalty;
            pure_orthosem(i,:) = this_fit;
        end
    end
end
save(filename)


%% Simulation of the fitted models.
% Headerline for pest .csv, because all models are fit with the same base
% code, we can use the same headerline since missing parameters will take
% on the appropriate value (usually 0) instead of missing a column. More
% convenient to have this consistency 
header_line = 'participant, model_name, ll, AIC, v1t_1, v2t_1,  v1i_1,  v2i_1,       eta1_t, eta2_t, eta1_i, eta2_i,     at,  ag,    beta,    gamma,      tau,  l_b,   l_f,       zeta,  rho,     chi,        psi1,       iota,  upsilon,       Ter,    st';
sim_pure_guess = simulate_model(pure_guess, 'Pure Guess', header_line, data);
sim_pure_intrusion = simulate_model(pure_intrusion, 'Pure Intrusion', header_line, data);
sim_flat_intrusion = simulate_model(flat_intrusion, 'Flat Intrusion', header_line, data);
sim_temporal = simulate_model(temporal, 'Temporal', header_line, data);
sim_spatiotemporal = simulate_model(spatiotemporal, 'Spatiotemporal', header_line, data);
sim_ortho = simulate_model(ortho, 'Spatiotemporal-Orthographic', header_line, data);
sim_sem = simulate_model(sem, 'Spatiotemporal-Semantic', header_line, data);
sim_four_factor = simulate_model(four_factor, 'Four Factor', header_line, data);
sim_pure_orthosem = simulate_model(pure_orthosem, 'Orthographic-Semantic', header_line, data);

%% Simulation function
% Function that takes in a fitted model structure, and generates some
% output, a .csv with simulated observations for plotting in R, and also
% the parameter estimates and AIC fit stats, in a separate .csv file
function [sim_data] = simulate_model(model, model_name, header_line, data)
    n_participants = length(data);
    sim_data = [];
    for i = 1:n_participants
        this_sim = simulate_intrusion_model(data(i,:), model{i,3}, i);
        sim_data = vertcat(sim_data, this_sim);
    end
    csvwrite(strcat('exp2_sim_', model_name, '.csv'), sim_data)

    filename = ['exp2_',datestr(now,'yyyy_mm_dd_HH'),'_pest_', model_name, '.csv'];
    param_to_csv(filename, 1:n_participants, model, model_name, header_line);
end