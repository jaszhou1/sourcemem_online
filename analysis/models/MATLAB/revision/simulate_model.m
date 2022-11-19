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