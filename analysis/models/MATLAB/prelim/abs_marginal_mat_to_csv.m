function [] = abs_marginal_mat_to_csv(filename, model_string, input_cells)
%ABS_MAT_TO_CSV Output a CSV file from .MAT structure with diffusion runs
%   Generate a CSV file at the given output which contains the model
%   probability density estimates for response angles and the kernel density estimate of the
%   data to be plotted

%   Data and model predictions to be expressed in terms of absolute theta

%% Open the file
fp = fopen(filename, 'w');

%% Extract the required quantities from the cell array.
data_cells = input_cells(:, 6);


for i = 1:size(input_cells,1)
    if isempty(data_cells{i, :})
        continue
    end
    %Combine Conditions
    data_cells{i,:} = data_cells{i,1};
    
%     %Make Absolute
%     data_cells{i,1}(:,1) = abs(data_cells{i,1}(:,1));
end

model_cells = input_cells(:, 3);

header_line = 'model_name, participant, is_model, is_theta, value, prob';
fprintf(fp, [header_line '\n']);

%% For each observer in the cell array...
n_observers = min(size(data_cells, 1), ...
    size(model_cells, 1));

for i = 1:n_observers
    %  There appears to be a strange observer without any data in them, so
    %  let's skip over anyone who doesn't seem to have data.
    if isempty(data_cells{i, :})
        continue
    end
    
    %Get Phase angle densities
    data = data_cells{i,1};
    [ptheta, theta] = abs_kernel_theta(data);
    data_prob = [theta;ptheta];
    model_prob = model_cells{i,1}{1,2};
    
    %Data, angle
    for k = 1:size(data_prob,2)
        this_line = {model_string, num2str(i), 'false', 'true' ...
            num2str(data_prob(1,k)), num2str(data_prob(2,k))};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
    end
    
    %Model, angle
    for k = 1:size(model_prob,2)
        this_line = {model_string, num2str(i), 'true', 'true' ...
            num2str(model_prob(1,k)), num2str(model_prob(2,k))};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
    end
    
    
    
    %Data, RT
    
    %Get RT densities
    data = data_cells{i,1};
    [prt, rt] = abs_kernel_rt(data);
    data_rt_prob = [rt;prt];
    model_rt_prob = model_cells{i,1}{1,1};
    
    for k = 1:size(data_rt_prob,2)
        this_line = {model_string, num2str(i), 'false', 'false' ...
            num2str(data_rt_prob(1,k)), num2str(data_rt_prob(2,k))};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
    end
    
    %Model, RT
    for k = 1:size(model_rt_prob,2)
        this_line = {model_string, num2str(i), 'true', 'false' ...
            num2str(model_rt_prob(1,k)), num2str(model_rt_prob(2,k))};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
    end
    
end

%% Close the file
fclose(fp);

end
