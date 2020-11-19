function [] = abs_mat_to_csv(filename, model_string, input_cells)
%MAT_TO_CSV Output a CSV file from .MAT structure with diffusion runs
%   Generate a CSV file at the given output (as well as the table for
%   the CSV as the returned output of this function) from the .MAT
%   structure taken from the output of the diffusion runs.
%


  %% Parameters for the plots.
  time_quantiles = [0.1, 0.5, 0.9];
  n_theta_obs_quantiles = 4;
  time_step = 0.01;
  min_rt_filter = 0.0;
  max_rt_filter = 5.5;
  
  %% Open the file
  fp = fopen(filename, 'w');
  
  %% Extract the required quantities from the cell array.
 
  %make absolute
  data_cells = input_cells(:, 6);


for i = 1:size(input_cells,1)
    if isempty(data_cells{i, :})
        continue
    end
    %Combine Conditions
    data_cells{i,:} = vertcat(data_cells{i,1}{1,2});
    
    %Make Absolute
    data_cells{i,1}(:,1) = abs(data_cells{i,1}(:,1));
end

  model_cells = input_cells(:, 5);
  
  %% Print the header line in the CSV.
  header_line = 'model_name, participant, is_model, quantile_idx, rt, theta';
  fprintf(fp, [header_line '\n']);
  
  %% For each observer in the cell array...
  n_observers = min(size(data_cells, 1), ...
                    size(model_cells, 1));

  for i = 1:n_observers
    %% There appears to be a strange observer without any data in them, so
    %  let's skip over anyone who doesn't seem to have data.
    if isempty(data_cells{i, :}) || isempty(model_cells{i, :})
       continue 
    end
      
    %% Get the appropriate data and models.
    
    %troubleshooting
    disp('Participant')
    disp(i);
    %
    
    this_data_cell = data_cells{i, :};
   this_model_cell = model_cells{i, :};

    time_vector = this_model_cell{1, 1};
    theta_vector = this_model_cell{2, 1};
    joint = this_model_cell{3, 1};

    
    %% Output the Model predictions (the quantile lines).
    [pred_qlines, pred_theta] = ...
        joint_dist_to_qlines(time_vector, theta_vector, ...
                             joint, time_quantiles, time_step);
    for j = 1:size(time_quantiles, 2)
      for k = 1:size(pred_theta, 2)
        this_quantile = time_quantiles(j);
        this_qline_point = pred_qlines(k, j);
        this_line = {model_string, num2str(i), 'true', ...
                     num2str(this_quantile), num2str(this_qline_point), ...
                     num2str(pred_theta(k))};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
      end
    end
    
    
%     % Output the observed data QxQ points.
    [qxq, qxq_thetas] = data_to_qxq(this_data_cell, ...
                                              n_theta_obs_quantiles, ...
                                              time_quantiles, ...
                                              min_rt_filter, ...
                                              max_rt_filter);
    for j = 1:size(time_quantiles, 2)
      for k = 1:size(qxq_thetas, 2)
        this_quantile = time_quantiles(j);
        this_qxq_rt = qxq(j, k);
        this_qxq_theta = qxq_thetas(k);
        this_line = {model_string, num2str(i), 'false', ...
                     num2str(this_quantile), num2str(this_qxq_rt), ...
                     num2str(this_qxq_theta)};
        this_line = strjoin(this_line, ', ');
        fprintf(fp, [this_line '\n']);
      end
    end

  end
  
  %% Close the file
  fclose(fp);
end
