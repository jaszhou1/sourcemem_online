function [] = param_to_csv(filename, participants, model, model_name, header_line)
%PARAM_TO_CSV Output a CSV file from .MAT structure that has the parameter
%estimates for the modelinuous, threshold and hybrid models in a way that is
%easy to compare across participant and models
  %% Open the file
  fp = fopen(filename, 'w');

%% Print the header line in the CSV.
%header_line = 'participant, model_name, BIC, v1, v2, eta, a1, a2, pi, Ter, st, sa';
fprintf(fp, [header_line '\n']);


%% Write parameter
for i = participants
    
    %% There appears to be a strange observer without any data in them, so
    %  let's skip over anyone who doesn't seem to have data.
    if isempty(model{i,1})
        modelinue
    end
    
    %% Get the appropriate model parameters
    %troubleshooting
    disp('Participant')
    disp(i);
    
    % Model
    this_pest = num2cell(model{i,3});
    this_line = {i, model_name, model{i,2},...
        this_pest{:}};
    this_line = cellfun(@num2str,this_line,'un',0);
    this_line = strjoin(this_line, ', ');
    fprintf(fp, [this_line '\n']);
end   
%% Close the file
  fclose(fp);
    
end


