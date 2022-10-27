function [] = pest_to_csv(fits, filename, header_line)
%% Open the file
fp = fopen(filename, 'w');

fprintf(fp, [header_line '\n']);

% print rows
for i = 1:length(fits)
    this_LL = fits{i, 1};
    this_aic = fits{i, 2};
    this_pest = fits{i, 3};
    this_line = {num2str(i), num2str(this_LL), num2str(this_aic), strjoin(cellstr(num2str(this_pest')),',')};
    this_line = strjoin(this_line, ', ');
    fprintf(fp, [this_line '\n']);
end
%% Close the file
fclose(fp);
end