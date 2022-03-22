function simulate_qq(P, model)
% This is a function to play around with parameter values and see what it
% does to the qq plots.

% Read in some real data to get the pattern of non-targets and similarity
% values
load('exp2_data_cutoff.mat')

minrt = 0.15;  % Filter
maxrt = 3.5;

% Define quantile values
rtq = [.1, .3, .5, .7, .9];
errorq = [.1, .3, .5, .7, .9];

% Pool all participant data together, get data quantiles
data = cell2mat(data);
data_quantiles = get_qq(data, rtq, errorq);

% Simulate data, get simulated quantiles
sim_data = simulate_spatiotemporal(data, P);
sim_data = sim_data(sim_data(:,2) < maxrt,:);
sim_quantiles = get_qq(sim_data, rtq, errorq);

% Plotting constants
axis([0 2*pi -1.5 1.5])
symbol = ['o', 's', 'd', 'v', '^'];
symbol_lines = ["--or" "--sr" "--dr" "--vr" "--^r"];

% Plot the quantiles
for i = 1:length(rtq)
    this_data = data_quantiles(data_quantiles(:,4) == rtq(i), :);
    plot(this_data(:,1), this_data(:,2), symbol(i),...
        'MarkerSize',6,...
        'MarkerEdgeColor','black',...
        'MarkerFaceColor', 'black')
    hold on
end

for i = 1:length(rtq)
    this_data = sim_quantiles(sim_quantiles(:,4) == rtq(i), :);
    plot(this_data(:,1), this_data(:,2), symbol_lines(i),...
        'MarkerSize',6,...
        'MarkerEdgeColor','red')
    hold on
end
axis([0 pi 0 3])
end

function [qq] = get_qq(data, rtq, errorq)
qq = [];
error_quantiles = quantile(abs(data(:,1)), errorq);
% append a zero as the lower bound for the first bin
error_bins = [0, error_quantiles];

for i = 1:length(error_quantiles)
    % bin the data within each step of bin edges
    this_bin_data = data(data(:,1) > error_bins(i) & data(:,1) < error_bins(i+1),:);
    rt_quantiles = (quantile(this_bin_data(:,2), rtq))';
    this_bin_quantile = [repmat(error_quantiles(i),length(rtq),1), rt_quantiles,...
        repmat(errorq(i), length(rtq), 1), rtq'];
    qq = vertcat(qq, this_bin_quantile);
end
end

