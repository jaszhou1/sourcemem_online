function plot_simulation(Data, Pred)
% ========================================================================
% Plot fitted values of circular diffusion model with drift variability.
% ========================================================================

axhandle = setfig3;

Theta1 = Data(:,1);
Rt1 = Data(:,2);

% Accuracy
axes(axhandle(1))
histogram(Theta1, 50, 'Normalization', 'pdf', 'BinLimits', [-pi,pi]);
set(gca, 'Xlim', [-pi, pi])
set(gca, 'Ylim', [0, 2.5])
xlabel('Response Error (rad)')
ylabel('Probability density')
%subtitle('Accuracy');
hold
ksdensity(Pred(:,1))


% RT
axes(axhandle(2));
histogram(Rt1, 50, 'Normalization', 'pdf', 'BinLimits', [0,4.5]);
set(gca, 'Xlim', [0, 4.0])
xlabel('Response Time (s)')
ylabel('Probability density')
set(gca, 'Ylim', [0, 3.75])
%subtitle('Time');
hold
ksdensity(Pred(:,2))

% Joint QQ Plot
data_qq = get_qq(Data);
model_qq = get_qq(Pred);

% Define RT quantile symbols
%        0.1,  0.3,  0.5, 0.7, 0.9
data_symbol = ['ko'; 'ks'; 'kd'; 'kv'; 'k^'];
% Same but with lines for model
model_symbol = ['r--o'; 'r--s'; 'r--d'; 'r--v'; 'r--^'];

axes(axhandle(3));
% Draw each RT quantile. By default there are five of these, but this can
% be changed
for i = 1:5
    this_data_qq = data_qq(data_qq(:,4) == i, :);
    this_model_qq = model_qq(model_qq(:,4) == i, :);
    plot(this_data_qq(:,1), this_data_qq(:,2), data_symbol(i,:))
    hold on
    plot(this_model_qq(:,1), this_model_qq(:,2), model_symbol(i,:))
    hold on
end
set(gca, 'Ylim', [0, 3])
xlabel('Absolute Response Error (rad)')
ylabel('Response Time (s)')
end
