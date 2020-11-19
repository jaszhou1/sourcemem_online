function fitplot(Data, Pred)
% ========================================================================
% Plot fitted values of circular diffusion model with drift variability.
%     fitplot(Data, Pred)
% ========================================================================
name = 'FITPLOT: ';
errmg1 = 'Data should be a 1 x 2 cell array from <makelike>...';

axhandle = setfig2;
Theta = Data(:,1);
Rt = Data(:,2);


t = Pred{1}(1,:);
gtm = Pred{1}(2,:);
theta = Pred{2}(1,:);
ptheta = Pred{2}(2,:);

%Accuracy Low Imageability
axes(axhandle(1))
histogram(Theta, 50, 'Normalization', 'pdf', 'BinLimits', [-pi,pi]);
set(gca, 'Xlim', [-pi, pi])
set(gca, 'Ylim', [0, 1.5])
xlabel('Response Error')
ylabel('Probability density')
% label(gca, .7, .9, 'Low');
title('Low')
hold
plot(theta, ptheta, 'Linewidth', 2);

% RT low
axes(axhandle(2));
histogram(Rt, 50, 'Normalization', 'pdf', 'BinLimits', [0,3.0]);
xlabel('Response Time (s)')
set(gca, 'Ylim', [0, 3.5])
% label(gca, .7, .9, 'Low');
title('Low')
hold
ylabel('Probability density')
plot(t, gtm, 'Linewidth', 2);

