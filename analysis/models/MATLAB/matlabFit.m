%% matlabFit.m
% This is a script that fits the data for one participant from Experiment 1
% of Jason Zhou's PhD Project. The purpose of this script is to provide a
% simplified version of the MATLAB functions used in that experiment that
% can be ported into R for the second experiment.
%%

% load in some data
load('data.mat');
badix = 5;

% Set starting parameters
    
v1 = 1;                         % Drift norm, low imageability, x
v2 = 0;                         % Drift norm, low imageability, y
eta = 0.5;                      % Drift variability, low
a = 2;                          % Criterion (same for both low and high)
Ter = 0.1;                      % Non decision time
st = 0.05;                      % Non decision time variability
sa = 0;                         % Criterion variability

% Assemble parameter vector
P = [v1, v2, eta, a, Ter, st, sa];

% Define fixed and free parameters
Sel = [1,0,1,1,1,1,0]; 

nlow = length(data{1,1});
nhigh = length(data{1,2});

% Get model predictions and fit statistics
[ll,bic,Pred,Gstuff, penalty, pest_penalty] = ...
    fitdcircle4x(P(Sel==1), P(Sel==0), Sel, data, nlow, nhigh, badix);

% Plot model predictions against data
fitplot(data, Pred);
