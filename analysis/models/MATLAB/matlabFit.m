%% matlabFit.m
% This is a script that fits the data for one participant from Experiment 1
% of Jason Zhou's PhD Project. The purpose of this script is to provide a
% simplified version of the MATLAB functions used in that experiment that
% can be ported into R for the second experiment.
%%

% load in some data
load('p2data_split.mat');
data = p2data_split;
badix = 5;

% Set starting parameters
    
v1 = normrnd(1,0.25);           % Drift norm, low imageability, x
v2 = 0;                         % Drift norm, low imageability, y
eta = normrnd(0.5,0.25);        % Drift variability, low
a = normrnd(1.6,0.4);           % Criterion (same for both low and high)
Ter = normrnd(0,0.05);          % Non decision time
st = abs(normrnd(0.05,0.01));   % Non decision time variability
sa = 0;                         % Criterion variability

% Assemble parameter vector
P = [v1, v2, eta, a, Ter, st, sa];

% Define fixed and free parameters
Sel = [1,0,1,1,1,1,0]; 

nlow = length(data{1,1});
nhigh = length(data{1,2});

[ll,bic,Pred,Gstuff, penalty, pest_penalty] = ...
    fitdcircle4x(P(Sel==1), P(Sel==0), Sel, data, nlow, nhigh, badix);
