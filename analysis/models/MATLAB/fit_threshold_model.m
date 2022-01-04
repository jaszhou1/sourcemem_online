function [ll, aic, pest, penalty] = fit_threshold_model(data)
% Jason Zhou <jasonz1 AT student DOT unimelb DOT edu DOT au>

setopt;
%Starting parameters for P
%    [ll, bic, pest_penalty] = fitdcircle(Pvar, Pfix, Sel, Data)
%    P = [v1, v2, eta, a, gamma, Ter, st]
%         1    2    3   4   5    6     7    

v1 = normrnd(0.3,0.5); % Drift norm x
v2 = 0; % Drift norm y
eta = normrnd(0.1, 0.2);   % Drift variability
a1 = normrnd(1.6,0.4); 
a2 = normrnd(1.6,0.4); 
beta = normrnd(0.4, 0.1); % Proportion of guesses
Ter = normrnd(0.15, 0.05);     % Non decision time
st = abs(normrnd(0.01, 0.01));   % Non decision time variability


P = [v1, v2, eta, a1, a2, beta, Ter, st];
Sel = [1,0,1,1,1,1,1,1]; 
% 
pest = fminsearch(@threshold_model, P(Sel==1), options, P(Sel==0), Sel, data);

P(Sel== 1) = pest; 

[ll, aic, pest, penalty] = threshold_model(P(Sel==1), P(Sel==0), Sel, data);

end
