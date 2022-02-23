function [ll, aic, P, penalty] = fit_temporal_gradient_model(data, badix)
setopt;

% Default value for badix
if nargin < 2
    badix = 5;
end

% v1_targ = P(1);
% v2_targ = P(2);
% v1_int = P(3);
% v2_int = P(4);
% eta_targ = P(5);
% eta_int = P(6);
% a_targ = P(7);
% a_int = P(7);
% a_guess = P(8);
% gamma = P(9);
% beta = P(10);
% kappa = P(11);
% lambda_b = P(12);
% lambda_f = P(13);
% ter = P(14);
% st = P(15);

v1_targ = normrnd(4, 0.5);
v2_targ = 0;
v1_int = normrnd(0.5, 0.1);
v2_int = 0;
eta_targ = normrnd(0.1, 0.1);
eta_int = normrnd(0.1, 0.1);
a_targ = normrnd(3, 0.2);
% a_int = normrnd(1.5, 0.4);
a_guess = normrnd(1, 0.2);
gamma = abs(normrnd(0.2, 0.1));
beta = abs(normrnd(0.7, 0.1));
kappa = normrnd(0.6, 0.1);
lambda_b = abs(normrnd(1, 0.2));
lambda_f = abs(normrnd(1, 0.2));
Ter = normrnd(0.2, 0.05);
st = 0;


P = [v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, Ter, st];
Sel = [1,        0,     1,       0,       1,        1,        1,       1,      1,    1,     1,      1,        1,      1,   0];  

pest = fminsearch(@temporal_gradient_flat_guess, P(Sel==1), options, P(Sel==0), Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = temporal_gradient_flat_guess(P(Sel==1), P(Sel==0), Sel, data, badix);

end
