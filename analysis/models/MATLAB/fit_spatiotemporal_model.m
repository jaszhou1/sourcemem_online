function [ll, aic, P, penalty] = fit_spatiotemporal_model(data, badix)
setopt;

% Default value for badix
if nargin < 2
    badix = 5;
end

v1_targ = 4.5;
v2_targ = 0;
v1_int = 0.05;
v2_int = 0;
eta_targ = 0.05;
eta_int = 0.05;
a_targ = normrnd(1.5, 0.1);
% a_int = normrnd(1.2, 0.1);
a_guess = normrnd(0.6, 0.05);
gamma = abs(normrnd(0.1, 0.01));
beta = abs(normrnd(0.7, 0.05));
kappa = normrnd(0.5, 0.05);
lambda_b = abs(normrnd(0.4, 0.1));
lambda_f = abs(normrnd(0.8, 0.1));
zeta = normrnd(0.01, 0.001);
rho = normrnd(0.9, 0.05);
Ter = normrnd(0.1, 0.05);
st = 0;
iota_t = 1;
iota_sp = 1;


P = [v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, Ter, st, iota_t, iota_sp];
Sel = [1,        0,     1,       0,       1,        1,        1,       1,      1,    1,     1,      1,        1,      1,    1,   1,   0, 1, 1];  

pest = fminsearch(@spatiotemporal_model, P(Sel==1), options, P(Sel==0), Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = spatiotemporal_model(P(Sel==1), P(Sel==0), Sel, data, badix);

end
