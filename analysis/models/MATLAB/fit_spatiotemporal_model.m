function [ll, aic, P, penalty] = fit_spatiotemporal_model(data, badix)
setopt;

% Default value for badix
if nargin < 2
    badix = 5;
end

v1_targ = normrnd(4, 0.1);
v2_targ = 0;
v1_int = abs(normrnd(0.1, 0.01));
v2_int = 0;
eta_targ = normrnd(0.2, 0.1);
eta_int = normrnd(0.01, 0.01);
a_targ = normrnd(1.5, 0.1);
a_guess = normrnd(0.5, 0.1);
gamma = normrnd(0.03, 0.01);
beta = normrnd(0.6, 0.1);
kappa = normrnd(0.35, 0.05);
lambda_b = normrnd(0.15, 0.1);
lambda_f = normrnd(0.9, 0.1);
zeta = normrnd(0.03, 0.01);
rho = normrnd(0.5, 0.1);
chi = 0; 
psi = 0; 
Ter = normrnd(0.2, 0.05);
st = 0;
iota = 0;

P = [v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess,...
    gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st];
Sel = [1,        0,     1,       0,       1,        1,        1,       1,...
    1,    1,     1,      1,        1,      1,    1,   0,  0,  1,   0];   

pest = fminsearch(@intrusion_gradient_model, P(Sel==1), options, P(Sel==0), Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = intrusion_gradient_model(P(Sel==1), P(Sel==0), Sel, data, badix);

end
