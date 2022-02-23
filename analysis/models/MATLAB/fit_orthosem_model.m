function [ll, aic, P, penalty] = fit_orthosem_model(data, badix)
setopt;

% Default value for badix
if nargin < 2
    badix = 5;
end

v1_targ = normrnd(4, 0.1);
v2_targ = 0;
v1_int = abs(normrnd(0.1, 0.05));
v2_int = 0;
eta_targ = normrnd(0.2, 0.1);
eta_int = normrnd(0.01, 0.01);
a_targ = normrnd(1.7, 0.1);
a_guess = normrnd(0.5, 0.1);
gamma = abs(normrnd(0.1, 0.1));
beta = abs(normrnd(0.6, 0.1));
kappa = normrnd(0.6, 0.1);
lambda_b = abs(normrnd(1, 0.2));
lambda_f = abs(normrnd(1, 0.2));
zeta = normrnd(0.2, 0.1);
rho = normrnd(0.5, 0.1);
chi = norm(0.5, 0.1); 
psi = norm(0.1, 0.1); 
Ter = normrnd(0.2, 0.05);
st = 0;


P = [v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess,...
    gamma, beta, kappa, lambda_b, lambda_f, zeta, rho, chi, psi, Ter, st];
Sel = [1,        0,     1,       0,       1,        1,        1,       1,...
    1,    1,     1,      1,        1,      1,    1,   1,  1,  1,   0];  

pest = fminsearch(@orthosem_multi_model, P(Sel==1), options, P(Sel==0), Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = orthosem_multi_model(P(Sel==1), P(Sel==0), Sel, data, badix);

end
