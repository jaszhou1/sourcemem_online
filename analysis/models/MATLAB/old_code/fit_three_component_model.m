function [ll, aic, P, penalty] = fit_three_component_model(data, badix)
%    [ll,aic,Pred] = fitmixture3(Pvar, Pfix, Sel, Data)
%    P = [v1, v2, eta, a1, a2, pi, Ter, st, sa]
%          1    2    3  4   5   6   7   8   9 
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
% a_int = P(8);
% a_guess = P(9);
% gamma = P(10);
% beta = P(11);
% ter = P(12);
% st = P(13);

v1_targ = normrnd(4, 2);
v2_targ = 0;
v1_int = normrnd(0.5, 0.1);
v2_int = 0;
eta_targ = 0;
eta_int = 0;
a_targ = normrnd(1.5, 0.4);
% a_int = normrnd(1.5, 0.4);
a_guess = normrnd(0.5, 0.1);
gamma = abs(normrnd(0.2, 0.3));
beta = abs(normrnd(0.7, 0.05));
Ter = normrnd(0.2, 0.05);
st = 0;


P = [v1_targ, v2_targ, v1_int, v2_int, eta_targ, eta_int,  a_targ, a_guess, gamma, beta, Ter, st];
Sel = [1,        0,     1,       0,       0,        0,        1,       1,      1,    1,   1,   0];  

pest = fminsearch(@three_component_model, P(Sel==1), options, P(Sel==0), Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = three_component_model(P(Sel==1), P(Sel==0), Sel, data, badix);

end
