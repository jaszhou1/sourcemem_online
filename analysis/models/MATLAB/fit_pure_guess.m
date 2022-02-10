function [ll, aic, P, penalty] = fit_pure_guess(data)
%    [ll,bic,Pred] = fitmixture3(Pvar, Pfix, Sel, Data)
%    P = [v1, v2, eta, a1, a2, pi, Ter, st, sa]
%          1    2    3  4   5   6   7   8   9 
setopt;

v1 = normrnd(6,0.1);
v2 = 0;
eta = normrnd(0.1, 0.1);
a1 = normrnd(4,0.5);
a2 = normrnd(1.4,0.5);
beta = normrnd(0.6,0.1);
Ter = normrnd(-.05,0.02);
st = 0;

P = [v1, v2, eta, a1, a2, beta, Ter,st];
Sel = [1,0,1,1,1,1,1,0];  


pest = fminsearch(@pure_guess_model, P(Sel==1), options, P(Sel==0), Sel, data);
P(Sel==1) = pest;
[ll,aic,P, penalty] = pure_guess_model(P(Sel==1), P(Sel==0), Sel, data); 
end
