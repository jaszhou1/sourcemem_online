function [ll, bic, Pred, pest, Gstuff, penalty, pest_penalty] = Fit_Mix(data, badix)
%    [ll,bic,Pred] = fitmixture3(Pvar, Pfix, Sel, Data)
%    P = [v1, v2, eta, a1, a2, pi, Ter, st, sa]
%          1    2    3  4   5   6   7   8   9 
setopt;

v1 = normrnd(6,0.1);
v2 = 0;
eta = 0;
a1 = normrnd(4,0.5);
a2 = normrnd(1.4,0.5);
pi = normrnd(0.8,0.2);
Ter = normrnd(-.05,0.02);
st = abs(normrnd(0.03,0.05));
sa = 0;

P = [v1, v2, eta, a1, a2, pi, Ter,st,sa];
Sel = [1,0,0,1,1,1,1,1,0];  
n = length(data);


pest = fminsearch(@fitmixture4x, P(Sel==1), options, P(Sel==0), Sel, data, n, badix);
P(Sel==1) = pest;
[ll,bic,Pred, Gstuff, penalty, pest_penalty] = fitmixture4x(P(Sel==1), P(Sel==0), Sel, data, n, badix); 
end
