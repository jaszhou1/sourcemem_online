function [T, Gt] = dhamana(P, kmax, h, tmax, badix)
% ====================================================================
% First passage time density for a bessel process, 
% Derivative of Hamana-Matsumoto solution, for x0 = 0 (Eq. 2.7)
% [t, gt] = dhamana(P, kmax, h, tmax, badix);
% P =[a, sigma];
% b is boundary, a is starting point.
% kmax controls truncation of series
% badix is number of bad initial values in Gt to zero.
% =====================================================================
if nargin < 5
    badix = 0;
end    
a = P(1);
sigma = P(2);
sigma2 = sigma^2;
T = [h:h:tmax];
a2 = a^2;
J0k = besselzero(0, kmax, 1); % v = 0 for drift = (2v + 1)/2x 
J0k_squared = J0k.^2;
J1k = besselj(1, J0k);
Rt = zeros(size(T));
scaler = sigma2 / a2;
for k = 1:kmax
    Rt = Rt + J0k(k) * exp(-J0k_squared(k) * sigma^2 * T /(2 * a2)) ...
         / J1k(k);
end;
Gt = scaler * Rt;
T = [0,T];
Gt = [0,Gt];
if badix > 0 
   Gt(1:badix) = 0;
end
Gt = max(Gt, 0);   
end
    
