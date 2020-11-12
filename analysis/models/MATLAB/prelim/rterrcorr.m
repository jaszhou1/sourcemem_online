function [Corri, Vi, Ri] = rterrcorr(Data);
% ================================================================
% Correlation between RT and absolute value of error 
%   [Corri, Vi, Ri] = rterrcorr(Data);
%   corri is correlation between RT and error
%   Vi is circular variance
%   Ri is test statistic for Rayleigh test of uniformity
%   and associated p value (chisquare(2).
%   Uses NCSS defintions of circular statistics
% ================================================================
nsub = 19; % Seems to be...!
rtx = 4;
errx = 6;
Subject = [1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20];
sz = size(Data);
ntrials = sz(1) / nsub;
Corri = zeros(nsub, 2);
Vi = zeros(nsub, 2);
Ri = zeros(nsub, 3);


for i = 1:nsub
   Ix = (i - 1) * ntrials + [1:ntrials];
   RT = Data(Ix, rtx);
   ThetaErr = Data(Ix, errx);
   Cp = sum(cos(ThetaErr));
   Sp = sum(sin(ThetaErr));
   Rp = sqrt(Cp^2 + Sp^2);
   Rpbar = Rp / ntrials;
   Rayleigh = (2 * ntrials - 1) * Rpbar^2 + ntrials * Rpbar^4 / 2; 
   Ri(i,1) = Subject(i);
   Vi(i,1) = Subject(i);
   Corri(i,1) = Subject(i); 
   Ri(i, 2) = Rayleigh;
   Ri(i, 3) = chisqpr(Rayleigh, 2);   
   V = 1 - Rpbar; % Circular variance.
   Sd = sqrt(-2 * log(Rpbar)); % Circular standard deviation.
   Vi(i,2) = V;
   ci = corrcoef(RT, abs(ThetaErr));
   Corri(i,2) = ci(1,2);
end

function q = chisqpr(chisq, df)
% ================================================================================
% Returns chi-square tail probability for specified d.f.
%    q = chisqpr(chisq, df);
% ================================================================================
q = zeros(size(chisq));
for i = 1:length(chisq)
    q(i) = 1 - gammainc(chisq(i)/2, df(i)/2);
end
