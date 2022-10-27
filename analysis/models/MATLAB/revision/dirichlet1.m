function [Theta, Mt, Pt, T, Gt, Pmt, Mtscale] = dirichlet1(P, h, tmax, nbin, ntrials)
% =======================================================================
%Simulate ntrials of dirichlet problem of first exit of a Brownian motion 
% with drift from a circular region.
% [Theta, Mt, Pt, T, Gt, Pmt, Mtscale] = dirichlet1(P, h, tmax, nbin, ntrials)
% P = [mu1, mu2, sigma1, sigma2, a]
% Theta = is nbin radial measure
% Mt is the mean RT in bin i (1...nbin)
% Pt is the probability of terminating in bin i
% Gt is the conditional first passage time distribution in bin i
% Pmt is the Girsanov correction for bin i (the drift dependent terms)
% Mtscale is the time-dependent Girsanov correction to the bin i
% distribution x Pmt (constant time component x variable drift component)
%========================================================================
mu1 = P(1);
mu2 = P(2);
sigma1 = P(3);
sigma2 = P(4);
a = P(5);

w = 2*pi/nbin;
Theta = [-pi: w : pi - w];
Thetabound = [Theta + w/2];
sztheta = length(Theta);
Mt = zeros(1, sztheta);
Nt = zeros(1,sztheta);
Pmt = zeros(1, sztheta);
T = [0:h:tmax];
tbound = [T(1) - h/2, T + h/2];
szt = length(T);
Gt = zeros(sztheta, szt);
nonterminating = 0; 
for i = 1:ntrials
   [~,~,Ta,theta, nonterm] = dirisim(P, h, tmax);
   thetaindex = min(find(theta <= Thetabound));
   if isempty(thetaindex)  % between -pi and Thetabound(1), pool into last
      thetaindex = 1;
   end
   %theta
   %thetaindex
   %pause
   tindex = min(max(find(Ta > tbound)),szt);  % Pool into last bin.
   if ~nonterm
       Nt(thetaindex) = Nt(thetaindex) + 1;
       Mt(thetaindex) = Mt(thetaindex) + Ta;
   end;    
   Gt(thetaindex,tindex) = Gt(thetaindex, tindex) + 1;
end;
nonterminating = nonterminating + nonterm
Mt = Mt./(Nt + eps);
Pt = Nt / ntrials;
filter = cos(-pi/2:.025:pi/2); % was .1 
filter = filter / sum(filter);  % Normalize mass in filter
for thetaindex = 1:sztheta
    Gtfi = conv(Gt(thetaindex,:), filter);
    %Gt(i,:) = Gtfi(1:szt)./(Nt(i) * h +eps); % conditional
    Gt(thetaindex,:) = Gtfi(1:szt)./(ntrials  * h);  % joint density
end
for i = 1:sztheta
    Pmt(i) = exp(a * cos(Theta(i)) * mu1 / sigma1^2 + a * sin(Theta(i)) * mu2 / sigma2^2);
end;
Commonscale = exp(-0.5 * (mu1^2/sigma1^2 + mu2^2/sigma2^2) * T);
% Multiply theta-dependent drift term by invariant time-dependent term
Mtscale = Pmt' * Commonscale;
Pt = Pt / w; % To make into a density estimate.
end

function [T,Xt, Ta,theta,nonterm] = dirisim(P, h, tmax)
% ======================================================================
% Simulate dirichlet problem of first exit of a Brownian motion with
% drift from a circular region.
% [T,Xt, Ta,theta,nonterm] = dirichlet1(P, h)
% P = [mu1, mu2, sigma1, sigma2, a]
% ======================================================================
mu1 = P(1);
mu2 = P(2);
sigma1 = P(3);
sigma2 = P(4);
a = P(5);
a2 = a * a;
T = [0:h:tmax];
nmax = length(T);
Xt = zeros(2, nmax);
Sigma_Wt = [sigma1 * randn(1,nmax); ...
            sigma2 * randn(1,nmax)]; 
Mut = [mu1 * ones(1,nmax); ...
       mu2 * ones(1, nmax)];
Dt2 = 0;
i = 2;
a2 = a * a;
while(Dt2 < a2 && i <= nmax)
    Xt(:,i) =  Xt(:,i-1) + Mut(:,i) * h + Sigma_Wt(:,i) * sqrt(h);
    Dt2 = Xt(1, i)^2 + Xt(2,i)^2;
    i = i + 1;
end;
nonterm = i > nmax;
% ???
Ta = (i-1) * h;
%Ta = (i-3) * h;
theta = atan2(Xt(2, i-1), Xt(1,i-1));
       
end

