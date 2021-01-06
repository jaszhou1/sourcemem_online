function [ll,bic,Pred, Gstuff, penalty, pest_penalty] = fitmixture4x(Pvar, Pfix, Sel, Data, n, badix, trace)
% ========================================================================
% Circular diffusion with drift variability for Jason's source memory task.
% Across-trial variability in criterion.
% Mixture of memory based and guessing based process with different
% criteria. Assumes the eta components in the x and y directions are the
% same.
%    [ll,bic,Pred] = fitmixture4x(Pvar, Pfix, Sel, Data)
%    P = [v1a, v2a, v1b, v2b, eta1, eta2, a1, a2, pi1, pi2, Ter  st sa]
%          1    2    3    4    5      6    7   8   9   10    11  12 13
%    a1, a2 are criteria for memory and guessing process, pi1, pi2 are
%    mixing proportions for long and short.
%    'Data' is cell array structure created by <makelike>
% ========================================================================
name = 'FITMIXTURE4X: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Data should be a 1 x 2 cell array from <makelike>...';

tmax = 5.1;
np = 9;
nt = 300;
h = tmax / nt;

n_sz_step =  11; % Criterion variability.
epsx = 1e-9;
epsilon = 0.0001;
cden = 0.05;  % Contaminant density.

nw = 50;
w = 2 * pi / nw;
%h = tmax / 300;

% Set manually if needed - 30/1/19
%This affects the leading edge of the model RT predictions. If criterion is
%high, then badix should increase to avoid big discontinuity at quick RT
%badix = 5; %default is 5


if nargin < 7
    trace = 0;
end;
lp = length(Pvar) + length(Pfix);
if lp ~= np
    [name, errmg1], length(Pvar), length(Pfix), return;
end
if length(Sel) ~= np
    [name, errmg2], length(Sel), return;
end
if size(Data) ~= [1,2]
    [name, errmg3], size(Data), return;
end

% Assemble parameter vector.
P = zeros(1,np);
P(Sel==1) = Pvar;
P(Sel==0) = Pfix;

% Save on each iteration - 30/1/19
Ptemp = P;
save Ptemp Ptemp

v1 = P(1);
v2 = P(2);
eta = P(3);
a1 = P(4);
a2 = P(5);
pmem = P(6);
ter = P(7);
st = P(8);
sa = P(9);


% Components of drift variability.
eta1 = eta;
eta2 = eta;
sigma = 1.0;

% Putting this here so that pest_penalty has a value assigned to it when
% boundaries return early
pest_penalty(1,:) = P;

% Cleaned up penalty calculation, hard and soft bounds - 30/1/19
penalty = 0;
% ---------------------------------------------------------------------------
%   v1, v2, eta,   a1, a2, pi,    Ter  st, sa]
% ---------------------------------------------------- ----------------------

Ub= [ 7, 7,  1,  6, 6,  1,   1.0, 0.8, 0];
Lb= [0, 0, 0,  0.3*ones(1,2), 0 , -0.5, 0, 0];

Pub=[ 6, 6, 0.7, 5.5, 5.5, 1,  0.8, 0.7, 0];

Plb=[0, 0, 0,  0.4*ones(1,2), 0.01, -0.4, 0, 0];

Pred = cell(1,2);
if any(P - Ub > 0) | any(Lb - P > 0)
    ll = 1e7 + ...
        1e3 * (sum(max(P - Ub, 0).^2) + sum(max(Lb - P, 0).^2));
    bic = 0;
    Gstuff = cell(3,2);
    if trace
        max(P - Ub, 0)
        max(Lb - P, 0)
    end
    return
else
    penalty =  1e3 * (sum(max(P - Pub, 0).^2) + sum(max(Plb - P, 0).^2));
    if trace
        max(P - Pub, 0)
        max(Plb - P, 0)
        penalty
    end
end

% Saving the full vector of parameters and an indication of whether or not
% a penalty was applied.
pest_penalty(1,:) = P;
pest_penalty(2,:) = max(P - Pub, 0).^2 + max(Plb - P, 0).^2;

% Parameters for memory process
P_mem = [v1, v2, eta1, eta2, sigma, a1];

if sa < epsilon % No criterion variability
    % Memory-based process
    [t, gt, theta, ptheta, mt] = vdcircle300cls(P_mem, tmax, badix);
%     [t, gt, theta, ptheta, mt] = vdcircle3cls(P, nw, h, tmax, badix);
    
else  % Criterion variability
    % Rectangular mass for starting point variability.
    U = ones(n_sz_step, 1);
    Rmass = U / n_sz_step ;
    Rstep = [-(n_sz_step-1)/2:(n_sz_step-1)/2]' / (n_sz_step-1);
    A = a1 + Rstep * sa;
    gt = zeros(nw+1, nt+1);
    ptheta = zeros(1, nw+1);
    mt = zeros(1, nw+1);
    for i = 1:n_sz_step
        Pi = [v1, v2, eta1, eta2, sigma, A(i)];
        [t, gti, theta, pthetai, mti] = vdcircle300cls(Pi, tmax, badix);
        gt = gt + gti;
        ptheta = ptheta + pthetai;
        mt = mt + pthetai .* mti;
    end
    gt = gt / n_sz_step;
    mt = mt ./ ptheta;
    ptheta = ptheta / n_sz_step;
end;
% Parameters for guessing process - zero drift, different criterion
P_guess = [0, 0, 0, 0, sigma, a2];
[tc, gtc, thetac, pthetac, mtc] = vdcircle300cls(P_guess, tmax, badix);
%[tc, gtc, thetac, pthetac, mtc] = vdcircle3cls(P_guess, nw, h, tmax, badix);

% Mixture of memory-based processes and guesses
gt =  pmem * gt + (1 - pmem) * gtc;
ptheta =  pmem * ptheta + (1 - pmem) * pthetac;

% Filter zeros
gt = max(gt, epsx);

% Add nondecision times
t = t + ter;

% --------------------
% Convolve with Ter(Added from fitgvm, 07/05)
% --------------------
h = t(2) - t(1);
if st > 2 * h
    m = round(st/h);
    n = length(t);
    fe = ones(1, m) / m;
    for i = 1:nw + 1
        gti = conv(gt(i, :), fe);
        gt(i,:) = gti(1:n);
    end
end

% Create mesh for interpolation
[angle,time]=meshgrid(t, theta);

% Interpolate in joint density to get likelihoods of each data point.
% 1 is long, 2 is short
l0a = interp2(angle, time, gt,  Data(:,2),Data(:,1), 'linear');

% Out of range values returned as NaN's. Treat as contaminants - set small.
ixa = isnan(l0a);
l0a(ixa) = cden;
% Log-likelihoods.
ll0a = log(l0a);



% Pass out joint density for use in quantile plot
Gstuff = cell(3,1);
Gstuff{1,1} = t;
Gstuff{2,1} = theta;
Gstuff{3,1} = gt;

% Minimize sum of minus LL's across two conditions. Added penalty term 30/1/19
ll = sum(-ll0a) + penalty;

bic = 2 * ll + sum(Sel) * log(n);
if trace > 1
    Val = [Data, l0a, ixa]
end

% Predictions for plot
gtm = sum(gt) * w;
Pgta = [t;gtm];
Ptha = [theta;ptheta];
Pred{1} = Pgta;
Pred{2} = Ptha;


