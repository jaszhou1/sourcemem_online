function [ll,bic,Pred, Gstuff, penalty, pest_penalty] = fitdcircle4x(Pvar, Pfix, Sel, Data, n, badix, trace)
% ========================================================================
% Circular diffusion with drift variability for Jason's source memory task

% Assumes the eta components in the x and y directions are the
% same.

%    [ll,bic,Pred] = fitdcircle3(Pvar, Pfix, Sel, Data)
%    P = [v1a, v2a, v1b, v2b, eta1, eta2, a, Ter, st,sa]
%          1    2    3    4    5      6   7   8   9   10
%    'Data' is cell array structure created by <makelike>
% ========================================================================
name = 'FITDCIRCLE4x: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Data should be a 1 x 2 cell array...';

tmax = 5.1;
nt = 301;
np = 7; % Number of parameters
n_sz_step =  11; % Criterion variability.
% nlong = 320;
% nshort = 360;
epsx = 1e-9;
cden = 0.05;  % Contaminant density.
epsilon = 0.0001;
% These used by Matlab version of vdcircle by not the C version.
nw = 50;
h = tmax / nt;
w = 2 * pi / nw;


% Set manually if needed - 30/1/19
%badix = 5;

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

v1 = P(1); % Mean drift in the x direction
v2 = P(2); % Mean drift in the y direction
eta1 = P(3); % Drift variability x
eta2 = P(3); % Drift variability y
a= P(4);
ter = P(5);
st = P(6);
sa = P(7);

sigma = 1.0;

%% Bounds and Penalties

% Cleaned up penalty calculation, hard and soft bounds - 30/1/19
penalty = 0;

% ---------------------------------------------------------------------------
%   v1, v2, eta  a,  Ter  st sa]
% ---------------------------------------------------- ----------------------
Ub= [ 3, 3, 3, 4.0, 1.0, 0.7, 3.0];
Lb= [0, 0, 0, 0.5, -0.35, 0,  0];
Pub=[ 2.5, 2.5, 3.5,  4.5, 0.8, 0.65, 2.8];
Plb=[0, 0, 0,  0.7, -0.40, 0.01, 0];
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

pest_penalty(1,:) = P;
pest_penalty(2,:) = max(P - Pub, 0).^2 + max(Plb - P, 0).^2;


if sa < epsilon % No criterion variability
    [t, gt, theta, ptheta, mt] = vdcircle300cls(P, tmax, badix);

else  % Criterion variability
    % Rectangular mass for starting point variability.
    U = ones(n_sz_step, 1);
    Rmass = U / n_sz_step ;
    Rstep = [-(n_sz_step-1)/2:(n_sz_step-1)/2]' / (n_sz_step-1);
    A = a + Rstep * sa; %used to be sz
    gt = zeros(nw+1, nt);
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
end
% Filter zeros
gt = max(gt, epsx);
%plot(ta, gta);
% Add nondecision times
t = t + ter;

% --------------------
% Convolve with Ter(Added from fitgvm, 07/05)
% --------------------
h = t(2) - t(1);
if st > 2 * h
    m = round(st/h);
    n = length(ta);
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
l0 = interp2(angle, time, gt, Data{1}(:,2),Data{1}(:,1), 'linear');

% Pass out joint density for use in quantile plot 8/4
Gstuff = cell(3,1);
Gstuff{1,1} = t;
Gstuff{2,1} = theta;
Gstuff{3,1} = gt;

% Out of range values returned as NaN's. Treat as contaminants - set small.
ix = isnan(l0);
l0(ix) = cden;

% Log-likelihoods.
ll0 = log(l0);

% Minimize sum of minus LL's across two conditions.
ll = sum(-ll0) + penalty;


%% Resume with Fit statistics

bic = 2 * ll + sum(Sel) * log(n);
%    if trace
%       Vala = [Data{1}, l0a, ixa]
%       Valb = [Data{2}, l0b, ixb]
%    end

% Predictions for plot
gtm = sum(gt) * w;
Pgt = [t;gtm];
Pth = [theta;ptheta];
Pred{1} = Pgt;
Pred{2} = Pth;

end
