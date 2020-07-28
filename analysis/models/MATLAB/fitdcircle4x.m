function [ll,bic,Pred, Gstuff, penalty, pest_penalty] = fitdcircle4x(Pvar, Pfix, Sel, Data, nlow, nhigh, badix, trace)
% ========================================================================
% Circular diffusion with drift variability for Jason's source memory task
% with across-trial variability in criterion
% Assumes the eta components in the x and y directions are the
% same.


%   Pooled indicates that parameters for low and high imageability (or long and short
%   delay in the original set-up) are constrained to be the same. We are doing this because
%   we saw no difference in the summary statistics of data between the two
%   conditions


%    [ll,bic,Pred] = fitdcircle3(Pvar, Pfix, Sel, Data)
%    P = [v1a, v2a, v1b, v2b, eta1, eta2, a, Ter, st,sa]
%          1    2    3    4    5      6   7   8   9   10
%    'Data' is cell array structure created by <makelike>
% ========================================================================
name = 'FITDCIRCLE4: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Incorrect length selector vector, exiting...';
errmg3 = 'Data should be a 1 x 2 cell array from <makelike>...';

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


if nargin < 8
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

%% Setting parameters to be the same. 
%I think this is fine for the sake of calculating the penalty, the vectors
%for low and high conditions are drawn from this afterwards.
v1a = P(1);
v2a = P(2);
v1b = P(1);
v2b = P(2);
eta1 = P(3);
eta2 = P(3);
a= P(4);
ter = P(5);
st = P(6);
sa = P(7);

sigma = 1.0;


eta1a = eta1;
eta2a = eta1;
eta1b = eta2;
eta2b = eta2;

%% Transplanting the fitmixture4x penalties into fitdcircle

% Cleaned up penalty calculation, hard and soft bounds - 30/1/19
penalty = 0;

% ---------------------------------------------------------------------------
%   v1, v2, eta  a,  Ter  st sa]
% ---------------------------------------------------- ----------------------
Ub= [ 3, 3, 3, 4.0, 1.0, 0.7, 3.0]; 
Lb= [0, 0, 0, 0.5, -0.35, 0,  0];
Pub=[ 2.5, 2.5, 3.5,  4.5, 0.8, 0.65, 2.8]; 
Plb=[0, 0, 0,  0.7, -0.40, 0.01, 0];
Pred = cell(1,4);
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


% Parameters for low.
    Pa = [v1a, v2a, eta1a, eta2a, sigma, a, ter, sa];
    % Parameters for high.
    Pb = [v1b, v2b, eta1b, eta2b, sigma, a, ter, sa];
    if sa < epsilon % No criterion variability
       % Parameters for long
       Pa = [v1a, v2a, eta1a, eta2a, sigma, a];
       % Parameters for short
       Pb = [v1b, v2b, eta1b, eta2b, sigma, a];
       %[ta, gta, thetaa, pthetaa, mta] = vdcircle300cls(Pa, tmax, badix);
       %[tb, gtb, thetab, pthetab, mtb] = vdcircle300cls(Pb, tmax, badix);
       [ta, gta, thetaa, pthetaa, mta] = vdcircle3cls(Pa, nw, h, tmax, badix);
       [tb, gtb, thetab, pthetab, mtb] = vdcircle3cls(Pb, nw, h, tmax, badix);
   else  % Criterion variability
       % Rectangular mass for starting point variability.
       U = ones(n_sz_step, 1); 
       Rmass = U / n_sz_step ; 
       Rstep = [-(n_sz_step-1)/2:(n_sz_step-1)/2]' / (n_sz_step-1);        
       A = a + Rstep * sa; %used to be sz
       gta = zeros(nw+1, nt);
       gtb = zeros(nw+1, nt);
       pthetaa = zeros(1, nw+1);
       pthetab = zeros(1, nw+1);
       mta = zeros(1, nw+1);
       mtb = zeros(1, nw+1);
       for i = 1:n_sz_step
           Pai = [v1a, v2a, eta1a, eta2a, sigma, A(i)];
           Pbi = [v1b, v2b, eta1b, eta2b, sigma, A(i)];
           %[ta, gta, thetaa, pthetaa, mta] = vdcircle300cls(Pa, tmax, badix);
           %[tb, gtb, thetab, pthetab, mtb] = vdcircle300cls(Pb, tmax, badix);
           [ta, gta, thetaa, pthetaa, mta] = vdcircle3cls(Pa, nw, h, tmax, badix);
           [tb, gtb, thetab, pthetab, mtb] = vdcircle3cls(Pb, nw, h, tmax, badix);
           gta = gta + gtai;
           gtb = gtb + gtbi;
           pthetaa = pthetaa + pthetaai;
           pthetab = pthetab + pthetabi;
           mta = mta + pthetaai .* mtai;
           mtb = mtb + pthetabi .* mtbi;
      end
      gta = gta / n_sz_step;
      gtb = gtb / n_sz_step;
      mta = mta ./ pthetaa;
      mtb = mtb ./ pthetab;
      pthetaa = pthetaa / n_sz_step;
      pthetab = pthetab / n_sz_step;
    end
   % Filter zeros
   gta = max(gta, epsx);
   gtb = max(gtb, epsx);
   %plot(ta, gta);
   % Add nondecision times
   ta = ta + ter;
   tb = tb + ter;
   
   
   % --------------------
   % Convolve with Ter(Added from fitgvm, 07/05)
   % --------------------
   h = ta(2) - ta(1);
   if st > 2 * h
       m = round(st/h);
       n = length(ta);
       fe = ones(1, m) / m;
       for i = 1:nw + 1
           gti = conv(gta(i, :), fe);
           gta(i,:) = gti(1:n);
           gti = conv(gtb(i, :), fe);
           gtb(i,:) = gti(1:n);
       end
   end
   
   % Create mesh for interpolation
   [anglea,timea]=meshgrid(ta, thetaa);
   [angleb,timeb]=meshgrid(tb, thetab);

   % Interpolate in joint density to get likelihoods of each data point.
   % 1 is long, 2 is short
   l0a = interp2(anglea, timea, gta, Data{1}(:,2),Data{1}(:,1), 'linear');
   l0b = interp2(angleb, timeb, gtb, Data{2}(:,2),Data{2}(:,1), 'linear');
   
   
  % Pass out joint density for use in quantile plot 8/4
   Gstuff = cell(3,2);
   Gstuff{1,1} = ta;
   Gstuff{2,1} = thetaa;
   Gstuff{3,1} = gta;
   Gstuff{1,2} = tb;
   Gstuff{2,2} = thetab;
   Gstuff{3,2} = gtb;
   
   % Out of range values returned as NaN's. Treat as contaminants - set small.
   ixa = isnan(l0a);
   l0a(ixa) = cden;
   ixb = isnan(l0b);
   l0b(ixb) = cden;
   % Log-likelihoods.
   ll0a = log(l0a);
   ll0b = log(l0b);
   

   % Minimize sum of minus LL's across two conditions.
   ll = sum(-ll0a) + sum(-ll0b) + penalty;
 
   
   %% Resume with Fit statistics 
 
   bic = 2 * ll + sum(Sel) * log(nlow + nhigh);
%    if trace
%       Vala = [Data{1}, l0a, ixa]
%       Valb = [Data{2}, l0b, ixb]
%    end

   % Predictions for plot
   gtam = sum(gta) * w;
   gtbm = sum(gtb) * w;
   Pgta = [ta;gtam];
   Pgtb = [tb;gtbm];
   Ptha = [thetaa;pthetaa];
   Pthb = [thetab;pthetab];
   Pred{1} = Pgta;
   Pred{2} = Pgtb;
   Pred{3} = Ptha;
   Pred{4} = Pthb;

end