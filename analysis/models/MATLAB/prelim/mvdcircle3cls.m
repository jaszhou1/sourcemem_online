function [T, Gt, Theta, Ptheta, Mt] = mvdcircle3cls(P, nw, h, tmax);
% ========================================================================
% 2D diffusion on a circle with independent normal drift variability.
% and uniform criterion variability.
% Closes the domain [-pi:pi] so don't lose last item.
%     [T, Gt, Theta, Ptheta, Mt] = mvdcircle3cls(P, nw, h, tmax)
%     P = [v1, v2, eta1, eta2, sigma, a, sa]   
% ========================================================================
name = 'MVDCIRCLE3CLS: ';
errmg1 = 'Incorrect number of parameters for model, exiting...';
errmg2 = 'Criterion range error, exiting....';
maxarg = 5;
np = 7;
n_sa_step = 9; % Number of criterion steps.
nt = tmax / h + 1;    
v1 = P(1);
v2 = P(2);
eta1 = P(3);
eta2 = P(4);
sigma = P(5);
a = P(6);
sa = P(7);
epsilon = 0.01;
epsx = 1e-9;
badix = 5;

if length(P) ~= np
    [name, errmg1], length(P), return;
end

if sa / 2 >= a
    [name, errmg2];
end;

if sa < epsilon % No criterion variability
     Pi = [v1, v2, eta1, eta2, sigma, a];
     % C-version
     [T, Gt, Theta, Ptheta, Mt] = vdcircle300cls(Pi, tmax, 5);
     % Matlab version
     [T, Gt, Theta, Ptheta, Mt] = vdcircle3cls(Pi, nw, h, tmax, 5);
else % Criterion variability
     U = ones(n_sa_step, 1); 
       Rmass = U / n_sa_step ; 
       Rstep = [-(n_sa_step-1)/2:(n_sa_step-1)/2]' / (n_sa_step-1); 
       A = a + Rstep * sa;
       Gt = zeros(nw+1, nt);
       Ptheta = zeros(1, nw+1);
       Mt = zeros(1, nw+1); 
       for i = 1:n_sa_step
           Pi = [v1, v2, eta1, eta2, sigma, A(i)];
           %C-Version
           %[T, Gti, Theta, Pthetai, Mti] = vdcircle300cls(Pi, tmax, 5);
           % Matlab version
           [T, Gti, Theta, Pthetai, Mti] = vdcircle3cls(Pi, nw, h, tmax, 5);
           Gt = Gt + Gti;
           Ptheta = Ptheta + Pthetai; 
           Mt = Mt + Pthetai .* Mti;
          %plot(Theta, Pthetai)
           %if i == 1
           %     hold
           %end
           %A(i)
           %plot(Theta, Mti)
           %pause
      end
      Gt = Gt / n_sa_step;
      Mt = Mt ./ Ptheta;
      Ptheta = Ptheta / n_sa_step;
      %plot(Theta, Ptheta, '+')
      %plot(Theta, Mt, '+')
end
Gt = max(Gt, epsx);
end  % mvdircle3cls



function [T,Gt, Theta, Ptheta, Mt] = vdcircle3cls(P, nw, h, tmax, badix)
% ========================================================================
% 2D diffusion on a circle with independent normal drift variability.
% Closes the domain [-pi:pi] so don't lose last item.
%     [T, Gt, Theta, Ptheta, Mt] = vdcircle3cls(P, nw, h, tmax, badix)
%     P = [v1, v2, eta1, eta2, sigma, a]   
% ========================================================================
maxarg = 5;
kmax = 50; % Truncation length of series.
nw1 = nw + 1; % Enlarged domain.
if nargin < maxarg
    badix = 0;
end;    
v1 = P(1);
v2 = P(2);
eta1 = P(3);
eta2 = P(4);
sigma = P(5);
a = P(6);

if eta1 <1e-5       
    eta1 = 0.01;
end
if eta2 <1e-5       
    eta2 = 0.01;
end 

sigma2 = sigma^2;
eta1onsigma2 = (eta1 / sigma)^2;
eta2onsigma2 = (eta2 / sigma)^2;
two_pi = 2.0 * pi;

[T, Gt0]= dhamana([a, sigma], kmax, h, tmax, badix);

w = two_pi / nw;
Theta = [-pi: w : pi]; % Closed domain
sztheta = length(Theta);
szt = length(T);

Ptheta = zeros(1, sztheta);
Mt = zeros(1, sztheta);


Gt0u = ones(nw,1) * Gt0; % Replicate copies of g0s.

Tscale = sqrt(1./(1 + eta1onsigma2 * T)) .* sqrt(1./(1 + eta2onsigma2 * T));
G11 = 2.0 * eta1 * eta1 * (1 + eta1onsigma2 * T);
G21 = 2.0 * eta2 * eta2 * (1 + eta2onsigma2 * T);
for i = 1:nw
    G12 = v1 + a * eta1onsigma2 * cos(Theta(i));
    G22 = v2 + a * eta2onsigma2 * sin(Theta(i));
    for k = 2:szt
          Girs1 = exp(G12^2 / G11(k) - (v1 / eta1)^2 / 2);
          Girs2 = exp(G22^2 / G21(k) - (v2 / eta2)^2 / 2);
          Gt(i,k) = Tscale(k) * Girs1 * Girs2 * Gt0(k) / two_pi; 
    end
end

totalmass = sum(sum(Gt)) * w * h;
for i = 1:nw
    for k = 2:szt
        Ptheta(i) = Ptheta(i) + (Gt(i,k) + Gt(i,k-1)) /2.0;
        Mt(i) = Mt(i) + (T(k) * Gt(i,k) + T(k - 1) * Gt(i,k)) / 2.0; 
    end
    Ptheta(i) = Ptheta(i) * h / totalmass;
    Mt(i) = Mt(i) * h / Ptheta(i) / totalmass; 
end
% Close the domain
Gt(nw1,:) = Gt(1,:);
Ptheta(nw1) = Ptheta(1);
Mt(nw1) = Mt(1);

end % vdcircircle3cls



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
end % dhamana

function x=besselzero(n,k,kind)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% besselzero.m
%
% Find first k positive zeros of the Bessel function J(n,x) or Y(n,x) 
% using Halley's method.
%
% Written by: Greg von Winckel - 01/25/05
% Contact: gregvw(at)chtm(dot)unm(dot)edu
%
% Use:
%    x=besselzero(n,k,kind)
%       kind = 1: bessel_J; kind = 2: bessel_Y
%       n = order of function
%       k = number of zeros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

k3=3*k;

x=zeros(k3,1);

for j=1:k3
    
    % Initial guess of zeros 
    x0=1+sqrt(2)+(j-1)*pi+n+n^0.4;
    
    % Do Halley's method
    x(j)=findzero(n,x0,kind);

    if x(j)==inf
        error('Bad guess.');
    end
    
end

x=sort(x);
dx=[1;abs(diff(x))];
x=x(dx>1e-8);

x=x(1:k);

function x=findzero(n,x0,kind)

n1=n+1;     n2=n*n;

% Tolerance
tol=1e-12;

% Maximum number of times to iterate
MAXIT=100;

% Initial error
err=1;

iter=0;

while abs(err)>tol & iter<MAXIT
    
    switch kind
        case 1
            a=besselj(n,x0);    
            b=besselj(n1,x0);   
        case 2
            a=bessely(n,x0);
            b=bessely(n1,x0);
    end
            
    x02=x0*x0;
    
    err=2*a*x0*(n*a-b*x0)/(2*b*b*x02-a*b*x0*(4*n+1)+(n*n1+x02)*a*a);
    
    x=x0-err;
    x0=x;
    iter=iter+1;
    
end

if iter>MAXIT-1
    warning('Failed to converge to within tolerance. ',...
            'Try a different initial guess');
    x=inf;    
end

end
end  % besselzero

