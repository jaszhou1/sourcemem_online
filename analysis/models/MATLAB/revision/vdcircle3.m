function [T,Gt, Theta, Ptheta, Mt] = vdcircle3(P, nw, h, tmax, ter, st, badix)
% ========================================================================
% 2D diffusion on a circle with independent normal drift variability
%     [T, Gt, Theta, Ptheta, Mt] = vdcircle3(P, nw, h, tmax, badix)
%     P = [v1, v2, eta1, eta2, sigma, a]
%     Mt is closed-form E[T]
% ========================================================================
maxarg = 7;
kmax = 50; % Truncation length of series.
epsx = 1e-9; % Small values to substitute for zeroes
if nargin < maxarg
    badix = 0;
end


v1 = P(1);
v2 = P(2);
eta1 = P(3);
eta2 = P(4);
sigma = P(5);
a = P(6);

% trap for eta = 0, set it to something very small, or else Girs becomes
% undefined

if (eta1 <1e-5)
    eta1 = 0.01;
end

if (eta2 <1e-5)
    eta2 = 0.01;
end


[T, Gt0]= dhamana([a, sigma], kmax, h, tmax, badix);

w = 2*pi/nw;
Theta = [-pi: w : pi];
sztheta = length(Theta);
szt = length(T);
Pmt = zeros(1, sztheta);
for i = 1:sztheta
    Pmt(i) = exp(a * cos(Theta(i)) * v1 / sigma^2 + a * sin(Theta(i)) * v2 / sigma^2);
end;


Commonscale = exp(-0.5 * (v1^2/sigma^2 + v2^2/sigma^2) * T);
% Multiply theta-dependent drift term by invariant time-dependent term
Mtscale = Pmt' * Commonscale;
%plot(T, Commonscale .* Gt0)
DensityScale = sum(Commonscale .* Gt0) * h; % Integral of K(|mu|)*G_bessel(t)

Gt0 = Gt0/(2*pi); % Scale to put density on 2d scale.
Gt0u = ones(nw,1) * Gt0; % Replicate copies of g0s.
for i = 1:sztheta
   G11 = (v1 * sigma^2 + a * eta1^2 * cos(Theta(i)))^2;     
   G21 = (v2 * sigma^2 + a * eta2^2 * sin(Theta(i)))^2;     
   Gt(i,1) = 0;
   for k = 2:szt
      Multiplier = sigma^2./((sigma^2 + eta1^2 * T(k)).^0.5 .* (sigma^2 + eta2^2 * T(k)).^0.5);
      G12 = 2 * (eta1^2 * sigma^2) * (sigma^2 + eta1^2 * T(k));
      G22 = 2 * (eta2^2 * sigma^2) * (sigma^2 + eta2^2 * T(k));
      Girs1 = exp(G11/G12 - v1^2/(2*eta1^2));
      Girs2 = exp(G21/G22 - v2^2/(2*eta2^2));
      Gt(i,k) = Multiplier * Girs1 * Girs2 * Gt0(k);
   end
end

Ptheta = Pmt * DensityScale / (2*pi); % -- this agrees!
%mass_in_gt = sum(Gt, 2) * h;
%mean_mass_in_gt = mean(mass_in_gt) * 2 * pi

ActualMass = DensityScale;
%PossibleMass = 1/besseli(0, a * munorm/sigma^2);

% Estimate means numically
%NumericalMean = mean(sum((ones(nw, 1) * T).*Gt, 2)./sum(Gt, 2));

mt = a * besseli(1, a * (v1^2 + v2^2)/sigma^2)/besseli(0, a * (v1^2 + v2^2)/sigma^2) / (v1^2 + v2^2);
Mt = mt * ones(size(Theta));

% Filter zeros
Gt = max(Gt, epsx);

% Add nondecision times
T = T + ter;

% Add nondecision time variability
if st > 2 * h
    m = round(st/h);
    n = length(T);
    fe = ones(1, m) / m;
    for k = 1:nw+1
        gti = conv(Gt(k, :), fe);
        Gt(k,:) = gti(1:n);
    end
end
end

