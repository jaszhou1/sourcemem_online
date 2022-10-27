function [Gt, Theta, T] = vdcircle3_shiftx(P, T, Gt0, num_steps_offset)
% ========================================================================
% 2D diffusion on a circle with independent normal drift variability

% This function calculates the exponential term in canonical orientation to
% be fed into the Girsanov transformation.
% ========================================================================

maxarg = 5;
nw = 50;

if nargin < maxarg
    badix = 0;
end;


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

w = 2*pi/nw;
Theta = [-pi: w : pi];
sztheta = length(Theta);
szt = length(T);
Pmt = zeros(1, sztheta);
for i = 1:sztheta
    Pmt(i) = exp(a * cos(Theta(i)) * v1 / sigma^2 + a * sin(Theta(i)) * v2 / sigma^2);
end

Gt0 = Gt0/(2*pi); % Scale to put density on 2d scale.

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
        Gt(i,k) = Multiplier * Girs1 * Girs2;
    end
end

% Shift Z_T(X) (Gt) by that number of steps along the rows (angles)
% And then apply the Girsanov transformation by multiplying it with the
% zero drift (Gt0)

% Does it make a difference that I shift first and then multiply by Gt0? I
% don't see that it does, since I shift by rows, then multiply each column
% in every row by the corresponding value in Gt0 in each column.
Gt = circshift(Gt, num_steps_offset, 1);
for k = 2:szt
    Gt(:,k) = Gt(:,k) * Gt0(k);
end
end
