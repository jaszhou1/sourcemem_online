function [ll, aic, P, penalty] = fit_spatiotemporal_model(data, P, badix)
setopt;
% Default value for badix (Cuts off the leading edge of RT)
if nargin < 2
    badix = 5;

    %% Default Parameter Starting Points
    % This is in the if statement so that on a first instance, default
    % parameter starting points are used, and then on a second run the best
    % fitting parameters from the previous run are used as starting points

    % The first run lets us start from a wide range of starting points to
    % try and avoid local minima, and then subsequent runs help fine tune
    % each attempt at fitting.
    v1t = normrnd(4, 0.1);
    v2t = 0;
    v1i = normrnd(1.5, 0.05);
    v2i = 0;

    eta1_t = normrnd(0.5, 0.1);
    eta2_t = normrnd(0.1, 0.05);
    eta1_i = normrnd(0.5, 0.1);
    eta2_i = normrnd(0.1, 0.05);

    a_t = normrnd(1.7, 0.1);
    a_g = normrnd(0.5, 0.1);

    beta = normrnd(0.3, 0.1);

    gamma = normrnd(0.05, 0.01);

    % Temporal Gradient
    tau = normrnd(0.6, 0.1); %Weight forwards vs backwards intrusion decay slope
    l_b = normrnd(0.8, 0.1); % Decay of the backwards slope
    l_f = normrnd(0.6, 0.1); % Decay of the forwards slope
    zeta = normrnd(0.5, 0.1); %precision for Shepard similarity function (perceived spatial distance)
    rho = normrnd(0.3, 0.1); % Spatial component weight in intrusion probability calculation
    chi = 0; % Item v Context
    psi = 0; % Semantic v Ortho
    iota = 0; % Ortho decay
    upsilon = 0; % Semantic decay

    % Nondecision Time
    ter = normrnd(0.25, 0.05);
    st = 0;

    P = [v1t, v2t,  v1i,  v2i,  eta1_t, eta2_t, eta1_i, eta2_i,       a_t,  a_g,    beta, gamma,  tau,  l_b,   l_f,   zeta,  rho,   chi,    psi,   iota,  upsilon,    ter,    st];
elseif nargin < 3
    badix = 5;
end

%% Parameter Boundaries

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%            DRIFT RATES                           DRIFT VARIABILITY             CRIT.      GUESS   w(INT)            TEMPORAL         SPATIAL       w(ITEM)        SEMANTIC       DECAY              NON-DEC TIME
%      1      2        3      4                5      6        7      8        9     10      11      12         13     14     15        16      17      18          19          20      21             22      23    
%     [v1t_1, v2t_1,  v1i_1,  v2i_1,       eta1_t, eta2_t, eta1_i, eta2_i,     at,  ag,    beta,    gamma,      tau,  l_b,   l_f,       zeta,  rho,     chi,        psi1,       iota,  upsilon,       Ter,    st]
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Sel= [1,    0,      1,      0,           1,       1,      1,      1,         1,   1,       1,      1,        1,     1,      1,       1,      1,     0,           0,          0,      0,            1,     0];

Ub= [ 10,   1,      8,      1,            1,       1,      1,      1,         4.0, 3.0,    0.6,   0.5,        0.8,   2.5,  2.5,       0.7,   0.7,    0,         0,        0,     0,           0.5,    0.2];
Lb= [ 1,    0,      0.2,    0,            0.01,    0.01,   0.01,   0.01,      0.1, 0.1,    0.05,  0.05,       0.2,   0.3,  0.3,       0.1,   0.1,    0,         0,        0,   0,         0.1,    0];
Pub=[ 9,    0.9,    7,      0.9,          0.9,     0.9,    0.9,    0.9,       3.0, 2.5,    0.55,  0.4,        0.75,  2,    2,         0.6,   0.65,   0,         0,        0,     0,           0.45,   0.15];
Plb=[ 1.1,  0,      0.5,    0,          0.05,    0.05,   0.05,   0.05,        0.7, 0.7,    0.1,   0.1,        0.25,  0.4,  0.4,       0.2,   0.2,    0,         0,        0,   0,         0.2,    0];

Pbounds = [Ub; Lb; Pub; Plb];

%% Call to model code
pest = fminsearch(@diffusion_intrusion_model, P(Sel==1), options, P(Sel==0), Pbounds, Sel, data, badix);
P(Sel==1) = pest;
[ll, aic, P, penalty] = diffusion_intrusion_model(P(Sel==1), P(Sel==0), Pbounds, Sel, data, badix);
end