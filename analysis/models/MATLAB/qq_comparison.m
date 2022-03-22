% Change parameters of the spatiotemporal model and see what happens to qq
% predictions


%  Default Parameters for simulation
%  [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  ag,  gamma, beta, kappa, l_b,   l_f,   zeta,  rho, Ter, st]
P = [4,    0,    1,  0,    0.2,   0.1,    3,    1,  0.05,  0.2,  0.5,   0.2,   0.5,   0.7,   0.8, 0.05,	0];
simulate_qq(P)
saveas(gcf,'1.png')

hold off
close all

%  Smaller drift rate for intrusions
%  [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  ag,  gamma, beta, kappa, l_b,   l_f,   zeta,  rho, Ter, st]
P = [4,    0,    0.1,  0,    0.2,   0.1,    3,    1,  0.05,  0.2,  0.5,   0.2,   0.5,   0.7,   0.8, 0.05,	0];
simulate_qq(P)
saveas(gcf,'2.png')

hold off
close all

%  Equal drift rate for intrusions
%  [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  ag,  gamma, beta, kappa, l_b,   l_f,   zeta,  rho, Ter, st]
P = [4,    0,    4,  0,    0.2,   0.2,    4,    1,  0.05,  0.2,  0.5,   0.2,   0.5,   0.7,   0.8, 0.05,	0];
simulate_qq(P)
saveas(gcf,'3.png')

hold off
close all

%  
%  [v1t, v2t,  v1i, v2i, eta_t, eta_i,   at,  ag,  gamma, beta, kappa, l_b,   l_f,   zeta,  rho, Ter, st]
P = [4,    0,    3,  0,    3,   3,    3,    1,  0.05,  0.2,  0.5,   0.2,   0.5,   0.7,   0.8, 0.05,	0];
simulate_qq(P)
saveas(gcf,'4.png')

hold off
close all