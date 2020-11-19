function[ptheta,theta] = abs_kernel_theta(data)
 
%Participant i, thetas, low imageability
[ptheta,theta]= ksdensity(data(:, 1), 'Kernel', 'epanechnikov', 'Bandwidth', 0.1, 'Support',[-pi,pi]) ;

end