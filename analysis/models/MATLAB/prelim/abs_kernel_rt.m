function[prt,rt] = abs_kernel_rt(data)
 
%Participant i, thetas, low imageability
[prt,rt]= ksdensity(data(:, 2), 'Kernel', 'epanechnikov', 'Bandwidth', 0.1, 'Support','positive') ;

end