%% model_recovery.m
% This is a script to run a model recovery exercise between the continuous
% and threshold versions of the circular diffusion model reported in Zhou
% et al, 2020. 

% Jason Zhou <jasonz1 AT student DOT unimelb DOT edu DOT au>
% 05/09/2020
%% Simulate Data
% For each individual, generate a set of simulated data. The parameters
% used to generate simulated data will be those that provided the best fit
% of that model to that participant's empirical data, and will be based on
% the same number of observations. 

% See Donkin et al, 2013 at https://dx.doi.org/10.1037
[cont,thresh] = simulate_data(0);

%% Cross-fit Models
% Use the same fitting procedures as those reported in the original
% manuscript to fit each model to the simulated datasets
participants = 1:length(cont);
nruns = 10;
badix = 5;

% The format of each heading will be:
% MODEL USED TO GENERATE THE SIMULATED DATA - MODEL BEING USED TO FIT

% Continuous-Continuous
% Data generated by the continuous model, fit using the continuous model
VP_VP = cell(length(participants),8);
for i = participants
    ll = 1e7;
    
    % Multiple Starts
    for j = 1:nruns
        [llnew, bic, Pred, pest, Gstuff, penalty, pest_penalty] = ...
            Fit_VP(cont{i,2},badix);
  
        if (llnew < ll && llnew > 0)
            ll = llnew;
            VP_VP{i,1} = ll;
            VP_VP{i,2} = bic;
            VP_VP{i,3} = Pred;
            VP_VP{i,4} = pest;
            VP_VP{i,5} = Gstuff;
            VP_VP{i,6} = cont(i,:);
            VP_VP{i,7} = penalty;
            VP_VP{i,8} = pest_penalty;
        end
    end 
end

% Continuous-Threshold
VP_MX = cell(length(participants),8);
for i = participants
    VP_MX{i,1} = -1;
    VP_MX{i,2} = 0;
    
    ll = 1e7;
    for j=1:nruns
        [llnew, bic, Pred, pest, Gstuff,penalty, pest_penalty] =...
            Fit_Mix(cont{i,2},badix);
        
        if (llnew < ll && llnew > 0)
            ll = llnew;
            VP_MX{i,1} = ll;
            VP_MX{i,2} = bic;
            VP_MX{i,3} = Pred;
            VP_MX{i,4} = pest;
            VP_MX{i,5} = Gstuff;
            VP_MX{i,6} = cont(i,:);
            VP_MX{i,7} = penalty;
            VP_MX{i,8} = pest_penalty;
        end
    end
end


% Threshold-Continuous
MX_VP = cell(length(participants),8);
for i = participants
    ll = 1e7;
    
    % Multiple Starts
    for j = 1:nruns
        [llnew, bic, Pred, pest, Gstuff, penalty, pest_penalty] = ...
            Fit_VP(thresh{i,2},badix);
  
        if (llnew < ll && llnew > 0)
            ll = llnew;
            MX_VP{i,1} = ll;
            MX_VP{i,2} = bic;
            MX_VP{i,3} = Pred;
            MX_VP{i,4} = pest;
            MX_VP{i,5} = Gstuff;
            MX_VP{i,6} = thresh(i,:);
            MX_VP{i,7} = penalty;
            MX_VP{i,8} = pest_penalty;
        end
    end 
end

% Threshold-Threshold
MX_MX = cell(length(participants),8);
for i = participants
    MX_MX{i,1} = -1;
    MX_MX{i,2} = 0;
    
    ll = 1e7;
    for j=1:nruns
        [llnew, bic, Pred, pest, Gstuff,penalty, pest_penalty] =...
            Fit_Mix(thresh{i,2},badix);
        
        if (llnew < ll && llnew > 0)
            ll = llnew;
            MX_MX{i,1} = ll;
            MX_MX{i,2} = bic;
            MX_MX{i,3} = Pred;
            MX_MX{i,4} = pest;
            MX_MX{i,5} = Gstuff;
            MX_MX{i,6} = thresh(i,:);
            MX_MX{i,7} = penalty;
            MX_MX{i,8} = pest_penalty;
        end
    end
end
%% Plot Fits superimposed on Data, and save.
% % Plot
% for i = participants
%    filename = ['Cont_Cont',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%    fitplot(cont(i,:), VP_VP{i,3});
%    saveas(gcf,filename);
    
%    filename = ['Cont_Thresh',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%    fitplot(cont(i,:), VP_MX{i,3});
%    saveas(gcf,filename);

%    filename = ['Thresh_Cont',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%    fitplot(thresh(i,:), MX_VP{i,3});
%    saveas(gcf,filename);
    
%    filename = ['Thresh_Thresh',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%    fitplot(thresh(i,:), MX_MX{i,3});
%    saveas(gcf,filename);
%    close all     
% end

%% Save 

% Save MATLAB workspace
filename = [datestr(now,'yyyy_mm_dd_HH_MM'),'_recovery_sim'];
save(filename)
