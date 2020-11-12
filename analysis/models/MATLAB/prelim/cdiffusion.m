%% This is copy-pasted over from FitHybridCrit, with a fixed at zero and boundaries allowing zero.
% TODO: Implement this as a switch in an overall fitting script rather than
% have two separate folders.
%% Open Data
%data = read_data();
load('REP.mat')
recognised = data(:,2);
unrecognised = data(:,1);

%Fit the data, generate predictions.

participants = 1:5;

nruns = 10; %Number of times I want to run fits on each participant to find the best fit

% % %Empty array for Log Likelihoods and Predictions to live.
VP_Recognised = cell(length(participants),9);
for i = participants 
    ll = 1e7;
    badix = 5;
    % Multiple Starts
    for j = 1:nruns
        [llnew, bic, Pred, pest, Gstuff, penalty, pest_penalty] = Fit_VP(recognised{i},badix);
        disp(i);
        
        if (llnew < ll && llnew > 0)
            ll = llnew;
            VP_Recognised{i,1} = ll;
            VP_Recognised{i,2} = bic;
            VP_Recognised{i,3} = Pred;
            VP_Recognised{i,4} = pest;
            VP_Recognised{i,5} = Gstuff;
            VP_Recognised{i,6} = recognised {i};
            VP_Recognised{i,7} = penalty;
            VP_Recognised{i,8} = pest_penalty;
            VP_Recognised{i,9} = data{i,3};
        end
    end
end

%% Plot Fits superimposed on Data, and save.
% % Plot
%  for i = participants
%     filename = ['Cont_Recog_pooled',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%     fitplot(seq_recognised {i}, VP_LL_Preds_Recognised{i,3});
%     saveas(gcf,filename);
%
%     filename = ['Thresh_Recog_pooled',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%     fitplot(seq_recognised {i}, MX_LL_Preds_Recognised{i,3});
%     saveas(gcf,filename);
%
%      filename = ['Hybrid_Recog_pooled',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
%      fitplot(seq_recognised {i}, HY_LL_Preds_Recognised{i,3});
%      saveas(gcf,filename);
%      close all
%  end

%close all


%% Save mat file
filename = datestr(now,'yyyy_mm_dd_HH_MM');
save(filename)