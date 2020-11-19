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

MX_Recognised = cell(length(participants),9);
for i = participants
    ll = 1e7;
    badix = 5;
    % Multiple Starts
    %for j = 1:nruns
        [llnew, bic, Pred, pest, Gstuff, penalty, pest_penalty] = Fit_Mix(recognised{i},badix);
        disp(i);
        
        if (llnew < ll && llnew > 0)
            ll = llnew;
            MX_Recognised{i,1} = ll;
            MX_Recognised{i,2} = bic;
            MX_Recognised{i,3} = Pred;
            MX_Recognised{i,4} = pest;
            MX_Recognised{i,5} = Gstuff;
            MX_Recognised{i,6} = recognised {i};
            MX_Recognised{i,7} = penalty;
            MX_Recognised{i,8} = pest_penalty;
            MX_Recognised{i,9} = data{i,3};
        end
    %end
end
%% Plot Fits superimposed on Data, and save.
% % Plot
for i = participants
    filename = ['Cont',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
    fitplot(recognised {i}, VP_Recognised{i,3});
    saveas(gcf,filename);
    
    filename = ['Thresh',num2str(i),'_',datestr(now,'dd_mm_yy_HH_MM'),'.png'];
    fitplot(recognised {i}, MX_Recognised{i,3});
    saveas(gcf,filename);
    
    close all
end

%close all


%% Save mat file
filename = datestr(now,'yyyy_mm_dd_HH_MM');
save(filename)

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Cont','.csv'];
abs_marginal_mat_to_csv(filename, 'Cont', VP_LL_Preds_Recognised);

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Thresh','.csv'];
abs_marginal_mat_to_csv(filename, 'Thresh', MX_LL_Preds_Recognised);

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Hybrid','.csv'];
abs_marginal_mat_to_csv(filename, 'Hybrid', HY_LL_Preds_Recognised);

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Cont','_Joint','.csv'];
abs_mat_to_csv(filename, 'Cont', VP_LL_Preds_Recognised);

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Thresh','_Joint','.csv'];
abs_mat_to_csv(filename, 'Thresh', MX_LL_Preds_Recognised);

filename = [datestr(now,'yyyy-mm-dd-HH-MM'),'_Hybrid','_Joint','.csv'];
abs_mat_to_csv(filename, 'Hybrid', HY_LL_Preds_Recognised);