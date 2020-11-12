function [Data1, Data2] = makesubject_simple(sno, Alldata, size1, size2)
% ==============================================================================
% Make data structure for one subject for EXPSOURCE-IMG.
%    [Data1, Data2] = makesubject(sno, Alldata)
%    Columns are subject, trialno, targtheta, rt, resptheta, error
% Maximum subject number is 20: missing 14, 6,
% ==============================================================================
ntrials1 = size1;  % (not anymore) Different numbers for Short and Long!
ntrials2 = size2;
Data1 = zeros(ntrials1, 6);
Data2 = zeros(ntrials2, 6);

t1 = 1;
t2 = 1;
sz = size(Alldata);
for i = 1:sz(1)
    if round(Alldata(i,8) == sno)  % Include this subject 
        targtheta = Alldata(i,4);
        resptheta = Alldata(i,5);
        error = Alldata(i,6);
        rt = Alldata(i,7);
        
        %          Data1(t1, 1) = sno;
        %          Data1(t1, 2) = t1;
        %          Data1(t1, 3) = targtheta;
        %          Data1(t1, 4) = rt;
        %          Data1(t1, 5) = resptheta;
        %          Data1(t1, 6) = error;
        %          t1 = t1 + 1; %So then it writes to the next line, for the next trial of that subject.
        
        if round(Alldata(i,1) == 1) % Low Imageability Condition
            Data1(t1, 1) = sno;
            Data1(t1, 2) = t1;
            Data1(t1, 3) = targtheta;
            Data1(t1, 4) = rt;
            Data1(t1, 5) = resptheta;
            Data1(t1, 6) = error;
            t1 = t1 + 1;
        else % High
            Data2(t2, 1) = sno;
            Data2(t2, 2) = t2;
            Data2(t2, 3) = targtheta;
            Data2(t2, 4) = rt;
            Data2(t2, 5) = resptheta;
            Data2(t2, 6) = error;
            t2 = t2 + 1;
        end
    end
end


