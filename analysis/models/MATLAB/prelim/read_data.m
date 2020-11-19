function [data] = read_data()
%
% Jason Zhou <jasonz1 AT student DOT unimelb DOT edu DOT au>

%Read Data in
% % J2 and J3 are auxiliary, tells you what the column headers are.
% % MATLAB doesnt like having strings and numbers together
[J1, J2, J3] = xlsread("rep_data.csv");

% Rescale RTs to be measured in s rather than ms for compatability with
% MATLAB version of this code
J1(:,7) = J1(:,7)/1000;

% J1(:,1) codes the presentation conditions with 1 = SEQ, 0 = SIM
seq_data = J1(J1(:,1) == true,:);
sim_data = J1(J1(:,1) == false,:);

HC = J1(J1(:,2)>=3,:);
LC = J1(J1(:,2)<3,:);

participants = unique(J1(:,9));

% Extract the Response Error and RTs and store in new structure

data = cell(length(participants),3); 
% First column will be unrecognised items, second is recognised items, and
% third will be the presentation condition.

for i = 1:length(participants)
    % Extract data for participant i (Col 9)
    this_low = LC(LC(:,9) == participants(i),:);
    this_high = HC(HC(:,9) == participants(i),:);
    % Marking presentation condition
    data{i,3} = this_low(1,1);
    
    % Extract Error (Col 6) and RT (Col 7)
    data{i,1}(:,1) = this_low(:,6);
    data{i,1}(:,2) = this_low(:,7);
    data{i,2}(:,1) = this_high(:,6);
    data{i,2}(:,2) = this_high(:,7);
end
end