function [data] = read_sourcemem_data()
%   This function takes in a raw .csv file from the R access data file, and
%   turns it into a MATLAB-friendly array. This version handles the
%   large-ish n dataset with spatial distance columns (but not orthographic
%   or semantic similarity columns)

%   Jason Zhou <jasonz1 AT student DOT unimelb DOT edu DOT au>

% Read Data in
% % J1 is all the numbers, J2 is all the strings, and J3 is the full array
% % MATLAB doesnt like having strings and numbers together

[J1, J2, J3] = xlsread("experiment_1.csv");

% First, exclude the practice blocks
all_data = J1(J1(:,10) ~= 0,:);

% Then, exclude trials with invalid RTs
all_data = all_data(all_data(:,9) ~= 0,:);

% Get a list of participants
participants = unique(all_data(:,39));
data = cell(length(participants),1);

% Rescale response times to be in seconds, not ms
all_data(:,8) = all_data(:,8)/1000;

% Each cell is a participant
% [response error, response time, response angle, target angle,intrusion angle 1 ... intrusion angle 9]
for i = 1:length(participants)
    this_participant_data = all_data(all_data(:,39) == i,:);
    this_participant_cell = zeros(length(this_participant_data), 32);
    this_participant_cell(:,1) = this_participant_data(:,7); % Response Error
    this_participant_cell(:,2) = this_participant_data(:,8); % response time
    this_participant_cell(:,3) = this_participant_data(:,6); % response angle
    this_participant_cell(:,4) = this_participant_data(:,5); % target angle
    this_participant_cell(:,5:13) = this_participant_data(:,21:29); % intrusion offsets
    this_participant_cell(:,14:22) = this_participant_data(:,30:38); % intrusion lags
    this_participant_cell(:,23:31) = this_participant_data(:,40:48); % intrusion spatial distances
    this_participant_cell(:,32) = this_participant_data(:,2); % trial number (in block)
    this_participant_cell(:,33:41) = this_participant_data(:,50:58);
    data{i,1} = this_participant_cell;
end
end

