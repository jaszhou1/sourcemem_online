function axhandle = setfig4
% ==========================================================================
% setfig:
% Script to construct a 4 x 2 figure object and set default properties.
% (Uses the first four positions of setfig6.)
% Returns axis handles in axhandle, figure handle in fhandle.
%===========================================================================
fhandle = figure;
pw = 21;  % Reference figure sizes for computing positions.
pl = 29;
%set(0,       'ScreenDepth', 1); 
set(fhandle, 'DefaultAxesBox', 'on', ...
             'DefaultAxesLineWidth', 1.5, ...
             'DefaultAxesFontSize', 10, ...
             'DefaultAxesXLim', [0,Inf], ...
             'DefaultAxesYLim', [-Inf,Inf], ...
             'PaperUnits', 'centi', ...
             'PaperType', 'a4', ...
             'PaperPosition', [1, 1, 19, 27], ...
             'Position', [120, 10, 360, 510]);
set(fhandle, 'DefaultLineLineWidth', 0.5, ...
             'DefaultLineColor', [1,1,1], ...
             'DefaultLineLineStyle', '-', ...
             'DefaultLineMarkerSize', 6);
set(fhandle, 'DefaultTextFontSize', 10);
figure(fhandle);
%positions =[ 1.33 17 8 10
%            11.33 17 8 10
%             1.33 5 8 10
%            11.36 5 8 10];
positions =[ 2.3 17 8 10
            12.3 17 8 10
             2.3 3 8 10
            12.3 3 8 10];

positions(:,1) = positions(:,1) / pw;
positions(:,2) = positions(:,2) / pl;
positions(:,3) = positions(:,3) / pw;
positions(:,4) = positions(:,4) / pl;  % Normalized Units
axhandle=[];
for i=1:4
    axh=axes('Position', positions(i,:));
    axhandle=[axhandle,axh];
end;
