function q = chisqpr(chisq, df);
% ================================================================================
% Returns chi-square tail probability for specified d.f.
%    q = chisqpr(chisq, df);
% ================================================================================
q = zeros(size(chisq));
for i = 1:length(chisq)
    q(i) = 1 - gammainc(chisq(i)/2, df(i)/2);
end;