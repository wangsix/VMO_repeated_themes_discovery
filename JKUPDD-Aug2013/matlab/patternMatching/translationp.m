function tf = translationp(P, Q)

% 23/1/2012 Tom Collins

% This function returns the value 1 if Q is a translation of P, and zero
% otherwise. There is no assumption of lexicographic order, but it is
% assumed that P and Q have unique rows and the same (column) dimension.

% INPUT
%  P is a k-dimensional set of l points.
%  Q is a k-dimensional set of m points.

% EXAMPLE INPUT
% P = [0 0; 0 3+1e-11; 1 2];
% Q = [2 -1; 2 2; 3 1];

tf = 1;
m = size(P, 1);
n = size(Q, 1);
% Dimension of subsets.
k = size(P, 2);
if m ~=  n
    % If P and Q have a different number of points, Q cannot be a
    % tranlsation of P.
    tf = 0;
else
    % Put P and Q in lexicographic order.
    P = unique(P, 'rows');
    Q = unique(Q, 'rows');
    v = Q(1, :) - P(1, :);
    Ptrans = P + repmat(v, m, 1);
    i = 1;
    while i <= m
        if max(abs(Q(i, :) - Ptrans(i, :))) < 1e-5 % Test equality with
                                                   % tolerance for error.
        % if sum(Ptrans(i, :) == Q(i, :)) == k % Old, exact.
            i=i+1;
        else
            tf = 0;
            i = m + 1;
        end
    end
end

end