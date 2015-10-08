function [s_card, max_trans_vec] = cardinalityScore(P, Q, allowTrans)

% Calculates the difference between each pair of points in P and Q, sorts
% by frequency of occurrence, and then returns the frequency of the most
% frequently occurring difference vector, divided by the maximum of the
% number of points in P and Q. If P is a translation of Q, then the
% cardinality score is 1; if no two pairs of P points and Q points are
% translations, then the cardinality score is zero; otherwise it is
% somewhere between the two.
% 
% TC 2013.4.22

% INPUT
%  P is an m x k matrix of real numbers.
%  Q is an n x k matrix of real numbers.
%  allowTrans is a logical. If equal to one, cardinality score is
%   calculated up to translations. Otherwise, translations are not
%   permitted.

% EXAMPLE INPUT
% P = [1 1; 1 3; 1 4; 2 2; 3 1; 4 1; 4 4];
% Q = [3 4; 3 6; 3 7; 4 2; 5 4; 5 5; 6 7; 7 1];
% allowTrans = 1;

m = size(P, 1);
n = size(Q, 1);
if allowTrans
  % Calculate the difference array, but leave it as a vector.
  k = size(P, 2);
  N = m*n;
  V = zeros(N, k);
  L = 1; % Increment to populate V.
  for i = 1:m
    for j = 1:n
      V(L, :) = Q(j, :) - P(i, :);
      L = L + 1;
    end
  end
  % Count the difference array.
  V2 = count(V, 'rows');
  [numerator, idx] = max(V2(:, k + 1));
  max_trans_vec = V2(idx, 1:k);
else
  numerator = size(intersect(P, Q, 'rows'), 1);
  max_trans_vec = [];
end

s_card = numerator/max(m, n);

end
