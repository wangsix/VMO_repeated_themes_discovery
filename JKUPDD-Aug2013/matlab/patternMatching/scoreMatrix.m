function s = scoreMatrix(P, TP, Q, TQ, similarFunc, similarParam,...
  printProgressString)

% Copyright Tom Collins 4/4/2013

% A score matrix can be calculated in order to compare all occurrences of
% one pattern with all occurrences of another, either of which (1) may or
% (2) may not be translationally exact. It can also be calculated in order
% to (3) compare more than two patterns with one another, using one
% occurrence of each.

% This function calculates the score matrix. If translators TP and TQ are
% nonempty, then we are in situation (1), if P and Q are nonempty cells we
% are in situation (2), and if P is a vector of pattern structs and Q is
% empty, we are in situation (3). In situation (1), by convention the
% output is a matrix showing how all occurrences Q + TQ of a pattern in an
% algorithm's output compare to all occurrences P + TP of a ground truth
% pattern.

% INPUT
%  P is a point set.
%  TP is the set of translators provided of P in some point set.
%  Q is a point set.
%  TQ is the set of translators provided of Q in some point set.
%  similarFunc is a string indicating which function should be used for
%   calculating the symbolic music similarity, either 'cardinality score'
%   or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent',
%   'tempoindependent', or 'tempoandpitchindependent', see fpgethistogram2
%   for details).
%  printProgressString is a logical controlling whether a progress string
%   is printed for situation (3).

% EXAMPLE
% P = [-0.5 60; 0 63; 0.5 65; 1 67; 1.5 70; 2 72; 2.5 56; 2.5 58;...
%   2.5 60; 2.75 71; 2.83333 70; 2.91667 69];
% TP = [0 0; 3 4; 20 0];
% Q = [1.5 70; 2 72; 2.5 56; 2.5 58; 2.5 60; 2.75 71; 2.83333 70;...
%   2.91667 69; 3 56; 3 58; 3 60] + repmat([3 4], 11, 1);
% TQ = [0 0; 40 0];
% similarFunc = 'cardinality score';

if nargin < 7
  printProgressString = 0;
end
if nargin < 6
  if strcmp(similarFunc, 'cardinality score')
    similarParam = 0;
  else
    similarParam = 'normal';
  end
end

% If using normalised matching score, check which implementation has been
% defined.
if strcmp(similarFunc, 'normalised matching score')
  if exist('fpgethistogram') == 3
    h_nsm = @fpgethistogram;
  else
    h_nsm = @fpgethistogram2;
  end
end

if ~isempty(TP) && ~isempty(TQ) % Situation (1).
  % Get cardinalities of each pattern.
  lP = size(P, 1);
  lQ = size(Q, 1);
  denom = max(lP, lQ);
  % Get number of translators provided for each pattern.
  mP = size(TP, 1);
  mQ = size(TQ, 1);
  s = zeros(mP, mQ);
  % If using normalised matching score, calculate maximum possible score
  % for fingerprinting.
  if strcmp(similarFunc, 'normalised matching score')
    if lP >= lQ
      fphist = h_nsm(P, P, similarParam);
      maxscore = max(fphist(:, 2));
    else
      fphist = h_nsm(Q, Q, similarParam);
      maxscore = max(fphist(:, 2));
    end
  end
  % Iterate to create the score matrix.
  for iP = 1:mP
    tP = P + repmat(TP(iP, :), lP, 1);
    for iQ = 1:mQ
      tQ = Q + repmat(TQ(iQ, :), lQ, 1);
      if strcmp(similarFunc, 'cardinality score')
        s(iP, iQ) = size(intersect(tP, tQ, 'rows'), 1)/denom;
      elseif strcmp(similarFunc, 'normalised matching score')
        fphist = h_nsm(tP, tQ, similarParam);
        if ~isempty(fphist) % Empty histogram is returned for no matches.
          s(iP, iQ) = sqrt(max(fphist(:, 2))/maxscore);
        end
      end
    end
  end
  
elseif ~isempty(P) && ~isempty(Q) % Situation (2).
  % Get number of occurrences provided for each pattern.
  mP = size(P, 2);
  mQ = size(Q, 2);
  s = zeros(mP, mQ);
  % Get cardinalities of each pattern.
  lP = zeros(mP, 1);
  for iP = 1: mP
    lP(iP) = size(P{iP}, 1);
  end
  lQ = zeros(mQ, 1);
  for iQ = 1: mQ
    lQ(iQ) = size(Q{iQ}, 1);
  end
  % Iterate to create the score matrix.
  for iP = 1:mP
    tP = P{iP};
    for iQ = 1:mQ
      tQ = Q{iQ};
      if strcmp(similarFunc, 'cardinality score')
        if similarParam
          s(iP, iQ) = cardinalityScore(tP, tQ, similarParam);
        else
          denom = max(lP(iP), lQ(iQ));
          s(iP, iQ) = size(intersect(tP, tQ, 'rows'), 1)/denom;
        end
      elseif strcmp(similarFunc, 'normalised matching score')
         % If using normalised matching score, calculate maximum possible
         % score for fingerprinting.
        if lP(iP) >= lQ(iQ)
          fphist = h_nsm(tP, tP, similarParam);
          maxscore = max(fphist(:, 2));
        else
          fphist = h_nsm(tQ, tQ, similarParam);
          maxscore = max(fphist(:, 2));
        end
        fphist = h_nsm(tP, tQ, similarParam);
        if ~isempty(fphist) % Empty histogram is returned for no matches.
          s(iP, iQ) = sqrt(max(fphist(:, 2))/maxscore);
        end
      end
    end
  end
  
elseif isstruct(P) && isempty(Q) % Situation (3).
  % Get number of patterns provided.
  mP = size(P, 2);
  s = ones(mP);
  % Get cardinalities of each pattern.
  lP = zeros(mP, 1);
  for iP = 1: mP
    lP(iP) = P(iP).cardinality;
  end
  % Iterate to create the score matrix.
  for iP = 1:mP
    tP = P(iP).pattern;
    for jP = iP + 1:mP
      if printProgressString
        fprintf('Row %d, column %d of %d.\n', iP, jP, mP)
      end
      tQ = P(jP).pattern;
      if strcmp(similarFunc, 'cardinality score')
        if similarParam
          curr_sim = cardinalityScore(tP, tQ, similarParam);
        else
          denom = max(lP(iP), lP(jP));
          curr_sim = size(intersect(tP, tQ, 'rows'), 1)/denom;
        end
      elseif strcmp(similarFunc, 'normalised matching score')
         % If using normalised matching score, calculate maximum possible
         % score for fingerprinting.
        if lP(iP) >= lP(jP)
          fphist = h_nsm(tP, tP, similarParam);
          maxscore = max(fphist(:, 2));
        else
          fphist = h_nsm(tQ, tQ, similarParam);
          maxscore = max(fphist(:, 2));
        end
        fphist = h_nsm(tP, tQ, similarParam);
        if ~isempty(fphist) % Empty histogram is returned for no matches.
          curr_sim = sqrt(max(fphist(:, 2))/maxscore);
        else
          curr_sim = 0;
        end
      end
      s(iP, jP) = curr_sim;
      s(jP, iP) = curr_sim;
    end
  end
end

end
