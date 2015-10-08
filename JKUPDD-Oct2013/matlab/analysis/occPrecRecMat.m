function [p, r, Op, Or] = occPrecRecMat(datasetStruct, algoOutput,...
  similarThresh, similarFunc)

% 4/4/2013 Copyright Tom Collins

% This function calculates the establishment matrix, which is a matrix for
% recording an algorithm's capability for establishing that patterns in the
% dataset struct (ground truth) are repeated at least once during the input
% point set (e.g., piece of music). It is calculated by taking the score
% matrix for each pair of ground truth (row) and output (column) pattern,
% and finding the maximum value. Also returned are the establishment
% precision and establishment recall. The former is the mean of the
% columnwise maxima; the latter is the mean of the rowwise maxima.

% INPUT
%  datasetStruct is a struct consisting of fields k, details for patterns
%   and details.translators for occurrences of patterns. It is usually the
%   ground truth for a piece of music.
%  algoOutput is a vector of structs, containing the output of a pattern
%   discovery algorithm.
%  similarFunc is a string indicating which function should be used for
%   calculating the symbolic music similarity.

% EXAMPLE
% % Load ground truth.
% path = fullfile('~', 'ConferencesPresentations', 'ISMIR', '2013');
% fin = fullfile(path, 'repeatedSectionsGroundTruth',...
%   'beet_op002_no1_mv1.mat');
% load(fin, 'datasetStruct');
% % Load algorithm output.
% fin = fullfile(path, 'pattDiscTrainOut',...
%   'beet_op002_no1_mv1_SIARCT.mat');
% algoOutput = load(fin, 'S3');
% algoOutput = algoOutput.S3;
% % Now try another algorithm's output.
% fin = fullfile(path, 'pattDiscTrainOut',...
%   'beet_op002_no1_mv1_SIA_50+.mat');
% algoOutput = load(fin, 'S4');
% algoOutput = algoOutput.S4;
% similarFunc = 'cardinality score';

details = datasetStruct.details;
k = datasetStruct.k;
nP = size(details, 2);
nQ = size(algoOutput, 2);
Op = zeros(nP, nQ); % The occurrence precision matrix.
Or = zeros(nP, nQ); % The occurrence recall matrix.
% Index the values of the establishment matrix (not defined explicitly
% here) that are greater than some specifiable threshold.
rel_idx = zeros(nP*nQ, 2);
% Increment over relevant (greater-than-threshold) indices of Op and Or.
relj = 1;
% Iterate over dataset struct and algorithm output, and calculate the score
% matrix for each pair.
for iP = 1:nP
  P = details(iP).pattern;
  if isfield(details, 'translators')
    TP = details(iP).translators;
  else
    TP = [];
    P = details(iP).occurrences;
    % P = cell(1);
    % P{1} = details(iP).pattern;
  end
  for iQ = 1:nQ
    if mod(iQ, 1000) == 0
      fprintf(['Ground truth pattern %d of %d.'...
        ' Algorithm output pattern %d of %d.\n'], iP, nP, iQ, nQ)
    end
    Q = algoOutput(iQ).pattern;
    if isfield(algoOutput(iQ), 'translators')
      % Typical scenario where there are translators.
      TQ = algoOutput(iQ).translators;
    elseif isfield(algoOutput(iQ), 'vector')
      % No translators recorded; use the zero vector and MTP translation
      % vector if recorded to define some translators.
      TQ = [zeros(1, k); algoOutput(iQ).vector];
    elseif ~isfield(algoOutput(iQ), 'translators')
      % Set translators to empty, and Q to a cell of occurrences.
      TQ = [];
      Q = algoOutput(iQ).occurrences;
    else
      TQ = zeros(1, k);
    end
    s = scoreMatrix(P, TP, Q, TQ, similarFunc);
    % If the pattern can be considered to be discovered, then enter it in
    % the occurrence precision matrix and occurrence recall matrix, and add
    % iP and iQ to the matrix of relevant indices.
    [C, ~] = max(s(:));
    if C >= similarThresh
      % Calculate the precision of this score matrix.
      Op(iP, iQ) = mean(max(s, [], 1));
      % Calculate the recall of this score matrix.
      Or(iP, iQ) = mean(max(s, [], 2));
      rel_idx(relj, :) = [iP iQ];
      relj = relj + 1;
    end
  end
end
rel_idx = rel_idx(1:relj - 1, :);

% Calculate occurrence precision.
p = mean(max(Op(rel_idx(:, 1), rel_idx(:, 2)), [], 1));
if isnan(p)
  p = 0;
end

% Calculate occurrence recall.
r = mean(max(Or(rel_idx(:, 1), rel_idx(:, 2)), [], 2));
if isnan(r)
  r = 0;
end

end
