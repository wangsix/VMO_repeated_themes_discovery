function [P3, R3, F1] = threeLayerF1(Pcell, Qcell)

% Copyright Tom Collins 11/8/2013

% This function calculates three-layer precision, recall, and F1 as defined
% by David Meredith (28/7/2013).

% INPUT
%  Pcell is a cell of cells of point sets that represents a ground truth.
%  QoccSet is a cell of cells of point sets that represents the output of
%   some pattern discovery algorithm.

% EXAMPLE
% Pcell = {...% First ground truth pattern (or occurrence set).
%   {[-0.5 60; 0 63; 0.5 65; 1 67; 1.5 70; 2 72; 2.5 56; 2.5 58;...
%   2.5 60; 2.75 71; 2.83333 70; 2.91667 69]...
%   [2.5 64; 3 67; 3.5 69; 4 71; 4.5 74; 5 76; 5.5 60; 5.5 62; 5.5 64;...
%   5.75 75; 5.83333 74; 5.91667 73.0000]}...
%   ...% Second ground truth pattern (or occurrence set).
%   {[1 67; 1.5 70; 2 72; 2.5 56; 2.5 58; 2.5 60; 2.75 71; 2.91667 69]...
%   [10 68; 10.5 71; 11 73; 11.5 56; 11.5 58; 11.5 60; 11.75 71]}};
% Qcell = {...% First algorithm output pattern (or occurrence set).
%   {[1.5 70; 2 72; 2.5 56; 2.5 58; 2.5 60; 2.75 71; 2.83333 70;...
%   2.91667 69; 3 56; 3 58; 3 60]...
%   [0 63; 2 72; 2.5 56; 2.5 58; 2.5 60; 2.75 71; 2.83333 70; 3 56;...
%   3 58; 3 60]...
%   [2.5 64; 3 67; 3.5 69; 4.5 59; 4.5 61; 5.75 75; 4.83333 71; 5 57;...
%   5 59; 5 61]}...
%   ...% Second algorithm output pattern (or occurrence set).
%   {[1 60; 2 61; 3 56; 3 58; 4 60; 4.75 71; 5 70]...
%   [21 60; 22 61; 23 56; 24 60; 24.75 71; 25 74]...
%   [31 60; 32 61; 33 56; 33 58; 34 62; 34.75 71; 35 70]...
%   [51 60; 52 61; 53 56; 54.75 71; 55 70]}...
%   ...% Third algorithm output pattern (or occurrence set).
%   {[1.5 70; 2 72; 2.5 56; 2.5 58; 2.5 60; 2.75 71; 2.83333 70;...
%   2.91667 69; 3 56; 3 58; 3 60]...
%   [10 68; 10.5 71; 11 73; 11.5 56; 11.5 58; 11.5 60; 11.75 71]}};

% Get number of patterns in the ground truth and output by each algorithm.
nP = size(Pcell, 2);
nQ = size(Qcell, 2);
SP = zeros(nP, nQ);
SR = zeros(nP, nQ);
SF = zeros(nP, nQ);
% Iterate to create the P, R, and F1 matrices for each pair of
% ground truth and algorithm output patterns.
for iP = 1:nP
  % Get number of occurrences provided for each pattern.
  PoccSet = Pcell{iP};
  mPcurr = size(PoccSet, 2);
  % Get cardinalities of each pattern.
  lP = zeros(mPcurr, 1);
  for jP = 1: mPcurr
    lP(jP) = size(PoccSet{jP}, 1);
  end
  for iQ = 1:nQ
    % Get number of occurrences provided for each pattern.
    QoccSet = Qcell{iQ};
    mQcurr = size(QoccSet, 2);
    % P holds all the results of |P n Q|/|Q|, R holds |P n Q|/|P|, and F1
    % holds 2PR/(P + R).
    P = zeros(mPcurr, mQcurr);
    R = zeros(mPcurr, mQcurr);
    F = zeros(mPcurr, mQcurr);
    % Get cardinalities of each pattern.
    lQ = zeros(mQcurr, 1);
    for jQ = 1: mQcurr
      lQ(jQ) = size(QoccSet{jQ}, 1);
    end
    % Iterate to create the P, R, and F1 matrices for each pair of
    % occurrences.
    for jP = 1:mPcurr
      for jQ = 1:mQcurr
        s = size(intersect(PoccSet{jP}, QoccSet{jQ}, 'rows'), 1);
        P(jP, jQ) = s/lQ(jQ);
        R(jP, jQ) = s/lP(jP);
        if P(jP, jQ) > 0 && R(jP, jQ) > 0
          F(jP, jQ) = 2*P(jP, jQ)*R(jP, jQ)/(P(jP, jQ) + R(jP, jQ));
        end
      end
    end
    SP(iP, iQ) = mean(max(F, [], 1));
    SR(iP, iQ) = mean(max(F, [], 2));
    if SP(iP, iQ) > 0 && SR(iP, iQ) > 0
      SF(iP, iQ) = 2*SP(iP, iQ)*SR(iP, iQ)/(SP(iP, iQ) + SR(iP, iQ));
    end
  end
end
% Summarise the P, R, and F1 matrices for the ground truth and algorithm
% output patterns.
P3 = mean(max(SF, [], 1));
R3 = mean(max(SF, [], 2));
if P3 > 0 && R3 > 0
  F1 = 2*P3*R3/(P3 + R3);
else
  F1 = 0;
end

end
