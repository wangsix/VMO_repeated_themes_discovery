function [p r tf] = precisionRecallTrans(algoOutput, datasetStruct)

% 23/1/2012 Tom Collins

% This function returns the precision and recall of a pattern discovery
% algorithm, up to translational equivalence. Precision is the number of
% target patterns returned, divided by the sum of target and nontarget
% patterns returned. Recall is the number of target patterns returned,
% divided by the total number of target patterns. The third returned
% argument takes the value one if any of the first five returned patterns
% are targets, and zero otherwise.

% tic
tf = 0;
total = size(algoOutput, 2);
runningCount = 0;
targetn = size(datasetStruct.details, 2);
% Increment over target patterns
for targeti = 1:targetn
    % Increment over output patterns
    outi = 1;
    while outi <= total
        if translationp(datasetStruct.details(targeti).pattern,...
                algoOutput(outi).pattern)
            % A translation of a target pattern has been found. If it is
            % for one of the first five returned patterns, set tf equal to
            % one.
            if outi <= 5
                tf = 1;
            end
            runningCount = runningCount + 1;
            outi = total + 1;
        else
            outi=outi+1;
        end
    end
end
p = runningCount/total;
r = runningCount/targetn;
% toc

end
