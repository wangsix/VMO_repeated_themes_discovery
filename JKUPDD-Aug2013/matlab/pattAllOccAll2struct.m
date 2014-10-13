function S = pattAllOccAll2struct(fname)

% Copyright Tom Collins 2/7/2013

% This function converts a text file of the format
%
% pattern1
% occurrence1
% 18.5, 73
% 19, 72
% 19.5, 71
% occurrence2
% 30.5, 60
% 31, 59
% 32, 58
% pattern2
% ...
%
% into a vector of structs consisting of the pattern and the occurrences of
% each pattern.

% INPUT
%  fname is a string specifying the location of the text file to be loaded.

fid = fopen(fname);
A = textscan(fid, '%s');
fclose(fid);
A = A{1};
nrow = size(A, 1);

% Find starting rows for each pattern.
pattStart = zeros(nrow, 1);
patti = 1; % Increment to populate pattStart
for irow = 1:nrow
  if regexp(A{irow}, 'pattern', 'once')
    pattStart(patti) = irow + 1;
    patti = patti + 1;
  end
end
pattn = patti - 1;
pattStart = pattStart(1:pattn);

% Now put the occurrences of each pattern into one struct.
S = struct([]);
for patti = 1:pattn
  % Find starting rows for each occurrence.
  if patti < pattn
    noccrow = pattStart(patti + 1) - pattStart(patti) + 1;
  else
    noccrow = nrow - pattStart(pattn) + 1;
  end
  occStart = zeros(noccrow, 1);
  occi = 1; % Increment to populate pattStart
  for ioccrow = pattStart(patti):(pattStart(patti) + noccrow - 2)
    if regexp(A{ioccrow}, 'occurrence', 'once')
      occStart(occi) = ioccrow + 1;
      occi = occi + 1;
    end
  end
  occn = occi - 1;
  occStart = occStart(1:occn);
  occurrences = cell(1, occn);
  % Now convert each element of each point of each occurrence to a matrix.
  % Begin by working out the dimension of the point set.
  k = 1;
  irow = occStart(1);
  while irow <= nrow
    if regexp(A{irow}, ',')
      k = k + 1;
    else
      irow = nrow; % Cause loop to terminate.
    end
    irow = irow + 1;
  end
  for occi = 1:occn
    % Define the row where the occurrence ends.
    if occi < occn
      occEnd = occStart(occi + 1) - 2;
    else
      if patti < pattn
        occEnd = pattStart(patti) + noccrow - 3;
      else
        occEnd = nrow;
      end
    end
    % Put the occurrence points into a vector.
    currOcc = zeros(noccrow*k, 1);
    relRows = occStart(occi):occEnd;
    nel = size(relRows, 2);
    for iel = 1:nel
      if mod(iel, k)
        currOcc(iel) = str2num(A{relRows(iel)}(1:end - 1)); % Remove comma.
      else
        currOcc(iel) = str2num(A{relRows(iel)});
      end
    end
    % Convert vector to matrix.
    currOcc = reshape(currOcc, k, noccrow)';
    currOcc = currOcc(1:round(nel/k), :);
    occurrences{occi} = currOcc;
  end
  % Update the output struct.
  S(patti).occurrences = occurrences;
  S(patti).pattern = occurrences{1};
end
        
end        
