function [volClean, keepMask, stats] = filterSurface(vol, Kmad, minPoints)
% Row-wise robust outlier removal using MAD (Median Absolute Deviation).
%
% Inputs:
%   vol       : [nTenors x nStrikes] implied vols
%   Kmad      : threshold multiplier (e.g. 5)
%   minPoints : minimum number of valid points in a row to apply MAD (e.g. 6)
%
% Outputs:
%   volClean  : same size as vol, outliers -> NaN
%   keepMask  : logical mask of kept points
%   stats     : per-row medians, MADs, kept counts

    if nargin < 2 || isempty(Kmad), Kmad = 5; end
    if nargin < 3 || isempty(minPoints), minPoints = 6; end

    [nT, nK] = size(vol);
    volClean = vol;

    % Basic validity
    keepMask = isfinite(volClean) & (volClean > 0);

    medRow = nan(nT,1);
    madRow = nan(nT,1);
    keptRow = zeros(nT,1);

    for i = 1:nT
        rowMask = keepMask(i,:);
        row = volClean(i, rowMask);

        if numel(row) < minPoints
            keptRow(i) = sum(rowMask);
            continue;
        end

        med = median(row, 'omitnan');
        madVal = median(abs(row - med), 'omitnan');

        medRow(i) = med;
        madRow(i) = madVal;

        if ~isfinite(madVal) || madVal <= 0
            keptRow(i) = sum(rowMask);
            continue;
        end

        fullRow = volClean(i,:);
        isOut = false(1,nK);
        isOut(rowMask) = abs(fullRow(rowMask) - med) > Kmad * madVal;

        keepMask(i, isOut) = false;
        keptRow(i) = sum(keepMask(i,:));
    end

    volClean(~keepMask) = NaN;

    stats = struct();
    stats.medianVolPerTenor = medRow;
    stats.madPerTenor = madRow;
    stats.keptPerTenor = keptRow;
    stats.totalKept = sum(keepMask(:));
    stats.totalPoints = numel(vol);
    stats.totalRemoved = stats.totalPoints - stats.totalKept;
end
