clc;
clearvars;

% Import data from the Excel file
filename = 'MarketData_12.xlsx';

% Expiries (T)
T = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B6:H6');

% Implied Volatilities (MktVol)
MktVol = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B14:H18');

% Spot price
spot = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B4:B4');

% Discount factors (domestic and foreign)
disc_fact = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B7:H7');
disc_fact_for = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B8:H8');

% Forward prices calculation
fwd = spot .* disc_fact_for ./ disc_fact;

% Delta values to find corresponding strikes
Delta = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'A14:A18');  

% Initialize matrix for strikes (K)
K = zeros(length(Delta), size(T, 2));  % For each maturity and delta

% Loop over each expiry and calculate K for the given Delta
for j = 1:size(T, 2)
    for i = 1:length(Delta)
        % We use fzero to find K such that blsdelta(K, fwd, T(j), MktVol(i, j)) = Delta(i)
        K(i, j) = fzero(@(Strike) blsdelta(fwd(j), Strike, 0, T(j), MktVol(i, j)) - (1 - Delta(i) / 100), fwd(j));
    end
end

% normalized market strikes
[rows, cols] = size(K);
K_norm = K ./ repmat(fwd, rows, 1);

% Dupire solver settings
Lt = 20;
Lh = 300;
K_min = 0.5;
K_max = 2.5;
Scheme = 'cn';

% calibration settings
Threshold = 0.001;
MaxIter = 100;

[V, ModelVol, MaxErr] = calibrator(T,K_norm,MktVol,Threshold,MaxIter,Lt,Lh,K_min,K_max,Scheme);

% plot local volatility function vs market implied volatility
figure;
plot(K(:,1),MktVol(:,1),'o',K(:,1),ModelVol(:,1),':.',K(:,1),V(:,1),':.b','linewidth',1.5);
title('Calibrated model and local volatility for asset EUR/USD');
legend('MktVol','ModelVol','LocalVol');

figure;
plot(MaxErr,'.','MarkerSize',15);
title('calibration error at each iteration of the fixed-point calibration');
save('CalibratedLVModel.mat', 'V', 'T', 'K', 'MktVol', 'spot', 'disc_fact', 'disc_fact_for', 'fwd');



