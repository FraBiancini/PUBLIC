filename = 'MarketData_12.xlsx';

% Import "T"
T = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B6:I6');

% Import "Fwd"
Fwd = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B8:I8');

% Import "K"
K = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B24:I30');

% Import "MktVol"
MktVol = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B13:I19');


% normalized market strikes
[rows, cols] = size(K);
K_norm = K ./ repmat(Fwd, rows, 1);

% Dupire solver settings
Lt = 10;
Lh = 200;
K_min = 0.1;
K_max = 3;
Scheme = 'cn';

% calibration settings
Threshold = 0.001;
MaxIter = 100;

[V, ModelVol, MaxErr] = calibrator(T,K_norm,MktVol,Threshold,MaxIter,Lt,Lh,K_min,K_max,Scheme);

% plot local volatility function vs market implied volatility
figure;
plot(K(:,1),MktVol(:,1),'o',K(:,1),ModelVol(:,1),':.',K(:,1),V(:,1),':.b','linewidth',1.5);
title('Calibrated model and local volatility for asset E CORP');
legend('MktVol','ModelVol','LocalVol')

figure;
plot(MaxErr,'.','MarkerSize',15);
title('calibration error at each iteration of the fixed-point calibration');