clc;
clearvars;
filename = 'MarketData_12.xlsx';

% Importare i dati di mercato relativi a "FAIL"
% Expiries (T)
T = readmatrix(filename, 'Sheet', 'FAIL', 'Range', 'B6:C6');

% Forward prices (Fwd)
Fwd = readmatrix(filename, 'Sheet', 'FAIL', 'Range', 'B8:C8');

% Strikes (K)
K = readmatrix(filename, 'Sheet', 'FAIL', 'Range', 'B22:C26');

% Volatilità implicite di mercato (MktVol)
MktVol = readmatrix(filename, 'Sheet', 'FAIL', 'Range', 'B13:C17');

% Prevenzione di divisione per zero nei calcoli degli strike normalizzati
if any(Fwd <= 0)
    error('Errore: Prezzi forward negativi o nulli.');
end


% Normalizzare gli strike rispetto ai prezzi forward
[rows, cols] = size(K);
K_norm = K ./ repmat(Fwd, rows, 1);

% Impostazioni del solver Dupire
Lt = 10;         % Numero di step temporali
Lh = 200;        % Numero di nodi spaziali
K_min = 0.1;     % Limite inferiore degli strike normalizzati
K_max = 3;       % Limite superiore degli strike normalizzati
Scheme = 'cn';   % Metodo di soluzione (Crank-Nicolson)

% Impostazioni di calibrazione
Threshold = 0.001; % Errore massimo tollerato
MaxIter = 100;     % Numero massimo di iterazioni

% [V, ModelVol, MaxErr] = calibrator(T,K_norm,MktVol,Threshold,MaxIter,Lt,Lh,K_min,K_max,Scheme);

price_t1 = blsprice(Fwd(1),K(1,1),0,T(1),MktVol(1,1))
price_t2 = blsprice(Fwd(2),K(1,2),0,T(2),MktVol(1,2))

% Osservare i prezzi delle opzioni di mercato e la loro relazione con gli strike
MarketPrices = zeros(rows, cols);
for i = 1:cols
    for j = 1:rows
        % Calcolo del prezzo di mercato usando la formula di Black
        MarketPrices(j, i) = blsprice(Fwd(i), K(j, i), T(i), MktVol(j, i), 1.0);
    end
end

% Plot dei prezzi di mercato in funzione degli strike per osservare la non convessità
figure;
plot(K(:, 1), MarketPrices(:, 1), 'o-', 'LineWidth', 1.5);
hold on;
plot(K(:, 2), MarketPrices(:, 2), 'o-', 'LineWidth', 1.5);
title('Prezzi di mercato delle opzioni rispetto agli strike (FAIL)');
xlabel('Strike');
ylabel('Prezzo di mercato');
legend('Expiry 18-Dec-19', 'Expiry 22-Dec-19');
grid on;

% The calibration of the local volatility model fails because the market data
% for the asset "FAIL" do not display the required convexity in the price chart
% C_0(T_i, K_{i,j}) as a function of strike prices K_{i,j} for
% fixed time frames T_i. In particular, when calculating the market prices of call options
% using the Black formula, the resulting prices for different strike levels do not
% follow the expected convex pattern.
% 
% In theory, the option prices as a function of strike prices should form
% a convex curve, where prices increase as the strike price moves away from the forward 
% price and then decrease after reaching a certain point. However, the prices in
% this case exhibit irregularities and do not follow this expected convex shape. This
% non-convexity suggests that the data may contain inconsistencies or that
% The model used is not suitable for the market data provided. Since the local volatility 
% model relies on this convex structure to calibrate the volatility surfaces, the lack of 
% convexity leads to a calibration failure.
% 
% The problem therefore lies in the lack of convexity of the market price map, which 
% prevents the calibration procedure from converging to a valid solution.
