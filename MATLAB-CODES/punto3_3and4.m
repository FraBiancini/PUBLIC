clc;
clearvars;
filename = 'MarketData_12.xlsx';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DATI DI MERCATO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dati forniti
T = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B6:H6'); % Scadenze
Spot = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B4:B4'); % Prezzo spot
disc_fact = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B7:H7'); % Fattori di sconto domestici
disc_fact_for = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B8:H8');% Fattori di sconto esteri
Fwd = Spot .* disc_fact_for ./ disc_fact;
% Implied volatilities di mercato
MktVol = readmatrix(filename, 'Sheet', 'EURUSD', 'Range', 'B14:H18');
% Delta per strike (indice della 25-Delta volatility)
delta_idx = 2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALCOLO DEI PARAMETRI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scadenza target (T5) e volatilità di mercato
expiry_idx = 5; % Indice della scadenza T5
expiry = T(expiry_idx);
strike_delta_25 = Fwd(expiry_idx) * exp(-MktVol(delta_idx, expiry_idx)^2 / 2 * expiry); % Approssimazione strike

% Calcolo r e q
r = -log(disc_fact) ./ T; % Tassi domestici
q = -log(disc_fact_for) ./ T; % Tassi esteri

% Volatilità di mercato per 25-Delta
sigma = MktVol(delta_idx, expiry_idx);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SIMULAZIONE MONTE CARLO (DINAMICA DI BLACK)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N = 100000; % Numero di simulazioni

% Generazione di percorsi Monte Carlo per la dinamica di Black
dt = expiry / N; % Passo temporale
S_B = zeros(1, N);

% Simulazione con incremento geometrico browniano
for i = 1:N
    Z = randn; % Incremento normale standard
    S_B(i) = Spot * exp((r(expiry_idx) - q(expiry_idx) - 0.5 * sigma^2) * expiry + sigma * sqrt(expiry) * Z);
end

% Calcolo payoff per l'opzione
payoff = max(S_B - strike_delta_25, 0);

% Prezzo dell'opzione
discount_factor = exp(-r(expiry_idx) * expiry); % Fattore di sconto
price_black = discount_factor * mean(payoff);

% Calcolo intervallo di confidenza al 95%
sample_variance = var(payoff);
confidence_interval = 1.96 * sqrt(sample_variance / N); % 1.96 è il quantile della normale standard
lower_bound = price_black - confidence_interval;
upper_bound = price_black + confidence_interval;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT DEI RISULTATI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('Monte Carlo Price (Black Dynamics): %.6f\n', price_black);
fprintf('95%% Confidence Interval: [%.6f, %.6f]\n', lower_bound, upper_bound);
count=0;
    
count = sum(S_B > strike_delta_25);

percent=count./sum(S_B >= 0);
fprintf('percentage of times the derivative has a payoff>0: %.6f\n', percent);
payoff_medio=sum(max(0,S_B - strike_delta_25))./count;
fprintf('avarage payoff: %.6f\n', payoff_medio);
% ex:3_4
% The confidence intervals for prices calculated using the Black Dynamics model 
% and the local volatility model do not overlap. Specifically, the 95% confidence interval 
% for the Black Dynamics price ranges from 3.584294 to 3.651075, while for the local volatility 
% model it ranges from 7.273054 to 7.350676. The lack of overlap reflects the fundamental 
% differences between the two models and their underlying assumptions. The Black model is based 
% on the principle of constant volatility, which means that it assumes that the volatility of 
% the underlying asset remains uniform regardless of the strike price or the time of expiration. 
% This simplification makes it unsuitable for capturing the “smile or skew” of volatility commonly 
% observed in real market data. In contrast, the local volatility model incorporates the dependence 
% of volatility on both the strike price and expiration, allowing it to reproduce nuanced patterns of 
% implied volatility across a range of market conditions.
% 
% Exotic options, such as the derivative under consideration, often have returns that are very 
% sensitive to volatility changes over time and at different price levels. Because the local 
% volatility model is calibrated to reflect the entire surface of implied market volatilities, 
% it provides a more accurate representation of risk and price dynamics. This results in higher 
% prices for options whose valuation is highly dependent on market perception of risk, particularly 
% in the tails of the distribution. However, the Black model, by reducing volatility to a single constant 
% parameter, tends to undervalue these options.
%Indeed,the average payoff in this model is 7.221830±0.1 while in the previous one was 9.033634±0.1,and
%the Black-Scholes model resulted in a much lower percentage of times the derivative had 
%a positive payoff (49.71%), compared to the local volatility model(75.77%), which clarifies the reason 
% why the estimate of the price are so far apart.
% 
% The divergence in results also highlights the importance of accounting for market 
% perception of risk in pricing. The local volatility model achieves this by reproducing 
% the observed smile and skew effects, while the Black model ignores these complexities. 
% For this reason, the prices generated by the two models are expected to differ significantly, 
% especially for instruments with path-dependent or exotic payoffs. Ultimately, the non-overlapping 
% ranges highlight that the local volatility model provides a more complete and realistic framework 
% for pricing options in markets with pronounced volatility patterns.






