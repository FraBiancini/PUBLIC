clc;
clearvars;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CARICAMENTO DEL MODELLO CALIBRATO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


load('CalibratedLVModel.mat'); % Carica V, T, K, MktVol, ecc.

% Parametri Monte Carlo
N = 100000; % Numero di simulazioni
M = 100;    % Numero di passi temporali
expiry_idx = 5; % Indice della scadenza T5
expiry = T(expiry_idx); % Tempo di scadenza
strike_delta_25 = K(2, expiry_idx); % Strike corrispondente a 25-Delta

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SIMULAZIONE MONTE CARLO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Calculate q and r
[r,q]=calibrate_r_q(spot,T,disc_fact,fwd);

% Simula percorsi con il modello di volatilità locale
S = lv_simulation_log(T, spot, r, q, V, K, N, M, expiry);

% Calcola il payoff per ogni simulazione
payoff = zeros(1, N);
for i = 1:N
    if S(1, i) < strike_delta_25
        payoff(i) = 2;
    else
        payoff(i) = (S(1, i) - strike_delta_25);
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALCOLO PREZZO E INTERVALLO DI CONFIDENZA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calcola il fattore di sconto
discount_factor = discount(T,r,expiry(1));

% Prezzo stimato
price = discount_factor * mean(payoff);

% Varianza campionaria
sample_variance = var(payoff);

% Intervallo di confidenza al 95%
confidence_interval = 1.96 * sqrt(sample_variance / N);%1.96 è il quantile 0.975 della normale std
lower_bound = price - confidence_interval;
upper_bound = price + confidence_interval;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('Monte Carlo Price: %.6f\n', price);
fprintf('95%% Confidence Interval: [%.6f, %.6f]\n', lower_bound, upper_bound);
count=0;
    
count = sum(S > strike_delta_25);

percent=count./sum(S >= 0);
fprintf('percentage of times the derivative has a payoff>0: %.6f\n', percent);
payoff_medio=sum(max(0,S - strike_delta_25))./count;
fprintf('avarage payoff: %.6f\n', payoff_medio);

