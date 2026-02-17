%% 1. Initialization and Configuration
clear; close all; clc;

%%


today = datetime('2017-12-08','InputFormat','yyyy-MM-dd');
cutoffDate = datetime('2017-12-16','InputFormat','yyyy-MM-dd');

[B_today, Fwd_today, all_strikes, C_mat, P_mat, Spot_today, mkt_today, T] = loadMarket(today, cutoffDate);
price_handles = makeNIG_handles_per_expiry(T, today, Fwd_today, B_today, 1/2, all_strikes, 16, 0.001);

%% calibrate NIG model
theta0 = [0.2, 0.5, 1.0];
thetaNIG_hat = calibrateNIG(T, today, Fwd_today, B_today, C_mat, P_mat, all_strikes,theta0,price_handles);


%% plot IV smiles NIG vs MKT

idx_plot_NIG = [4,5,8,9];
plotVol(idx_plot_NIG, T, today, Fwd_today, B_today, all_strikes, C_mat,P_mat, price_handles, thetaNIG_hat, 'NIG');

%% Calibrate power scaling ATS model
theta0   = [0.1, 16, 1.5, 0.3, -0.0001]; 
thetaATS_hat = calibrateATS(T, today, Fwd_today, B_today, all_strikes, C_mat, P_mat, price_handles,theta0);


%% plot IV ATS  

idx_plot_ATS = 1:numel(all_strikes);
VolSurf_idxplot = plotVol(idx_plot_ATS, T, today, Fwd_today, B_today, all_strikes, C_mat,P_mat, price_handles, thetaATS_hat, 'ATS');

%% check that mc converges to lewis' price

Lewis_MC(today,all_strikes,T,thetaATS_hat,price_handles,Fwd_today,B_today)

%% MAPE e RMSE vol surfaces

% Errori NIG
[rmseNIG, mapeNIG] = errorMetricsVolSurface(thetaNIG_hat, price_handles, C_mat, P_mat,all_strikes, Fwd_today,B_today, T, today, 'NIG');

% Errori ATS
[rmseATS, mapeATS] = errorMetricsVolSurface(thetaATS_hat, price_handles, C_mat,P_mat, all_strikes, Fwd_today,B_today, T, today, 'ATS');

fprintf('NIG -> RMSE: %.4f, MAPE: %.2f%%\n', rmseNIG, mapeNIG);
fprintf('ATS -> RMSE: %.4f, MAPE: %.2f%%\n', rmseATS, mapeATS); 

%% pricing Miami Crocodile Certificate
Notional = 15e6;
Nsim = 5e6;
rng(357);
[price_MiamiCroco, CI_low, CI_high] = MiamiCrocodilePrice(thetaATS_hat, today, T, B_today,Fwd_today,Spot_today, Nsim,1,'ATS');

% Stampa risultato
fprintf('Prezzo normalizzato certificato Miami Crocodile = %.5f USD.\n', price_MiamiCroco/Notional);
fprintf('Intervallo di confidenza normalizzato al 99%%: [%.5f , %.5f] USD.\n', CI_low/Notional, CI_high/Notional);



%%
present_dates = [

    
    datetime('2017-12-08', 'InputFormat', 'yyyy-MM-dd');
    datetime('2017-12-11', 'InputFormat', 'yyyy-MM-dd');
    datetime('2017-12-12', 'InputFormat', 'yyyy-MM-dd');
    datetime('2017-12-13', 'InputFormat', 'yyyy-MM-dd');
    
];


%%
[daily_pnl_v,delta_cert] = vega_hedge_until(present_dates);


%%


hedging_error = pnl_metric(daily_pnl_v);
fprintf('The mean squared error of the daily PnL is %.3e\n', hedging_error);


%%
v = abs(daily_pnl_v(end-2:end));
w = abs(delta_cert(end-2:end));


x = 1:length(v);

% Plot dei due vettori
figure;
plot(x, v, '-o', 'LineWidth', 1.5, 'Color', 'b', 'MarkerFaceColor', 'b');
hold on;
plot(x, w, '-s', 'LineWidth', 1.5, 'Color', 'r', 'MarkerFaceColor', 'r');
hold off;

% Etichette e titolo
xlabel('Time (days)');
ylabel('Value');
title('Comparison of |Daily PnL| vs |delta Certificate|');

% Legenda
legend('|Daily PnL|','|Var Certificate|','Location','best');

% Griglia per leggibilità
grid on;


