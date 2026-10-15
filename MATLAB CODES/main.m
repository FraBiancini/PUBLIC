clear all
close all
clc

%% Read Prices

[prices, dates] = read_data('asset_prices.csv');
weights = read_weights('capitalization_weights');
[macroGroups, assetNames] = read_map('mapping_table.csv');



idx_start = find(dates == '2018-01-02');
idx_end = find(dates == '2022-12-30');
in_sample_dates = dates(idx_start:idx_end);
out_sample_dates = dates(idx_end+1:end);
in_sample_prices= prices(idx_start:idx_end,:);
out_sample_prices = prices(idx_end+1:end,:);

% in sample measures (Expected Returns and Covariance Matrix)

m = measures(prices(idx_start:idx_end,:),"Continuous");

n_assets = size(m.returns, 2);
n_views=3;


%% Preliminary analysis

t = tiledlayout(4,4);
for i = 1:16
    nexttile
    plot(dates,prices(:,i))
    title("Asset ",i)
end

figure
N = 64;
cmap = [linspace(0,0,N/2)' linspace(0,1,N/2)' ones(N/2,1); ...
        ones(N/2,1) linspace(1,0,N/2)' linspace(1,0,N/2)'];
h = heatmap(m.corr_mat);
colormap(cmap)
clim([-1 1])
h.Title = 'Correlation Matrix';
h.ColorbarVisible = 'on';


%% EXERCISE 1

%% a) Efficient frontier, Minimum Variance Portfolio and Maximum Sharpe Ratio Portfolio  

[w_MVP , w_sh, FrontierRet, FrontierVola] = Ex1_Efficient_Frontier(m,macroGroups);

figure
plot(FrontierVola.^2,FrontierRet)

% Built-in method
NumAssets=16;
lb = zeros(1,NumAssets);               % lower bound
ub = 0.3*ones(1,NumAssets);                % upper bound
def_ass=strcmp(macroGroups, 'Defensive')';
neu_ass=strcmp(macroGroups, 'Neutral')';
cyc_ass=strcmp(macroGroups, 'Cyclical')';
A=[def_ass;-neu_ass];% constraints on defensive and neutral assets
b=[0.45;-0.20];% constraints on defensive and neutral assets


p = Portfolio('AssetList', assetNames);      
p = setDefaultConstraints(p);
p.UpperBound = ub;
p = setInequality(p, A, b);
p = setAssetMoments(p, m.expected_returns, m.Cov_mat);

MVP_built_in=p.estimateFrontierLimits('min');
SH_built_in=estimateMaxSharpeRatio(p);


%% b) Robust Efficient frontier, Minimum Variance Portfolio and Maximum Sharpe Ratio Portfolio

[w_MVP_Rob, w_sh_Rob, meanRet, meanRisk, RetPtfSim, RiskPtfSim] = Ex1_Robust_Frontier(m,macroGroups,assetNames);

% Plot results
figure; hold on;
% Plot mean frontier
plot(meanRisk, meanRet, 'r','LineWidth',3);
% Plot all simulated frontiers in light grey
plot(RiskPtfSim, RetPtfSim, 'Color',[0.8 0.8 0.8]);
% Plot single average-weights portfolio
xlim([-0.01, 0.1])
xlabel('Volatility');
ylabel('Expected Return');
title('Resampled Efficient Frontier with 90% Confidence Interval');
legend('Mean Frontier','Simulated Frontiers','Location','best');
grid on;

%% Table of Results

Ann_Ret = @(w) w'*m.expected_returns' .*252;
Volatility = @(w) sqrt(252.*w'*m.Cov_mat*w);
SR = @(w) Ann_Ret(w)./ Volatility(w);

Metrics = [Ann_Ret(w_MVP),Volatility(w_MVP),SR(w_MVP);
    Ann_Ret(w_sh),Volatility(w_sh),SR(w_sh);
    Ann_Ret(w_MVP_Rob),Volatility(w_MVP_Rob),SR(w_MVP_Rob);
    Ann_Ret(w_sh_Rob),Volatility(w_sh_Rob),SR(w_sh_Rob);];

RowNames = {'Portfolio A (Nominal MVP)', 'Portfolio B (Nominal MRSP)', 'Portfolio C (Robust MVP)', 'Portfolio B (Robust MRSP)'};
VarNames = {'Annual Returns', 'Annual Volatility', 'Sharpe Ratio'};

ResultsTable = table(Metrics(:,1), Metrics(:,2), Metrics(:,3), ...
    'RowNames', RowNames, 'VariableNames', VarNames);

disp('---------- Nominal / Robust Comparison ----------');
disp(ResultsTable);

%% EXERCISE 2

%% a) equilibrium returns

m = measures(prices(idx_start:idx_end,:),"none");
lambda=1.2; %risk aversion coefficient

[mu_mkt, quarter_calmar,tau]=Ex2_Equilibrium_Return(m,weights,lambda);

% Plot prior distribution
X_prior = mvnrnd(mu_mkt, quarter_calmar, 200);
figure;
histogram(X_prior);
title('Prior Distribution of Returns (Equilibrium)');
%% b) Building the views

[P,q,Omega] = Ex2_Build_Views(m,macroGroups,assetNames,tau);

% Plot views distribution
X_views = mvnrnd(q, Omega, 200);
figure;
hold on
for i = 1:n_views
    histogram(X_views(:,i), 'DisplayName', ['View ' num2str(i)], 'FaceAlpha',0.5);
end
legend
title('Distribution of Views')
hold off

%% c) Posterior expected returns and plot frontiers


muBL = (quarter_calmar\eye(n_assets) + P'/Omega*P) \ (P'/Omega*q + quarter_calmar\mu_mkt); %posterior expected returns
covBL = inv(P'/Omega*P + inv(quarter_calmar));
daysPerYear=252;

% Compare prior vs BL
TBL = table(assetNames, mu_mkt*daysPerYear, muBL*daysPerYear, ...
    'VariableNames', ["Asset","PriorReturnAnnual","BLReturnAnnual"]);
disp(TBL)


% Portfolio Prior (equilibrium)
port_prior = Portfolio('NumAssets', n_assets, 'Name', 'Prior Frontier');
port_prior = setDefaultConstraints(port_prior);
port_prior = setAssetMoments(port_prior, mu_mkt, m.Cov_mat);

pwgt_prior = estimateFrontier(port_prior, 100);
[pf_vola_prior, pf_ret_prior] = estimatePortMoments(port_prior, pwgt_prior);

ptf_prior_min = port_prior.estimateFrontierLimits("min");
ptf_prior_sharpe = estimateMaxSharpeRatio(port_prior);

% Portfolio Black-Litterman (posterior)
portBL = Portfolio('NumAssets', n_assets, 'Name', 'BL Frontier');
portBL = setDefaultConstraints(portBL);
portBL = setAssetMoments(portBL, muBL, m.Cov_mat+covBL);

pwgt_BL = estimateFrontier(portBL, 100);
[pf_vola_BL, pf_ret_BL] = estimatePortMoments(portBL, pwgt_BL);

ptf_BL_min = portBL.estimateFrontierLimits("min");
ptf_BL_sharpe = estimateMaxSharpeRatio(portBL);

% Plot both frontiers together
figure;
plotFrontier(port_prior, 50);   % disegna la frontiera prior
hold on;
plotFrontier(portBL, 50);       % disegna la frontiera BL
hold off;

legend('Prior Frontier','BL Frontier','Location','best');
xlabel('Volatility');
ylabel('Expected Return');
title('Efficient Frontiers: Prior vs Black-Litterman');
grid on;



%% e) Impact of views on portfolio (Delta weights)


% Analysis of contribution of each view
[muBL_contrib,contrib]=Ex2_Views_Effect(m,mu_mkt,P,q,Omega);

% Plot contributions
figure;
bar(contrib * daysPerYear); %Each color (bar) corresponds to a different view
xlabel('Asset Index'); %Represents the assets
ylabel('Annualized Contribution'); % shows the annualized contribution (how much the expected return is shifted by that view)
title('Contribution of Each View to BL Expected Returns');
legend("View1","View2","View3");
% A positive bar → the view increases that asset's expected return
% A negative bar → the view decreases it


delta_weights_min = ptf_prior_min - ptf_BL_min;
delta_weights_sharpe = ptf_prior_sharpe - ptf_BL_sharpe;


figure;
bar(delta_weights_sharpe);
xlabel('Asset Index'); ylabel('Change in Weight');
title('Impact of Views on Portfolio Allocation (Sharpe)');
% Plot Distribution
% NB: Even a view on a single asset affects other assets through the covariance matrix: 
% the Black–Litterman model propagates the impact according to cross-asset correlations. 
% Positively correlated assets see their expected returns increase, 
% while negatively correlated ones decrease



    %% EXERCISE 3

%% a) Portfolio G and H

m = measures(prices(idx_start:idx_end,:),"Continuous");
[w_Entropy,w_DR] = Ex3_Diversification_Based_Optimization(m,macroGroups,assetNames);


%% EXERCISE 4

m = measures(in_sample_prices,"Continuous");

log_ret = m.returns;
mu_r = m.expected_returns;
sigma_r = std(log_ret);
cov_sample = m.Cov_mat;
n_assets = size(log_ret,2);

%% a) Identify how many components explain at least 80% of total variance

% Standardize returns
ret_std = (m.returns - m.expected_returns) ./ sigma_r;

% PCA decomposition
[factor_loading, factor_retn, latent, ~, explained] = pca(ret_std);
cumulative_explained = cumsum(explained);

% Find number of components explaining at least 80% of variance
k = find(cumulative_explained >= 80, 1);

fprintf('Number of components explaining at least 80%% of variance: %d\n', k);
fprintf('Cumulative variance explained by first %d components: %.2f%%\n', k, cumulative_explained(k));

% Plot explained variance
figure;
subplot(1,2,1);
bar(explained(1:min(10,length(explained))));
title('Variance Explained by Each Principal Component');
xlabel('Principal Component');
ylabel('Explained Variance (%)');
grid on;

subplot(1,2,2);
plot(1:length(cumulative_explained), cumulative_explained, 'b-', 'LineWidth', 2);
hold on;
plot(1:length(cumulative_explained), 80*ones(size(cumulative_explained)), 'r--', 'LineWidth', 1);
scatter(k, cumulative_explained(k), 100, 'ro', 'filled');
xlabel('Number of Principal Components');
ylabel('Cumulative Explained Variance (%)');
title('Cumulative Explained Variance');
legend('Cumulative Variance', '80% Threshold', 'Selected k', 'Location', 'Southeast');
grid on;

new_factor_loading = factor_loading(:,1:k);
new_factor_retn = factor_retn(:,1:k);
new_covar_factor = cov(new_factor_retn);

% Rescale back to original return units
lambda = diag(sigma_r);
D_std = diag(var(log_ret - (new_factor_retn*new_factor_loading' .* sigma_r + mu_r))); % diagonal matrix that contains std of original assets returns
covar_PCA = lambda * (new_factor_loading * new_covar_factor * new_factor_loading' + D_std) * lambda;
recon_return = new_factor_retn * new_factor_loading' .* sigma_r + mu_r;
unexplained_retn = log_ret - recon_return; % epsilon
muR=mean(recon_return);
% Explained variance
explained_Var = latent(1:k) / sum(latent);
figure;
bar(explained_Var*100);
title('Variance explained by each Principal Component');
xlabel('Principal Component');
ylabel('Explained Variance (%)');



%% b) 
% portfolio I 

[w_sharpe_PCA, fval_sharpe_PCA] = Ex4_Portfolio_PCA(m,covar_PCA,muR);


% portfolio J
conf_level = 0.95;
alpha = 0.05;
target_vol_annual = 0.10;

[w_CVar] = Ex4_Portfolio_CVar(m, alpha);
Ex4_analyze_PCA_CVaR_portfolios(w_sharpe_PCA, w_CVar, log_ret, covar_PCA, cov_sample, in_sample_dates, alpha);

%% EXERCISE 5

%% a-b) Our strategy

m = measures(in_sample_prices,"Continuous"); 

w_eq=ones(size(in_sample_prices,2),1)/size(in_sample_prices,2);

[ptf_mon,mon_dates] = Ex5_Monthly_Readjusted_PTF(in_sample_dates,in_sample_prices, w_eq);

[rt_mon,sigma_mon,Calmar_ratio,sh_rt_mon,mon_drawdown] = Ex5_Time_Evaluation_PTF(m,in_sample_dates,in_sample_prices, ptf_mon);

%% c) Plot

figure
plot(mon_dates,sh_rt_mon)
title("Monthly Sharpe ratio")

figure
plot(mon_dates,mon_drawdown)
title("Monthly drawdown")

%%
w_mix = ptf_mon(:,end); %placeholder

%% Final Analysis



Portfolios_Matrix=[w_eq, w_MVP , w_sh, w_MVP_Rob , w_sh_Rob, ptf_BL_min, ptf_BL_sharpe, w_Entropy,w_DR, w_sharpe_PCA, w_CVar, w_mix];


portfolio_names = {'EQ','MVP', 'SH', 'MVP_ROB', 'SH_ROB', 'BL_MVP', 'BL_SH', 'ENTROPY', 'DR', 'SHARPE_PCA', 'CVAR','MIX'};
for i = 1:length(portfolio_names)
    portfolio_weights.(['Portfolio_' (portfolio_names{i})]) = Portfolios_Matrix(:,i);
end


temp = measures(out_sample_prices, "none"); 
dim = size(temp.returns,1);

risk_metrics = ANA_calculate_risk_metrics(portfolio_weights,out_sample_prices);

performance_metrics = ANA_calculate_performance_metrics(portfolio_weights,out_sample_prices);

robustness_metrics = ANA_calculate_robustness_metrics(portfolio_weights,out_sample_prices,dim);

ANA_create_performance_plots(portfolio_weights, out_sample_dates(1:end-1),out_sample_prices);

ANA_generate_comparative_analysis(performance_metrics, risk_metrics, robustness_metrics);



%%


Portfolios_Matrix=[w_eq, w_MVP , w_sh, w_MVP_Rob , w_sh_Rob, ptf_BL_min, ptf_BL_sharpe, w_Entropy,w_DR, w_sharpe_PCA, w_CVar];


portfolio_names = {'EQ','MVP', 'SH', 'MVP_ROB', 'SH_ROB', 'BL_MVP', 'BL_SH', 'ENTROPY', 'DR', 'SHARPE_PCA', 'CVAR','MIX'};

[mon_returns, mon_vol, mon_calm, mon_shar, mon_drawdown, mon_dates] = ...
    Ex5_Time_Evaluation_PTF_ALL(in_sample_dates, in_sample_prices, Portfolios_Matrix, ptf_mon, portfolio_names);



% comparative plots

n_assets = 16;
base_colors = lines(7);
colors = interp1(1:7, base_colors, linspace(1, 7, n_assets + 1));

% 1. Sharpe ratio plot
figure();
hold on;
for i = 1:size(mon_shar, 2)
    if i < length(portfolio_names)
        plot(mon_dates, mon_shar(:, i), 'LineWidth', 2, ...
            'DisplayName', portfolio_names{i}, 'Color', colors(i, :));
    else
        plot(mon_dates, mon_shar(:, i), 'LineWidth', 3, ...
            'DisplayName', 'MIX', 'Color', 'k', 'LineStyle', '--');
    end
end

title('Monthly Sharpe Ratio');
xlabel('Date');
ylabel('Sharpe Ratio');
legend('Location', 'bestoutside', 'FontSize', 8,'Interpreter','none');
grid on;
datetick('x', 'QQ-YYYY', 'keeplimits');

% 2. Drawdown plot
figure();
hold on;
for i = 1:size(mon_drawdown, 2)
    if i < length(portfolio_names)
        plot(mon_dates, mon_drawdown(:, i), 'LineWidth', 2, ...
            'DisplayName', portfolio_names{i}, 'Color', colors(i, :));
    else
        plot(mon_dates, mon_drawdown(:, i), 'LineWidth', 3, ...
            'DisplayName', 'MIX', 'Color', 'k', 'LineStyle', '--');
    end
end
title('Monthly Drawdown (%)');
xlabel('Date');
ylabel('Drawdown (%)');
legend('Location', 'bestoutside', 'FontSize', 8,'Interpreter','none');
grid on;
datetick('x', 'QQ-YYYY', 'keeplimits');

% 3. Annualized returns plot
figure();
hold on;
for i = 1:size(mon_returns, 2)
    if i < length(portfolio_names)
        plot(mon_dates, mon_returns(:, i) * 252, 'LineWidth', 2, ...
            'DisplayName', portfolio_names{i}, 'Color', colors(i, :));
    else
        plot(mon_dates, mon_returns(:, i) * 252, 'LineWidth', 3, ...
            'DisplayName', 'MIX', 'Color', 'k', 'LineStyle', '--');
    end
end
title('Monthly Annualized Returns');
xlabel('Date');
ylabel('Annualized Return');
legend('Location', 'bestoutside', 'FontSize', 8,'Interpreter','none');
grid on;
datetick('x', 'QQ-YYYY', 'keeplimits');


% 4. Calmar ratio plot
figure();
hold on;
for i = 1:size(mon_calm, 2)
    if i < length(portfolio_names)
        plot(mon_dates, mon_calm(:, i), 'LineWidth', 2, ...
            'DisplayName', portfolio_names{i}, 'Color', colors(i, :));
    else
        plot(mon_dates, mon_calm(:, i), 'LineWidth', 3, ...
            'DisplayName', 'MIX', 'Color', 'k', 'LineStyle', '--');
    end
end
title('Monthly Calmar Ratio');
xlabel('Date');
ylabel('Calmar Ratio');
legend('Location', 'bestoutside', 'FontSize', 8,'Interpreter','none');
grid on;
datetick('x', 'QQ-YYYY', 'keeplimits');


% 4. vol plot
figure();
hold on;
mon_vol_annualized = mon_vol * sqrt(252); % Converti a annualizzata
for i = 1:size(mon_vol_annualized, 2)
    if i < length(portfolio_names)
        plot(mon_dates, mon_vol_annualized(:, i) * 100, 'LineWidth', 2, ...
            'DisplayName', portfolio_names{i}, 'Color', colors(i, :));
    else
        plot(mon_dates, mon_vol_annualized(:, i) * 100, 'LineWidth', 3, ...
            'DisplayName', 'MIX', 'Color', 'k', 'LineStyle', '--');
    end
end

title('Monthly Annualized Volatility');
xlabel('Date');
ylabel('Annualized Volatility (%)');
legend('Location', 'bestoutside', 'FontSize', 8,'Interpreter','none');
grid on;
datetick('x', 'QQ-YYYY', 'keeplimits');




% 4. Performance mean 
fprintf('\n=== Monthly PERFORMANCE SUMMARY (IN-SAMPLE) ===\n');
fprintf('%-15s %-12s %-12s %-12s %-12s %-12s\n', ...
    'Portfolio', 'Avg Sharpe', 'Avg Return', 'Avg Vol', 'Avg Drawdown', 'Avg Calmar');
fprintf('%-15s %-12s %-12s %-12s %-12s %-12s\n', ...
    '--------', '----------', '----------', '--------', '------------', '----------');

for i = 1:size(mon_shar, 2)
    if i < length(portfolio_names)
        port_name = portfolio_names{i};
    else
        port_name = 'MIX';
    end
    
    avg_sharpe = mean(mon_shar(:, i));
    avg_return = mean(mon_returns(:, i)) * 252;
    avg_vol = mean(mon_vol(:, i)) * sqrt(252);
    avg_drawdown = mean(mon_drawdown(:, i));
    avg_calmar = mean(mon_calm(:, i));
    
    fprintf('%-15s %-12.3f %-12.2f %-12.2f %-12.2f %-12.3f\n', ...
        port_name, avg_sharpe, avg_return, avg_vol, avg_drawdown, avg_calmar);
end






%% Macro Exposure Analysis

% Calculate macro exposures
macro_exposures = ANA_compute_macro_exposures(portfolio_weights, macroGroups);

% Analyze impact of macro exposures
ANA_analyze_macro_impact(macro_exposures, performance_metrics, risk_metrics);

% Extract exposure data for additional analysis
portfolio_names = fieldnames(portfolio_weights);
n_portfolios = length(portfolio_names);

% Initialize arrays
cyclical = zeros(n_portfolios, 1);
neutral = zeros(n_portfolios, 1);
defensive = zeros(n_portfolios, 1);
sharpe_ratios = zeros(n_portfolios, 1);
max_drawdowns = zeros(n_portfolios, 1);
annual_returns = zeros(n_portfolios, 1);
annual_volatilities = zeros(n_portfolios, 1);

% Populate arrays
for i = 1:n_portfolios
    port_name = portfolio_names{i};
    
    cyclical(i) = macro_exposures.(port_name).Cyclical;
    neutral(i) = macro_exposures.(port_name).Neutral;
    defensive(i) = macro_exposures.(port_name).Defensive;
    sharpe_ratios(i) = risk_metrics.(port_name).SharpeRatio;
    max_drawdowns(i) = risk_metrics.(port_name).MaxDrawdown;
    annual_returns(i) = performance_metrics.(port_name).AnnualReturn;
    annual_volatilities(i) = performance_metrics.(port_name).AnnualVolatility;
end

% Create additional analysis for final report
fprintf('\n=== FINAL MACRO EXPOSURE INSIGHTS ===\n\n');

% Display names for better readability
display_names = containers.Map;
display_names('Portfolio_EQ') = 'EQ';
display_names('Portfolio_MVP') = 'MVP';
display_names('Portfolio_SH') = 'Max Sharpe';
display_names('Portfolio_MVP_ROB') = 'Robust MVP';
display_names('Portfolio_SH_ROB') = 'Robust Sharpe';
display_names('Portfolio_BL_MVP') = 'BL Min Var';
display_names('Portfolio_BL_SH') = 'BL Max Sharpe';
display_names('Portfolio_ENTROPY') = 'Entropy';
display_names('Portfolio_DR') = 'DR';
display_names('Portfolio_SHARPE_PCA') = 'PCA Sharpe';
display_names('Portfolio_CVAR') = 'CVaR';
display_names('Portfolio_MIX') = 'MIX';

% Best portfolios for different market regimes
fprintf('\n=== PORTFOLIO RECOMMENDATIONS BY REGIME ===\n');

% Sort by defensive exposure (high to low)
[defensive_sorted, idx_def] = sort(defensive, 'descend');

fprintf('\nFor Defensive/Bearish markets (High Defensive Exposure):\n');
for i = 1:min(3, n_portfolios)
    port_name = portfolio_names{idx_def(i)};
    if isKey(display_names, port_name)
        port_display = display_names(port_name);
    else
        port_display = port_name;
    end
    fprintf('   %d. %s (%.1f%% defensive, Sharpe: %.3f, MaxDD: %.2f%%)\n', ...
        i, port_display, defensive_sorted(i)*100, ...
        sharpe_ratios(idx_def(i)), max_drawdowns(idx_def(i))*100);
end

% Sort by cyclical exposure (high to low)
[cyclical_sorted, idx_cyc] = sort(cyclical, 'descend');

fprintf('\nFor Cyclical/Bullish markets (High Cyclical Exposure):\n');
for i = 1:min(3, n_portfolios)
    port_name = portfolio_names{idx_cyc(i)};
    if isKey(display_names, port_name)
        port_display = display_names(port_name);
    else
        port_display = port_name;
    end
    fprintf('   %d. %s (%.1f%% cyclical, Sharpe: %.3f, MaxDD: %.2f%%)\n', ...
        i, port_display, cyclical_sorted(i)*100, ...
        sharpe_ratios(idx_cyc(i)), max_drawdowns(idx_cyc(i))*100);
end

% Balanced portfolios (closest to equal weight in each group)
balanced_score = abs(cyclical - 1/3) + abs(neutral - 1/3) + abs(defensive - 1/3);
[~, idx_bal] = sort(balanced_score, 'ascend');

fprintf('\nFor Balanced/Uncertain markets (Most Balanced Exposure):\n');
for i = 1:min(3, n_portfolios)
    port_name = portfolio_names{idx_bal(i)};
    if isKey(display_names, port_name)
        port_display = display_names(port_name);
    else
        port_display = port_name;
    end
    fprintf('   %d. %s (C:%.1f%%, N:%.1f%%, D:%.1f%%, Sharpe: %.3f)\n', ...
        i, port_display, ...
        cyclical(idx_bal(i))*100, neutral(idx_bal(i))*100, defensive(idx_bal(i))*100, ...
        sharpe_ratios(idx_bal(i)));
end

% Correlation analysis
fprintf('\n=== CORRELATION ANALYSIS ===\n');
corr_cyc_sharpe = corr(cyclical, sharpe_ratios);
corr_def_sharpe = corr(defensive, sharpe_ratios);
corr_cyc_dd = corr(cyclical, max_drawdowns);
corr_def_dd = corr(defensive, max_drawdowns);

fprintf('Cyclical Exposure vs Sharpe Ratio: %.3f\n', corr_cyc_sharpe);
fprintf('Defensive Exposure vs Sharpe Ratio: %.3f\n', corr_def_sharpe);
fprintf('Cyclical Exposure vs Max Drawdown: %.3f\n', corr_cyc_dd);
fprintf('Defensive Exposure vs Max Drawdown: %.3f\n', corr_def_dd);

% Create summary table for report
fprintf('\n=== SUMMARY TABLE FOR REPORT ===\n');
fprintf('\nPortfolio      | Cyclical%% | Neutral%% | Defensive%% | Sharpe | MaxDD%%\n');
fprintf('---------------|-----------|----------|------------|--------|--------\n');

for i = 1:n_portfolios
    port_name = portfolio_names{i};
    if isKey(display_names, port_name)
        port_display = display_names(port_name);
    else
        port_display = port_name;
    end
    
    fprintf('%-15s| %9.1f | %8.1f | %10.1f | %6.3f | %6.2f\n', ...
        port_display, ...
        cyclical(i)*100, neutral(i)*100, defensive(i)*100, ...
        sharpe_ratios(i), max_drawdowns(i)*100);
end


ANA_create_macro_plots(macro_exposures, sharpe_ratios, max_drawdowns, display_names)