function Ex4_analyze_PCA_CVaR_portfolios(w_PCA, w_CVaR, log_ret, covar_PCA, cov_matrix, in_sample_dates, alpha_cvar)
   

    % ANALYZE_PCA_CVaR_PORTFOLIOS - Comparison of PCA and CVaR portfolios
    
    %% Portfolio PCA - Maximum Sharpe with PCA (14% volatility constraint)
    port_returns_PCA = log_ret * w_PCA;
    portfolio_vol_PCA = sqrt(w_PCA' * covar_PCA * w_PCA);
    portfolio_vol_annual_PCA = portfolio_vol_PCA * sqrt(252);
    portfolio_return_PCA = mean(port_returns_PCA);
    portfolio_return_annual_PCA = portfolio_return_PCA * 252;

    % Calculate CVaR for Portfolio PCA
    var_PCA = quantile(port_returns_PCA, alpha_cvar);
    cvar_PCA = mean(port_returns_PCA(port_returns_PCA <= var_PCA));

    % Calculate Maximum Drawdown for Portfolio PCA
    portfolio_values_PCA = cumprod(1 + port_returns_PCA);
    peak_PCA = cummax(portfolio_values_PCA);
    drawdown_PCA = -(portfolio_values_PCA - peak_PCA) ./ peak_PCA;
    max_drawdown_PCA = max(drawdown_PCA);

    fprintf('\n=== PORTFOLIO PCA RESULTS (Max Sharpe PCA - 14%% Volatility) ===\n');
    fprintf('Annual Volatility: %.2f%%\n', portfolio_vol_annual_PCA*100);
    fprintf('Annual Return: %.2f%%\n', portfolio_return_annual_PCA*100);
    fprintf('Sharpe Ratio: %.3f\n', portfolio_return_annual_PCA/portfolio_vol_annual_PCA);
    fprintf('CVaR (5%%): %.4f\n', cvar_PCA);
    fprintf('Maximum Drawdown: %.2f%%\n', max_drawdown_PCA*100);

    %% Portfolio CVaR - Minimum CVaR with 10% annual volatility constraint
    port_returns_CVaR = log_ret * w_CVaR;
    portfolio_vol_CVaR = sqrt(w_CVaR' * cov_matrix * w_CVaR);
    portfolio_vol_annual_CVaR = portfolio_vol_CVaR * sqrt(252);
    portfolio_return_CVaR = mean(port_returns_CVaR);
    portfolio_return_annual_CVaR = portfolio_return_CVaR * 252;

    % Calculate CVaR
    var_CVaR = quantile(port_returns_CVaR, alpha_cvar);
    cvar_CVaR = mean(port_returns_CVaR(port_returns_CVaR <= var_CVaR));

    % Calculate maximum drawdown
    portfolio_values_CVaR = cumprod(1 + port_returns_CVaR);
    peak_CVaR = cummax(portfolio_values_CVaR);
    drawdown_CVaR = -(portfolio_values_CVaR - peak_CVaR) ./ peak_CVaR;
    max_drawdown_CVaR = max(drawdown_CVaR);

    % Display results for Portfolio CVaR
    fprintf('\n=== PORTFOLIO CVaR RESULTS (Minimum CVaR with 10%% Volatility) ===\n');
    fprintf('Annual Volatility: %.2f%%\n', portfolio_vol_annual_CVaR*100);
    fprintf('Annual Return: %.2f%%\n', portfolio_return_annual_CVaR*100);
    fprintf('Sharpe Ratio: %.3f\n', portfolio_return_annual_CVaR/portfolio_vol_annual_CVaR);
    fprintf('CVaR (5%%): %.4f\n', cvar_CVaR);
    fprintf('Maximum Drawdown: %.2f%%\n', max_drawdown_CVaR*100);

    %% Analysis of Results
    fprintf('\n=== COMPARATIVE ANALYSIS ===\n');
    fprintf('1. TAIL RISK (CVaR):\n');
    if cvar_CVaR < cvar_PCA
        fprintf('   Portfolio CVaR has BETTER tail risk protection (lower CVaR)\n');
        fprintf('   Reduction in CVaR: %.4f\n', cvar_PCA - cvar_CVaR);
    else
        fprintf('   Portfolio PCA has BETTER tail risk protection (lower CVaR)\n');
        fprintf('   Increase in CVaR: %.4f\n', cvar_CVaR - cvar_PCA);
    end

    fprintf('\n2. VOLATILITY:\n');
    fprintf('   Portfolio PCA: %.2f%% (constrained at 14%%)\n', portfolio_vol_annual_PCA*100);
    fprintf('   Portfolio CVaR: %.2f%% (constrained at 10%%)\n', portfolio_vol_annual_CVaR*100);
    fprintf('   Difference: %.2f%%\n', (portfolio_vol_annual_CVaR - portfolio_vol_annual_PCA)*100);

    fprintf('\n3. MAXIMUM DRAWDOWN:\n');
    if max_drawdown_CVaR > max_drawdown_PCA
        fprintf('   Portfolio CVaR has WORSE maximum drawdown\n');
        fprintf('   Additional drawdown: %.2f%%\n', (max_drawdown_CVaR - max_drawdown_PCA)*100);
    else
        fprintf('   Portfolio CVaR has BETTER maximum drawdown\n');
        fprintf('   Improvement: %.2f%%\n', (max_drawdown_PCA - max_drawdown_CVaR)*100);
    end

    fprintf('\n4. RISK-RETURN TRADE-OFF:\n');
    sharpe_PCA = portfolio_return_annual_PCA/portfolio_vol_annual_PCA;
    sharpe_CVaR = portfolio_return_annual_CVaR/portfolio_vol_annual_CVaR;
    if sharpe_CVaR > sharpe_PCA
        fprintf('   Portfolio CVaR offers BETTER risk-adjusted returns\n');
    else
        fprintf('   Portfolio PCA offers BETTER risk-adjusted returns\n');
    end

    %% Plot comparison
    figure('Position', [100, 100, 1200, 800]);

    % Equity curves
    subplot(2,2,1);
    plot(in_sample_dates(2:end), portfolio_values_PCA, 'b-', 'LineWidth', 2);
    hold on;
    plot(in_sample_dates(2:end), portfolio_values_CVaR, 'r-', 'LineWidth', 2);
    title('Portfolio Equity Curves');
    xlabel('Date');
    ylabel('Portfolio Value');
    legend('Portfolio PCA (Max Sharpe PCA)', 'Portfolio CVaR (Min CVaR)', 'Location', 'best');
    grid on;

    % Drawdown comparison
    subplot(2,2,2);
    plot(in_sample_dates(2:end), drawdown_PCA*100, 'b-', 'LineWidth', 1.5);
    hold on;
    plot(in_sample_dates(2:end), drawdown_CVaR*100, 'r-', 'LineWidth', 1.5);
    title('Portfolio Drawdowns');
    xlabel('Date');
    ylabel('Drawdown (%)');
    legend('Portfolio PCA', 'Portfolio CVaR', 'Location', 'southwest');
    grid on;

    % Return distributions
    subplot(2,2,3);
    histogram(port_returns_PCA*100, 50, 'FaceColor', 'b', 'FaceAlpha', 0.7);
    hold on;
    histogram(port_returns_CVaR*100, 50, 'FaceColor', 'r', 'FaceAlpha', 0.7);
    xline(var_PCA*100, 'b--', 'LineWidth', 2, 'Label', 'VaR PCA');
    xline(var_CVaR*100, 'r--', 'LineWidth', 2, 'Label', 'VaR CVaR');
    title('Return Distributions');
    xlabel('Daily Returns (%)');
    ylabel('Frequency');
    legend('Portfolio PCA', 'Portfolio CVaR');
    grid on;

    % Performance metrics bar chart
    subplot(2,2,4);
    metrics = [portfolio_vol_annual_PCA*100, portfolio_vol_annual_CVaR*100;
               abs(cvar_PCA)*100, abs(cvar_CVaR)*100;
               abs(max_drawdown_PCA)*100, abs(max_drawdown_CVaR)*100;
               sharpe_PCA, sharpe_CVaR];
    bar(metrics);
    set(gca, 'XTickLabel', {'Volatility (%)', 'CVaR (%)', 'Max DD (%)', 'Sharpe'});
    title('Performance Metrics Comparison');
    legend('Portfolio PCA', 'Portfolio CVaR');
    grid on;
end