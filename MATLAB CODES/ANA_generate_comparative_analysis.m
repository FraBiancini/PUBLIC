function ANA_generate_comparative_analysis(performance_metrics, risk_metrics, robustness_metrics)
    % Generate comparative analysis
    
    portfolio_names = fieldnames(performance_metrics);
    n_portfolios = length(portfolio_names);
    
    fprintf('\n=== OUT-OF-SAMPLE PERFORMANCE COMPARISON (2023-2024) ===\n\n');
    
    % Create summary table
    summary_table = table();
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        
        summary_table.Portfolio{i} = port_name;
        summary_table.AnnualReturn(i) = performance_metrics.(port_name).AnnualReturn;
        summary_table.AnnualVolatility(i) = performance_metrics.(port_name).AnnualVolatility;
        summary_table.SharpeRatio(i) = risk_metrics.(port_name).SharpeRatio;
        summary_table.SortinoRatio(i) = risk_metrics.(port_name).SortinoRatio;
        summary_table.MaxDrawdown(i) = risk_metrics.(port_name).MaxDrawdown;
        summary_table.CalmarRatio(i) = risk_metrics.(port_name).CalmarRatio;
        summary_table.CVaR_95(i) = risk_metrics.(port_name).CVaR_95;
        summary_table.EffectiveN(i) = robustness_metrics.(port_name).EffectiveN;
        summary_table.DiversificationRatio(i) = performance_metrics.(port_name).DiversificationRatio;
        summary_table.HerfindahlIndex(i) = performance_metrics.(port_name).HerfindahlIndex;
    end
    
    % Display summary table
    disp(summary_table);
    
    % Rank portfolios by different metrics
    fprintf('\n=== PORTFOLIO RANKINGS ===\n');
    
    [~, sharpe_rank] = sort([summary_table.SharpeRatio], 'descend');
    [~, sortino_rank] = sort([summary_table.SortinoRatio], 'descend');
    [~, drawdown_rank] = sort([summary_table.MaxDrawdown], 'ascend');
    
    fprintf('Top 3 by Sharpe Ratio: ');
    for i = 1:3
        fprintf('%s ', summary_table.Portfolio{sharpe_rank(i)});
    end
    fprintf('\n');
    
    fprintf('Top 3 by Sortino Ratio: ');
    for i = 1:3
        fprintf('%s ', summary_table.Portfolio{sortino_rank(i)});
    end
    fprintf('\n');
    
    fprintf('Top 3 by Minimum Drawdown: ');
    for i = 1:3
        fprintf('%s ', summary_table.Portfolio{drawdown_rank(i)});
    end
    fprintf('\n');
    
    % Analyze robustness
    fprintf('\n=== ROBUSTNESS ANALYSIS ===\n');
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        fprintf('%s: Sharpe Stability = %.4f, Vol Stability = %.4f\n', ...
            port_name, robustness_metrics.(port_name).SharpeStability, ...
            robustness_metrics.(port_name).VolatilityStability);
    end
end
