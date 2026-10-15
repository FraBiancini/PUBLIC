function ANA_analyze_macro_impact(macro_exposures, performance_metrics, risk_metrics)
    % Analyze impact of macro exposures on portfolio behavior
    
    % Replace this section in ANA_analyze_macro_impact.m:
portfolio_names = fieldnames(macro_exposures);
n_portfolios = length(portfolio_names);

% Extract exposure data
cyclical_exposures = zeros(n_portfolios, 1);
neutral_exposures = zeros(n_portfolios, 1);
defensive_exposures = zeros(n_portfolios, 1);
sharpe_ratios = zeros(n_portfolios, 1);
max_drawdowns = zeros(n_portfolios, 1);
annual_returns = zeros(n_portfolios, 1);
annual_volatilities = zeros(n_portfolios, 1);

for i = 1:n_portfolios
    port_name = portfolio_names{i};
    
    cyclical_exposures(i) = macro_exposures.(port_name).Cyclical;
    neutral_exposures(i) = macro_exposures.(port_name).Neutral;
    defensive_exposures(i) = macro_exposures.(port_name).Defensive;
    
    % Make sure these metrics exist in the structures
    if isfield(risk_metrics, port_name)
        sharpe_ratios(i) = risk_metrics.(port_name).SharpeRatio;
        max_drawdowns(i) = risk_metrics.(port_name).MaxDrawdown;
    else
        % Try without 'Portfolio_' prefix
        short_name = strrep(port_name, 'Portfolio_', '');
        if isfield(risk_metrics, short_name)
            sharpe_ratios(i) = risk_metrics.(short_name).SharpeRatio;
            max_drawdowns(i) = risk_metrics.(short_name).MaxDrawdown;
        end
    end
    
    if isfield(performance_metrics, port_name)
        annual_returns(i) = performance_metrics.(port_name).AnnualReturn;
        annual_volatilities(i) = performance_metrics.(port_name).AnnualVolatility;
    else
        % Try without 'Portfolio_' prefix
        short_name = strrep(port_name, 'Portfolio_', '');
        if isfield(performance_metrics, short_name)
            annual_returns(i) = performance_metrics.(short_name).AnnualReturn;
            annual_volatilities(i) = performance_metrics.(short_name).AnnualVolatility;
        end
    end
end