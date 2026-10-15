function risk_metrics = ANA_calculate_risk_metrics(portfolio_weights, out_sample_prices)

    % Calculate risk-adjusted performance measures
    
    portfolio_names = fieldnames(portfolio_weights);
    n_portfolios = length(portfolio_names);
    
    risk_metrics = struct();
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};

        if strcmp(portfolio_names{i}, 'Portfolio_BL_MVP') || strcmp(portfolio_names{i}, 'Portfolio_BL_SH')
                m_out = measures(out_sample_prices, "none");
                flag=1;
            else
                m_out = measures(out_sample_prices, "Continuous");
                flag=0;
        end
        returns = m_out.returns;
        
        if flag==1
            returns = log(1+returns);
        end

        port_returns = returns * portfolio_weights.(port_name);
        
        % Sharpe Ratio
        risk_free_rate = 0.00; 
        excess_returns = port_returns - risk_free_rate/252;
        sharpe_ratio = mean(excess_returns) / std(port_returns) * sqrt(252);
        
        % Sortino Ratio
        negative_returns = port_returns(port_returns < 0);
        downside_deviation = std(negative_returns);
        sortino_ratio = (mean(port_returns) - risk_free_rate/252) / downside_deviation * sqrt(252);
        
        % Maximum Drawdown
        cumulative_returns = cumprod(1 + port_returns);
        peak = cummax(cumulative_returns);
        drawdown = -(cumulative_returns - peak) ./ peak;
        max_drawdown = max(drawdown);
        
        % Calmar Ratio
        calmar_ratio = mean(port_returns) * 252 / max_drawdown;
        
        % Value at Risk and Conditional VaR (5%)
        var_95 = quantile(port_returns, 0.05);
        cvar_95 = mean(port_returns(port_returns <= var_95));

        % Store results
        risk_metrics.(port_name).SharpeRatio = sharpe_ratio;
        risk_metrics.(port_name).SortinoRatio = sortino_ratio;
        risk_metrics.(port_name).MaxDrawdown = max_drawdown;
        risk_metrics.(port_name).CalmarRatio = calmar_ratio;
        risk_metrics.(port_name).VaR_95 = var_95;
        risk_metrics.(port_name).CVaR_95 = cvar_95;
    end
end
