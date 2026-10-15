function performance_metrics = ANA_calculate_performance_metrics(portfolio_weights, out_sample_prices)
    % Calculate basic performance metrics for each portfolio
    
    portfolio_names = fieldnames(portfolio_weights);
    n_portfolios = length(portfolio_names);
    
    % Annualization factor (assuming daily returns)
    annual_factor = 252;
    
    performance_metrics = struct();
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        weights = portfolio_weights.(port_name);
        
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

        % Portfolio returns
        port_returns = returns * weights;
        
        % Basic metrics
        total_return = prod(1 + port_returns) - 1;
        annual_return = mean(port_returns) * 252;
        annual_volatility = std(port_returns) * sqrt(annual_factor);

        % Diversification Ratio
        diversification_ratio = compute_DR(weights,0,m_out.Cov_mat);
        
        % Herfindahl index
        herfindahl_index = weights'*weights;

        % Store results
        performance_metrics.(port_name).TotalReturn = total_return;
        performance_metrics.(port_name).AnnualReturn = annual_return;
        performance_metrics.(port_name).AnnualVolatility = annual_volatility;
        performance_metrics.(port_name).Returns = port_returns;
        performance_metrics.(port_name).Weights = weights;
        performance_metrics.(port_name).DiversificationRatio = diversification_ratio;
        performance_metrics.(port_name).HerfindahlIndex = herfindahl_index;
    end
end
