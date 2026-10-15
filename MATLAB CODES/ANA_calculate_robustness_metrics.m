function robustness_metrics = ANA_calculate_robustness_metrics(portfolio_weights, out_sample_prices,n_total)
    % Calculate robustness and stability measures
    
    portfolio_names = fieldnames(portfolio_weights);
    n_portfolios = length(portfolio_names);
    
    robustness_metrics = struct();
    
    % Split out-of-sample period into sub-periods for stability analysis
    n_subperiods = 4;
    subperiod_length = floor(n_total / n_subperiods);
    
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
        returns=m_out.returns;

        if flag==1
            returns = log(1+returns);
        end

        port_returns = returns * weights;
        
        % Calculate metrics for each sub-period
        sharpe_ratios = zeros(n_subperiods, 1);
        volatilities = zeros(n_subperiods, 1);
        
        for j = 1:n_subperiods
            start_idx = (j-1)*subperiod_length + 1;
            end_idx = min(j*subperiod_length, n_total);
            sub_returns = port_returns(start_idx:end_idx);
            
            sharpe_ratios(j) = mean(sub_returns) / std(sub_returns) * sqrt(252);
            volatilities(j) = std(sub_returns) * sqrt(252);
        end
        
        % Stability measures
        sharpe_stability = std(sharpe_ratios) / mean(sharpe_ratios);
        vol_stability = std(volatilities) / mean(volatilities);
        
        
        % Portfolio concentration (Herfindahl index)
        herfindahl = weights'*weights;
        effective_n_assets = 1 / herfindahl;
        
        robustness_metrics.(port_name).SharpeStability = sharpe_stability;
        robustness_metrics.(port_name).VolatilityStability = vol_stability;
        robustness_metrics.(port_name).EffectiveN = effective_n_assets;
    end
end
