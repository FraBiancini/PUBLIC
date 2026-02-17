function macro_exposures = ANA_compute_macro_exposures(portfolio_weights, macro_groups)
    % Calculate macro exposure for each portfolio
    
    portfolio_names = fieldnames(portfolio_weights);
    n_portfolios = length(portfolio_names);
    
    % Identify assets in each macro group
    cyclical_idx = find(strcmp(macro_groups, 'Cyclical'));
    neutral_idx = find(strcmp(macro_groups, 'Neutral'));
    defensive_idx = find(strcmp(macro_groups, 'Defensive'));
    
    % Initialize output structure
    macro_exposures = struct();
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        weights = portfolio_weights.(port_name);
        
        % Calculate exposures
        cyclical_exp = sum(weights(cyclical_idx));
        neutral_exp = sum(weights(neutral_idx));
        defensive_exp = sum(weights(defensive_idx));
        
        % Store results - use the same field name as in portfolio_weights
        macro_exposures.(port_name).Cyclical = cyclical_exp;
        macro_exposures.(port_name).Neutral = neutral_exp;
        macro_exposures.(port_name).Defensive = defensive_exp;
        macro_exposures.(port_name).Weights = weights;
        
        % Calculate concentration within groups
        if cyclical_exp > 0
            macro_exposures.(port_name).CyclicalHHI = sum((weights(cyclical_idx)/cyclical_exp).^2);
        else
            macro_exposures.(port_name).CyclicalHHI = 0;
        end
        
        if neutral_exp > 0
            macro_exposures.(port_name).NeutralHHI = sum((weights(neutral_idx)/neutral_exp).^2);
        else
            macro_exposures.(port_name).NeutralHHI = 0;
        end
        
        if defensive_exp > 0
            macro_exposures.(port_name).DefensiveHHI = sum((weights(defensive_idx)/defensive_exp).^2);
        else
            macro_exposures.(port_name).DefensiveHHI = 0;
        end
    end
end