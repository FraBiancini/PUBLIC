function ANA_create_performance_plots(portfolio_weights, dates,out_sample_prices)
   
% Create performance visualization plots 
    
    portfolio_names = fieldnames(portfolio_weights);
    n_portfolios = length(portfolio_names);
    
   
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
    
    
    
    base_colors = lines(7);
    colors = interp1(1:7, base_colors, linspace(1, 7, n_portfolios + 1));

    
    %% FIGURE 1: Cumulative Returns

    figure('Position', [100, 100, 1000, 600]);
    hold on;
    
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
        cumulative_returns = cumprod(1 + port_returns) - 1;
        
     
        if isKey(display_names, port_name)
            legend_name = display_names(port_name);
        else
            legend_name = port_name;
        end
        
        plot(dates, cumulative_returns, 'LineWidth', 2, 'DisplayName', legend_name, ...
            'Color', colors(i, :));
    end
    
    title('Cumulative Returns - Out-of-Sample (2023-2024)');
    xlabel('Date');
    ylabel('Cumulative Return');
    legend('show', 'Location', 'northwest', 'FontSize', 9);
    grid on;
    datetick('x', 'mm/yy', 'keeplimits');
    
    %% FIGURE 2: Drawdowns

    figure('Position', [200, 100, 1000, 600]);
    hold on;
    
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
        cumulative_returns = cumprod(1 + port_returns);
        peak = cummax(cumulative_returns);
        drawdown = abs((cumulative_returns - peak) ./ peak);
        
        
        if isKey(display_names, port_name)
            legend_name = display_names(port_name);
        else
            legend_name = port_name;
        end
        
        plot(dates, drawdown, 'LineWidth', 2, 'DisplayName', legend_name, ...
            'Color', colors(i, :));
    end
    
    title('Portfolio Drawdowns - Out-of-Sample (2023-2024)');
    xlabel('Date');
    ylabel('Drawdown');
    legend('show', 'Location', 'southeast', 'FontSize', 9);
    grid on;
    datetick('x', 'mm/yy', 'keeplimits');
    
    %% FIGURE 3: Risk-Return Profile

    figure('Position', [300, 100, 1000, 600]);
    hold on;
    
    annual_returns = zeros(n_portfolios, 1);
    annual_volatilities = zeros(n_portfolios, 1);
    scatter_labels = cell(n_portfolios, 1);
    
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
        annual_returns(i) = mean(port_returns) * 252;
        annual_volatilities(i) = std(port_returns) * sqrt(252);
        
        
        if isKey(display_names, port_name)
            scatter_labels{i} = display_names(port_name);
        else
            scatter_labels{i} = port_name;
        end
        
 
        scatter(annual_volatilities(i), annual_returns(i), 120, colors(i, :), 'filled', ...
            'MarkerEdgeColor', 'k', 'LineWidth', 1.5);
    end
    
    for i = 1:n_portfolios
        text(annual_volatilities(i), annual_returns(i) + 0.003, scatter_labels{i}, ...
            'FontSize', 10, 'FontWeight', 'bold', 'HorizontalAlignment', 'left');
    end
    
    xlabel('Annual Volatility');
    ylabel('Annual Return');
    title('Risk-Return Profile - Out-of-Sample (2023-2024)');
    grid on;
    
    %% FIGURE 4: Sharpe Ratio Comparison
    figure('Position', [400, 100, 1000, 600]);
    
    sharpe_ratios = zeros(n_portfolios, 1);
    bar_labels = cell(n_portfolios, 1);
    
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
        excess_returns = port_returns;
        sharpe_ratios(i) = mean(excess_returns) / std(port_returns) * sqrt(252);
        
        
        if isKey(display_names, port_name)
            bar_labels{i} = display_names(port_name);
        else
            bar_labels{i} = port_name;
        end
    end
    
    
    bar_handle = bar(sharpe_ratios);
    bar_handle.FaceColor = 'flat';
    for i = 1:n_portfolios
        bar_handle.CData(i,:) = colors(i, :);
    end
    
    set(gca, 'XTickLabel', bar_labels, 'XTickLabelRotation', 45, 'FontSize', 10);
    ylabel('Sharpe Ratio');
    title('Sharpe Ratio Comparison - Out-of-Sample (2023-2024)');
    grid on;
    

    for i = 1:length(sharpe_ratios)
        text(i, sharpe_ratios(i) + 0.02 * sign(sharpe_ratios(i)), ...
            sprintf('%.3f', sharpe_ratios(i)), ...
            'HorizontalAlignment', 'center', 'FontWeight', 'bold', 'FontSize', 9);
    end
    
    fprintf('\n=== OUT-OF-SAMPLE PERFORMANCE METRICS (2023-2024) ===\n');
    fprintf('Portfolio\t\tAnnual Return\tAnnual Vol\tSharpe Ratio\n');
    fprintf('---------\t\t-------------\t-----------\t------------\n');
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        if isKey(display_names, port_name)
            display_name = display_names(port_name);
        else
            display_name = port_name;
        end
        
        fprintf('%-15s\t%.3f\t\t%.3f\t\t%.3f\n', ...
            display_name, annual_returns(i), annual_volatilities(i), sharpe_ratios(i));
    end
end