function ANA_create_macro_plots(macro_exposures, sharpe_ratios, max_drawdowns, display_names)
    % Create plots for macro exposure analysis
    
    portfolio_names = fieldnames(macro_exposures);
    n_portfolios = length(portfolio_names);
    
    % Extract data
    cyclical = zeros(n_portfolios, 1);
    neutral = zeros(n_portfolios, 1);
    defensive = zeros(n_portfolios, 1);
    display_names_cell = cell(n_portfolios, 1);
    
    for i = 1:n_portfolios
        port_name = portfolio_names{i};
        cyclical(i) = macro_exposures.(port_name).Cyclical;
        neutral(i) = macro_exposures.(port_name).Neutral;
        defensive(i) = macro_exposures.(port_name).Defensive;
        
        if isKey(display_names, port_name)
            display_names_cell{i} = display_names(port_name);
        else
            display_names_cell{i} = port_name;
        end
    end
    
    % Colors
    colors = lines(3);
    
    %% FIGURE 1: Exposure vs Risk Scatter
    figure('Position', [100, 100, 1200, 600]);
    subplot(1,2,1);
    hold on;
    
    for i = 1:n_portfolios
        scatter(defensive(i)*100, max_drawdowns(i)*100, 100, ...
            'filled', 'MarkerFaceColor', [0.2, 0.6, 0.2], ...
            'MarkerEdgeColor', 'k', 'LineWidth', 1);
        
        text(defensive(i)*100 + 0.5, max_drawdowns(i)*100, display_names_cell{i}, ...
            'FontSize', 8, 'HorizontalAlignment', 'left');
    end
    
    xlabel('Defensive Exposure (%)');
    ylabel('Maximum Drawdown (%)');
    title('Defensive Exposure vs Maximum Drawdown');
    grid on;
    
    % Trend line
    if length(defensive) > 2
        p = polyfit(defensive*100, max_drawdowns*100, 1);
        x_fit = linspace(min(defensive*100), max(defensive*100), 100);
        y_fit = polyval(p, x_fit);
        plot(x_fit, y_fit, 'b--', 'LineWidth', 1.5);
    end
    
    hold off;
    
    %% FIGURE 2: Exposure vs Performance Scatter
    subplot(1,2,2);
    hold on;
    
    for i = 1:n_portfolios
        scatter(cyclical(i)*100, sharpe_ratios(i), 100, ...
            'filled', 'MarkerFaceColor', [0.8, 0.2, 0.2], ...
            'MarkerEdgeColor', 'k', 'LineWidth', 1);
        
        text(cyclical(i)*100 + 0.5, sharpe_ratios(i), display_names_cell{i}, ...
            'FontSize', 8, 'HorizontalAlignment', 'left');
    end
    
    xlabel('Cyclical Exposure (%)');
    ylabel('Sharpe Ratio');
    title('Cyclical Exposure vs Sharpe Ratio');
    grid on;
    
    % Trend line
    if length(cyclical) > 2
        p = polyfit(cyclical*100, sharpe_ratios, 1);
        x_fit = linspace(min(cyclical*100), max(cyclical*100), 100);
        y_fit = polyval(p, x_fit);
        plot(x_fit, y_fit, 'b--', 'LineWidth', 1.5, 'DisplayName', 'Trend');
    end
    
    hold off;
    
    
    %% FIGURE 5: Portfolio Comparison Matrix
    figure('Position', [100, 100, 1400, 800]);
    
    % Group portfolios by defensive exposure quartiles
    [~, sort_idx] = sort(defensive, 'descend');
    bar_data_sorted = [cyclical(sort_idx), neutral(sort_idx), defensive(sort_idx)] * 100;
    labels_sorted = display_names_cell(sort_idx);
    
    h = bar(bar_data_sorted, 'stacked');
    h(1).FaceColor = [0.8, 0.2, 0.2];
    h(2).FaceColor = [0.9, 0.9, 0.2];
    h(3).FaceColor = [0.2, 0.6, 0.2];
    
    set(gca, 'XTickLabel', labels_sorted, 'XTickLabelRotation', 45);
    ylabel('Exposure (%)');
    title('Portfolios divided by Macro-sensitivities');
    legend('Cyclical', 'Neutral', 'Defensive', 'Location', 'best');
    grid on;
    ylim([0 105]);
end