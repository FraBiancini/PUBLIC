function [mon_returns, mon_vol, mon_calm, mon_shar, mon_drawdown, mon_dates] = Ex5_Time_Evaluation_PTF_ALL(in_sample_dates, prices_in_sample, Portfolio_Matrix, ptf_mon, portfolio_names)


previous = 1;
first = 1;
n_port = size(Portfolio_Matrix, 2);


% Inizializzazione strutture
mon_returns = [];
mon_vol = [];
mon_calm = [];
mon_drawdown = [];
mon_shar = [];

mon_dates = in_sample_dates(1);
j = 1;

for i = 1:length(in_sample_dates)
    months = month(in_sample_dates(i));
    if ( months - previous ~= 0)
        j = j + 1; 
        
    % A-J portfolio
    for p = 1:n_port

        if strcmp(portfolio_names{p}, 'BL_MVP') || strcmp(portfolio_names{p}, 'BL_SH')
                m_mon = measures(prices_in_sample((first:i), :), "none");
            else
                m_mon = measures(prices_in_sample((first:i), :), "Continuous");
        end

        w = Portfolio_Matrix(:, p);
        mon_returns(p, j-1) = m_mon.expected_returns * w;
        mon_vol(p, j-1) = sqrt(w' * m_mon.Cov_mat * w);
        
  
        port_prices = prices_in_sample(first:i, :) * w;
        P_max = max(port_prices);
        P_min = min(port_prices);
        mon_drawdown(p, j-1) = 100 * (P_max - P_min) / P_max;
        mon_calm(p, j-1) = mon_returns(p, j-1) * 252 / (P_max - P_min) * 2;
    end
    
    % Monthly portfolio
    monthly_ptf_weights = ptf_mon(:, j-1);
    monthly_ptf_returns = m_mon.expected_returns * monthly_ptf_weights;
    monthly_ptf_vol = sqrt(monthly_ptf_weights' * m_mon.Cov_mat * monthly_ptf_weights);
    monthly_ptf_prices = prices_in_sample(first:i, :) * monthly_ptf_weights;
    monthly_ptf_P_max = max(monthly_ptf_prices);
    monthly_ptf_P_min = min(monthly_ptf_prices);
    monthly_ptf_drawdown = 100 * (monthly_ptf_P_max - monthly_ptf_P_min) / monthly_ptf_P_max;
    monthly_ptf_calmar = monthly_ptf_returns * 252 / (monthly_ptf_P_max - monthly_ptf_P_min) * 2;
    
    %Monthly portfolio added
    mon_returns(n_port+1, j-1) = monthly_ptf_returns;
    mon_vol(n_port+1, j-1) = monthly_ptf_vol;
    mon_drawdown(n_port+1, j-1) = monthly_ptf_drawdown;
    mon_calm(n_port+1, j-1) = monthly_ptf_calmar;
    
    mon_dates = [mon_dates, in_sample_dates(i)];
    first = i;

    end
    previous = months;
end

% Monthly sharpe ratio 
mon_shar = mon_returns ./ mon_vol;

%
mon_returns = mon_returns';
mon_vol = mon_vol';
mon_calm = mon_calm';
mon_shar = mon_shar';
mon_drawdown = mon_drawdown';

% First date removement
mon_dates = mon_dates(2:end);


end