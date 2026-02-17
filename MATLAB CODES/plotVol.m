function VolSmileSurf = plotVol(idx_plot, T, stop_date, Fwd_today, B_today, all_strikes, C_mat, P_mat, price_handles, theta_hat, modelType)
% Plotta volatility smile mercato vs modello
% modelType = 'NIG' oppure 'ATS'

    VolSmileSurf = {};
    for j = idx_plot
        K   = all_strikes{j};
        F0  = Fwd_today(j);
        DF  = B_today(j);
        tau = yearfrac(stop_date, T(j), 3);

        % --- Parametri modello ---
        if strcmpi(modelType,'ATS')
            % Parametri locali ATS per questa maturity
            theta_loc = ats_local_params(theta_hat, tau); % [sigma, eta(tau), k(tau)]
        else
            % NIG: parametri statici
            theta_loc = theta_hat; % [sigma, eta, k]
        end

        % Prezzi modello 
        C_model = price_handles{j}(theta_loc);
        % Prezzi modello 
        P_model = C_model - F0*DF + K*DF;
        

        % Prezzi mercato
        C_mkt = C_mat{j};
        P_mkt = P_mat{j};

        % Implied volatilities: call se K>F0, put se K<F0
        iv_mkt   = nan(size(K));
        iv_model = nan(size(K));
        for kk = 1:length(K)
            if K(kk) >= F0
                % OTM call
                iv_mkt(kk)   = blkimpv(F0, K(kk), 0 , tau, C_mkt(kk), 'Class', {'Call'});
                iv_model(kk) = blkimpv(F0, K(kk), 0, tau, C_model(kk), 'Class', {'Call'});
               
                
            else
                % OTM put
                iv_mkt(kk)   = blkimpv(F0, K(kk), 0 , tau, P_mkt(kk), 'Class', {'Put'});
                iv_model(kk) = blkimpv(F0, K(kk), 0 , tau, P_model(kk), 'Class', {'Put'});
               
               
            end
        end
        
        % Grafico separato per ogni expiry
        VolSmileSurf{j} = iv_mkt;
        figure;
        plot(K, iv_mkt, 'o-', 'LineWidth', 1.5, 'DisplayName', 'Market');
        hold on;
        plot(K, iv_model, '--', 'LineWidth', 2, 'DisplayName', ['Model ' modelType]);
        hold off;
        xlabel('Strike');
        ylabel('Implied Volatility');
        title(['Volatility Smile - Expiry ' datestr(T(j))]);
        legend('show');
        grid on;
    end
end


