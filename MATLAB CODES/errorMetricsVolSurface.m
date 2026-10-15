function [rmse, mape] = errorMetricsVolSurface(theta_hat, price_handles, C_mat, P_mat,all_strikes, Fwd_today,B_today, T, today, modelType)
% Calcola RMSE e MAPE della vol surface implicita
% modelType = 'NIG' oppure 'ATS'

    iv_err_cell = cell(numel(all_strikes),1);
    iv_mkt_cell = cell(numel(all_strikes),1);
    iv_err_all = [];
    iv_mkt_all = [];
    for i = 1:numel(all_strikes)
        K   = all_strikes{i};
        F0  = Fwd_today(i);
        tau = yearfrac(today, T(i), 3);
        B0 = B_today(i);

        % --- Parametri locali ---
        if strcmpi(modelType,'ATS')
            theta_loc = ats_local_params(theta_hat, tau); % [sigma, eta(tau), k(tau)]
        else
            theta_loc = theta_hat; % NIG: parametri statici
        end

        % Prezzi modello
        C_model = price_handles{i}(theta_loc);
       
        P_model = C_model - F0*B0 + K*B0;

        % Prezzi mercato
        C_mkt = C_mat{i};
        P_mkt = P_mat{i};

        % Volatilità implicite
        iv_mkt   = blkimpv(F0, K, 0, tau, C_mkt);
        iv_model = blkimpv(F0, K, 0, tau, C_model);

        iv_mkt   = nan(size(K));
        iv_model = nan(size(K));
        for kk = 1:length(K)
            if K(kk) >= F0*B0
                % OTM call
                iv_mkt(kk)   = blkimpv(F0, K(kk), 0, tau, C_mkt(kk), 'Class', {'Call'});
                iv_model(kk) = blkimpv(F0, K(kk), 0, tau, C_model(kk), 'Class', {'Call'});
               
            else
                % OTM put
                iv_mkt(kk)   = blkimpv(F0, K(kk), 0, tau, P_mkt(kk), 'Class', {'Put'});
                iv_model(kk) = blkimpv(F0, K(kk), 0, tau, P_model(kk), 'Class', {'Put'});
               
            end
        end

       
        iv_err_all = [iv_err_all,iv_model - iv_mkt];
        iv_mkt_all = [iv_mkt_all,iv_mkt];
    end

   

    % RMSE e MAPE
    rmse = sqrt(mean(iv_err_all.^2, 'omitnan'));
    mape = mean(abs(iv_err_all ./ iv_mkt_all), 'omitnan') * 100;
end
