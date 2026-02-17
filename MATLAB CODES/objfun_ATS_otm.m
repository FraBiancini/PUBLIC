function err = objfun_ATS_otm(theta5, price_handles, C_mat, P_mat, all_strikes, Fwd, B, T_datetime, stop_date)
% theta5 = [sigma, eta0, k0, beta, delta] (ATS globali)
% price_handles{i}(theta3) accetta [sigma, eta, k] per la maturity i
% Usa solo OTM: CALL per K > F0, PUT per K < F0 (PUT via parità)

    total_err = 0;

    for i = 1:numel(all_strikes)
        % Dati per expiry i
        K   = all_strikes{i};
        F0  = Fwd(i);
        DF  = B(i);
        tau = yearfrac(stop_date, T_datetime(i), 3);

        % Parametri locali ATS per questa maturity
        theta3_loc = ats_local_params(theta5, tau);   % [sigma, eta(tau), k(tau)]

        % Prezzi CALL modello su tutti gli strike
        C_model = price_handles{i}(theta3_loc);

        % Prezzi mercato
        C_mkt = C_mat{i};
        P_mkt = P_mat{i};

      
        % Indici OTM corretti
        idx_call_otm = K > F0;  % CALL OTM
        idx_put_otm  = K < F0;  % PUT OTM
        
        P_model = C_model(idx_put_otm) - F0*DF + K(idx_put_otm)*DF;
       

        
        % Errore CALL OTM
        err_call = (C_model(idx_call_otm) - C_mkt(idx_call_otm)).^2;

        % PUT modello via parità: P = C - B*(F0 - K) = C - F0*DF + K*DF


        % Errore PUT OTM
        err_put = (P_model - P_mkt(idx_put_otm)).^2;

        % Somma
        total_err = total_err + sum(err_call) + sum(err_put);
    end

    err = total_err;
end




