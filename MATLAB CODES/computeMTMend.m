function [mtm_total,mtm_cert,mtm_spot,mtm_straddle,K] = computeMTMend( ...
    price_cert_today, Spot_today, q_spot, ...
    Fwd_today, T_exp, K_mat, q_straddle, C_mat, P_mat,straddle_dates)

% Calcola il mark-to-market del portafoglio (certificato + spot + straddle)
% usando direttamente i prezzi di mercato call/put più vicini all'ATM.

    % --- certificato ---
    mtm_cert = -price_cert_today;   % sei short certificato

    % --- spot hedge ---
    mtm_spot = q_spot * Spot_today;
    
    % --- straddle hedge ---
    L = length(straddle_dates);
    idxrange = find(ismember(T_exp,straddle_dates));
    if( length(idxrange) ~= length(straddle_dates))
        fprintf("prodotto scomparso dal mercato overnight")
    end
    mtm_straddle = 0;
    K = zeros(1,L);
    for j = 1:L
        i = idxrange(j);
        strikes_i = K_mat{i};
        calls_i   = C_mat{i};
        puts_i    = P_mat{i};

        % ATM definito come forward (più corretto che spot)
        Fwd_i = Fwd_today(i);

        % Trova strike più vicino all'ATM
        [~, idxATM] = min(abs(strikes_i - Fwd_i));
        K(j) = strikes_i(idxATM);
        
        % Prezzo straddle di mercato = call + put al strike ATM
        straddle_price = calls_i(idxATM) + puts_i(idxATM);
       


        % Valore hedge
        mtm_straddle = mtm_straddle + q_straddle(j) * straddle_price;
    end

    % --- totale ---
    mtm_total = mtm_cert + mtm_spot + mtm_straddle;
end
