function [mtm_total,mtm_cert,mtm_spot,mtm_straddle] = computeMTMstart( ...
    today,price_cert_today, Spot_today, q_spot, ...
    Fwd_today, T_exp, K_mat, q_straddle, C_mat, P_mat,K,volbase,straddle_dates,B_today,thetaATS_hat)

% Calcola il mark-to-market del portafoglio (certificato + spot + straddle)
% usando direttamente i prezzi di mercato call/put più vicini all'ATM.

    % --- certificato ---
    mtm_cert = -price_cert_today;   % sei short certificato

    % --- spot hedge ---
    mtm_spot = q_spot * Spot_today;
    
    % --- straddle hedge ---
    L = length(straddle_dates);
    idxrange = find(ismember(T_exp,straddle_dates));
    flag = ismember(straddle_dates,T_exp);
    
    mtm_straddle = 0;
    ttm = yearfrac(today,T_exp,3);
    
    for j = 1:L
        i = idxrange(j);
        strikes_i = K_mat{i};
        calls_i   = C_mat{i};
        puts_i    = P_mat{i};
        vol_i = volbase{i};
        Fwd_i = Fwd_today(i);
        if(flag(j) == 1)
            
            
    
            % Trova strike più vicino all'ATM
            [val, idxATM] = min(abs(strikes_i - K(j)));
            if(val == 0)
                straddle_price = calls_i(idxATM) + puts_i(idxATM);
            else
                sigma = interp1(strikes_i,vol_i,K(j));
                [C,P] = blkprice(Fwd_i,K(j),0,ttm(i),sigma);
                straddle_price = (C+P);
            end
        else
            fprintf("prodotto scomparso dal mercato overnight")
            model_i = makeNIGModelHandles(ttm(i), Fwd_i, B_today(i), 0.5);
            call = model_i.price_calls(K(j),thetaATS_hat,16,0.001);
            put = call - B_today(i)*(Fwd_i - K(j));
            straddle_price = call+put;
        end
            


        % Valore hedge
        mtm_straddle = mtm_straddle + q_straddle(j) * straddle_price;
    end
    
    % --- totale ---
    mtm_total = mtm_cert + mtm_spot + mtm_straddle;
end
