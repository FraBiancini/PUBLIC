function [vega_exp,call_model_up,put_model_up] = computeExpiryVegas(today, K_mat, T_exp, Fwd_today,B_today, vol_base, bumpSize, SpotStart, Nsim, price0,price_handles,theta0,straddle_dates)
% Calcola la vega locale bumpando una ExpDate alla volta (solo up)
% INPUT:
%   C_mat,P_mat,K_mat,T_exp,Fwd_today : dati di mercato
%   vol_base : cella 1xN con vettori di vol implicite per ogni ExpDate
%   bumpSize : ampiezza del bump (es. 0.001 = 0.1%)
%   calibAndPriceFun : handle @(C,P,K,T,Fwd,Spot,Nsim) -> prezzo certificato
%   price0   : prezzo base del certificato (senza bump)
% OUTPUT:
%   vega_exp : vettore con la vega per ciascuna ExpDate (ultime 7)

   

    L = length(straddle_dates);
    idxRange = find(ismember(T_exp,straddle_dates));
    if(length(idxRange) ~= length(straddle_dates))
        fprintf("prodotto scomparso dal mercato overnight")
    end
    call_model_up = {};
    vega_exp = zeros(1,L);
    put_model_up = {};
    for k = 1:L
        iExp = idxRange(k);
        

        % --- bump up solo su ExpDate iExp ---
        vol_up   = vol_base;
        vol_up{iExp} = vol_base{iExp} + bumpSize;
        
        
        [C_up,P_up] = repriceFromVols(today,K_mat,T_exp,Fwd_today,vol_up);
        theta_up = calibrateATS(T_exp,today,Fwd_today,B_today,K_mat,C_up,P_up,price_handles,theta0);
        
        
        price_up = MiamiCrocodilePrice(theta_up,today,T_exp,B_today,Fwd_today,SpotStart,Nsim,bumpSize,'ATS');
        
        vega_exp(k) = (price_up - price0)/(bumpSize);
    end
end

function [C_out,P_out] = repriceFromVols(today,K_mat,T_exp,Fwd_today,vol_cell)
% Ricostruisce i prezzi call/put da vol implicite bumpate
    nMat = numel(T_exp);
    T_exp_y = yearfrac(today,T_exp,3);
    C_out = cell(1,nMat); P_out = cell(1,nMat);
    for i=1:nMat
        T_i = T_exp_y(i); K_i = K_mat{i};
        [C_tmp,P_tmp] = arrayfun(@(K,sigma) blkprice(Fwd_today(i),K,0,T_i,sigma), ...
                                 K_i(:), vol_cell{i}(:));
        C_out{i} = reshape(C_tmp,1,[]);
        P_out{i} = reshape(P_tmp,1,[]);
    end
end

