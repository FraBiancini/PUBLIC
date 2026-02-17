function [q_straddle,delta] = hedgeWithStraddleVolSurface(today,vega_exp,Fwd_today,T_exp,K_mat,vol_base,Spot_today,C_mat,P_mat,straddle_dates)
% Calcola le quantità di straddle ATM da comprare/vendere per neutralizzare vega_exp
% usando la vol surface e interpolando linearmente ad ATM.
%
% INPUT:
%   vega_exp   : [1xN] vettore vega del certificato per ciascuna expiry
%   Fwd_today  : [1xN] forward price per ciascuna expiry
%   B_today    : [1xN] discount factor per ciascuna expiry
%   T_exp      : [1xN] date di scadenza
%   K_mat      : cell array {1xN}, strikes quotati per ciascuna expiry
%   vol_base   : cell array {1xN}, vol implicite corrispondenti a K_mat
%   r          : [1xN] tasso risk-free coerente con B_today (oppure 0 se lavori in forward measure)
%
% OUTPUT:
%   q_straddle : [1xN] quantità di straddle ATM da comprare (<0 = vendere)

    
    T_exp_y = yearfrac(today,T_exp,3); % time to maturity in anni

    L = length(straddle_dates);
    idxrange = find(ismember(T_exp,straddle_dates));
    if( length(idxrange) ~= length(straddle_dates))
        fprintf("prodotto scomparso dal mercato overnight")
    end
    q_straddle = zeros(1,L);
    delta = zeros(1, L);
    for j = 1:L
        i = idxrange(j);
        % --- Interpolazione lineare della vol ATM ---
        K_i   = K_mat{i};
        vol_i = vol_base{i};
        Fwd_i = Fwd_today(i);
        % calls_i   = C_mat{i};
        % puts_i    = P_mat{i};
        [~, idxATM] = min(abs(K_i - Fwd_i));
        
       
        sigmaATM = vol_i(idxATM);
        
      
        % volbump = mean(vol_i)*5e-2;
        % [Cvup,Pvup] = blkprice(Fwd_i,K_i(idxATM),0,T_exp_y(i),sigmaATM+volbump);
        % vega_call = (Cvup - calls_i(idxATM))/(volbump);
        % vega_put = (Pvup - puts_i(idxATM))/(volbump);
        % vega_straddle = vega_call+vega_put;
        d1 = (log(Fwd_i/K_i(idxATM))+0.5*sigmaATM^2*T_exp_y(i))/(sigmaATM*sqrt(T_exp_y(i)));
        vega_straddle = 2*normpdf(d1)*sqrt(T_exp_y(i))*Fwd_i;
        
        % --- Quantità di straddle per neutralizzare vega_exp(i) ---
        q_straddle(j) = round(vega_exp(j) / vega_straddle);
        % bumpdelta = Spot_today*8e-3;
        % 
        % [Cup,Pup] = blkprice(Fwd_i+bumpdelta,K_i(idxATM),0,T_exp_y(i),sigmaATM);
        % [Cdn,Pdn] = blkprice(Fwd_i-bumpdelta,K_i(idxATM),0,T_exp_y(i),sigmaATM);
        % delta(j) = ((Cup+Pup)-(Cdn+Pdn))/(2*bumpdelta);
        delta(j) = normcdf(d1)*2-1;
       
    end
end
