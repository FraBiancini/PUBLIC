function [filtered_surface, filtered_strikes] = filter_surface_cells(F0, tenor, K_vector, vol_surface, B, Penny_Limit)
% Applica filtri di Delta e Penny Options e salva i risultati in cell arrays.
%
% INPUTS:
% F0            : Forward Price del sottostante (scalare, es. prices.F0_2026)
% T_opt_vector  : Vettore colonna delle maturità dell'opzione in anni (T_opt)
% K_vector      : Vettore riga degli Strike
% vol_surface   : Matrice di volatilità implicita
% Disc          : Struttura dei fattori di sconto
% Delta_Bounds  : Vettore [Delta_min, Delta_max], es. [0.1, 0.9]
% Penny_Limit   : Soglia in valuta (es. 0.01)
%
% OUTPUTS:
% filtered_surface : Cell array {i} contiene le vol implicite filtrate per la maturità i.
% filtered_strikes : Cell array {i} contiene gli Strike corrispondenti alla maturità i.

    Num_T = length(tenor);
    
    % Inizializza le cell array per i risultati filtrati
    filtered_surface = cell(Num_T, 1);
    filtered_strikes = cell(Num_T, 1);
    
    % Cicla su ogni maturità (riga della superficie)
    for i = 1:Num_T
        T = tenor(i);
      
        
        % Vettori temporanei per i dati validi della maturità corrente
        valid_strikes_temp = [];
        valid_vols_temp = [];
        price_prec = 1e30;
        % Cicla su ogni Strike (colonna della superficie)
        for j = 1:length(K_vector)
            K = K_vector(j);
            Vol = vol_surface(i, j);

            % Salta se la volatilità è non definita o assurda (es. NaN, 0)
            if isnan(Vol) || Vol <= 1e-6 || Vol > 1.8
                continue;
            end

             K_ratio = K / F0;
             ATM_Limit = 0.2;
             if  K_ratio > (1 + ATM_Limit)
                 continue; % Ignora se troppo lontano dall'ATM
             end

            % --- A. FILTRO DELTA (0.1 < Delta < 0.9) ---
            d1 = (log(F0./K) + 0.5 * Vol.^2 .* T) ./ (Vol .* sqrt(T));
            Delta = normcdf(d1);
            
            if Delta < 0.1|| Delta > 0.9
                continue; % Ignora se fuori dai bounds di Delta
            end

            % --- B. FILTRO PENNY OPTIONS ---
            Price = B(i)*blkprice(F0,K,0,T,Vol); 
            if(Price > price_prec)
                continue
            end
            price_prec = Price;
            if Price < Penny_Limit
                continue; % Ignora se è una "penny option"
            end
            
            % Se supera entrambi i filtri, salva il dato
            valid_strikes_temp = [valid_strikes_temp; K];
            valid_vols_temp = [valid_vols_temp; Vol];
        end
        
        % Salva i risultati filtrati nella cell array per la maturità i
        filtered_strikes{i} = valid_strikes_temp;
        filtered_surface{i} = valid_vols_temp;
    end
end