function [C_bump, P_bump, volSmile_bump] = bumpVolSmile(today,C_mat, P_mat, K_mat, T_vec, F_today, bumpSize)
% INPUT:
%   C_mat    : cell array, ogni cella = prezzi call per una maturity
%   P_mat    : cell array, ogni cella = prezzi put per una maturity
%   K_mat    : cell array, ogni cella = strike corrispondenti
%   T_vec    : vettore delle maturity (anni)
%   S0       : spot corrente
%   r        : tasso risk-free
%   bumpSize : shift da applicare (es. 0.0001 per 1bp)
% OUTPUT:
%   C_bump   : cell array con prezzi call ricalcolati alle vol bumpate
%   P_bump   : cell array con prezzi put ricalcolati alle vol bumpate
%   volSmile_bump : celle con le vol implicite bumpate

    nMat = numel(C_mat);
    C_bump = cell(size(C_mat));
    P_bump = cell(size(P_mat));
    volSmile_bump = cell(size(C_mat));

    for i = 1:nMat
        K_i = K_mat{i};
        T_i = yearfrac(today,T_vec(i),3);

        nStrikes = numel(K_i);
        vol_i = zeros(1,nStrikes);

        for j = 1:nStrikes
            K = K_i(j);

            if K > F_today(i)   % strike sopra il forward → uso call
                price = C_mat{i}(j);
                vol_i(j) = blkimpv(F_today(i), K, 0, T_i, price, 'Class', {'call'});
            else                   % strike sotto il forward → uso put
                price = P_mat{i}(j);
                vol_i(j) = blkimpv(F_today(i), K, 0, T_i, price, 'Class', {'put'});
            end
        end

        % bump delle vol
        vol_bumped = vol_i + bumpSize;
        volSmile_bump{i} = vol_bumped;

        % ricalcolo prezzi con Black-Scholes
        [C_bump{i},P_bump{i}] = arrayfun(@(K,sigma) blkprice(F_today(i), K, 0, T_i, sigma), K_i, vol_bumped);
        %P_bump{i} = arrayfun(@(K,sigma) blkprice(F_today(i), K, 0, T_i, sigma) - B_today(i)*(F_today(i) - K), K_i, vol_bumped);
    end
end
