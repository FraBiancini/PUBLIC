function err = objfun(theta, price_handles, C_mat, P_mat, all_strikes, Fwd, B)
% Objective function per calibrazione NIG su più expiry
% Input:
%   theta         : parametri NIG [sigma, eta, k]
%   price_handles : cella di handle, price_handles{i}(theta) = call modello su all_strikes{i}
%   C_mat, P_mat  : celle con prezzi di mercato (call e put)
%   all_strikes   : celle con vettori di strike per ogni expiry
%   Fwd, B        : vettori forward e discount factor per ogni expiry
% Output:
%   err           : somma dei quadrati delle differenze call/put su tutte le expiry

    total_err = 0;

    for i = 1:numel(all_strikes)
        K     = all_strikes{i};
        F0    = Fwd(i);
        DF    = B(i);

        % Prezzi modello (call)
        C_model = price_handles{i}(theta);

        % Prezzi mercato
        C_mkt = C_mat{i};
        P_mkt = P_mat{i};

        % Indici call/put
        idx_put = K < F0;
        idx_call  = K > F0;
        

        % Errori call
        err_call = (C_model(idx_call) - C_mkt(idx_call)).^2;

        % Put modello da parità put-call
        P_model = C_model(idx_put) - F0*DF + K(idx_put)*DF;
        err_put = (P_model - P_mkt(idx_put)).^2;

        % Somma
        total_err = total_err + sum(err_call) + sum(err_put);
    end

    err = total_err;
end
