function price_handles = makeNIG_handles_per_expiry(T_datetime, stop_date, Fwd, B, alpha, all_strikes, M, dz)
% Ritorna una cella di handle: price_handles{i}(theta3) => CALL modello su all_strikes{i}
% dove theta3 = [sigma, eta, k] (parametri NIG per quella maturity)

    nExp = numel(T_datetime);
    price_handles = cell(1, nExp);
    
    for i = 1:nExp
        tau = yearfrac(stop_date, T_datetime(i), 3);
        F0  = Fwd(i);
        DF  = B(i);
        K   = all_strikes{i};

        model_i = makeNIGModelHandles(tau, F0, DF, alpha);

        price_handles{i} = @(theta3) model_i.price_calls(K, theta3, M, dz);
      

    end
end
