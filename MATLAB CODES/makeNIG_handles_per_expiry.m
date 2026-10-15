function price_handles = makeNIG_handles_per_expiry(tenors, Fwd, B, strikes, M, dz)
% Ritorna una cella di handle: price_handles{i}(theta3) => CALL modello su all_strikes{i}
% dove theta3 = [sigma, eta, k] (parametri NIG per quella maturity)

    nExp = numel(tenors);
    price_handles = cell(1, nExp);
    
    for i = 1:nExp
        tau = tenors(i);
        K = strikes{i};
        DF  = B(i);
        

        model_i = makeNIGModelHandles(tau, Fwd, DF);

        price_handles{i} = @(theta3) model_i.price_calls(K, theta3, M, dz);
      

    end
end
