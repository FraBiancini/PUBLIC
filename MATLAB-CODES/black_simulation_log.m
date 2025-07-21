function[S] = black_simulation_log(T,Spot,r,q,sigma,N,expiry)
   % Monte Carlo simulation for the asset S under the LV model
   % T.. model expiries
   % Spot.. spot of the asset needed to reconstruct forwards
   % r.. risk-free rates at T, q.. dividend yiels at T
   % sigma.. Black volatility
   % N.. MC simulations, expiry.. MC time horizon

   logX = zeros(N,1);
   S = zeros(length(expiry),N);
   
   % generate random numbers
   W = randn(N,1,length(expiry));

   for k = 1:length(expiry)
      if k==1
         dt = expiry(k);
      else
         dt = (expiry(k)-expiry(k-1));
      end

      logX(:) = logX(:) - 0.5 * dt * sigma.^2 + sigma .* W(:,1,k) * sqrt(dt);

      fwd = forward(Spot,T,r,q,expiry(k));
      S(k,:) = fwd * exp(logX(:));
   end
end

