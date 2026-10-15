function [B, Fwd] = Disc_Forw_calib(mkt)
% Calibra i discount factor e i forward prices dalle opzioni
% Input:
%   mkt = struct con campi expiry, strikes, CallBid, CallAsk, PutBid, PutAsk
% Output:
%   DF  = vettore dei discount factor B(t0,Ti)
%   Fwd = vettore dei forward prices F(t0,Ti)

    nExp = length(mkt.datesExpiry);   % numero di expiry disponibili
    B   = zeros(1, nExp);
    Fwd  = zeros(1, nExp);
    today = datetime('2017-12-08', 'InputFormat', 'yyyy-MM-dd');
    tau = yearfrac(today,mkt.datesExpiry,3);
    for i = 1:nExp
        K = mkt.strikes(i).value;
        C = (mkt.callBid(i).prices + mkt.callAsk(i).prices) / 2; % mid price call
        P = (mkt.putBid(i).prices  + mkt.putAsk(i).prices)  / 2; % mid price put
        
        % --- Put-Call Parity: C - P = DF*(F - K) ---
        Y = C - P;
        X = K;
        
        
        % --- Regressione lineare Y = a*K + b ---
        coeffs = polyfit(X, Y, 1);
        a = coeffs(1);
        b = coeffs(2);

        % --- Parametri ---
        B(i)  = -a;        % discount factor
        Fwd(i) = -b / a;    % forward price
         % figure
         % plot(K, mkt.callAsk(i).prices-mkt.putBid(i).prices+B(i)*K)
         % hold on
         % plot(K,mkt.callBid(i).prices-mkt.putAsk(i).prices+B(i)*K)
         % Fwd(i) = (max(mkt.callBid(i).prices-mkt.putAsk(i).prices+B(i)*K)+min(mkt.callAsk(i).prices-mkt.putBid(i).prices+B(i)*K))/2;
         % 
         % iv=blkimpv(Fwd(i),K,-log(B(i))./tau(i),tau(i),C);
         % iv_put=blkimpv(Fwd(i),K,-log(B(i))./tau(i),tau(i),P,'Class', {'Put'});
         % surface=[iv_put(K<Fwd(i)),iv(K>=Fwd(i))]
         % figure
         % plot(K,surface)
    end
end
