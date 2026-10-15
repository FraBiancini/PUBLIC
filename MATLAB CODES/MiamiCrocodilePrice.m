function [price_MiamiCroco, CI_low, CI_high, n_delta_hedge] = MiamiCrocodilePrice(thetaATS_hat, today, T, B_today,Fwd_today,SpotStart, Nsim,bump,modelType)
% Calcola il prezzo medio del certificato Miami Crocodile e il delta
% INPUT:
%   thetaATS_hat : parametri ATS [sigma0, eta0, k0, beta, delta]
%   today        : data di valutazione (datetime)
%   T            : maturity (datetime)
%   B_today      : curva dei DF oggi
%   Nsim         : numero simulazioni
%   h            : bump percentuale per il delta (es. 0.01)
% OUTPUT:
%   price_MiamiCroco : valore medio del payoff simulato
%   CI_low, CI_high  : intervallo di confidenza al 99%
%   delta            : stima Monte Carlo del delta

    alpha = 0.5;
    % Observation dates
    obsDates = [ ...
        datetime(2018,4,9); ...
        datetime(2018,8,8); ...
        datetime(2018,12,10); ...
        datetime(2019,4,8); ...
        datetime(2019,8,8); ...
        datetime(2019,12,9) ...
    ];

    obsDates = obsDates(obsDates > today);

   
% Precompute time fractions
dt = yearfrac(today, obsDates, 3);
% cons = zeros(size(dt));
% cons(1) = dt(1);
% cons(2:end) = yearfrac(obsDates(1:end-1),obsDates(2:end),3);
n = numel(dt);

if strcmpi(modelType,'ATS')
    % Parametri
    sigma0 = thetaATS_hat(1);
    eta0   = thetaATS_hat(2);
    k0     = thetaATS_hat(3);
    beta   = thetaATS_hat(4);
    deltaP = thetaATS_hat(5);
    
else
    sigma0 = thetaATS_hat(1);
    eta0   = thetaATS_hat(2);
    k0    = thetaATS_hat(3);
    beta = 0;
    deltaP = 0;
end

L_t = @(z,t) exp(t./(k0.*(t.^beta)) .* (1 - sqrt(1 + 2*(k0.*(t.^beta))*sigma0^2.*z)));


Psi_t = @(u,t) ...
    (-1i.*u.*log(L_t(eta0.*(t.^deltaP), t))) + ...
     log(L_t((u.^2 + 1i.*u.*(1 + 2.*eta0.*(t.^deltaP)))/2, t));


p = (1/2 + (eta0 .* (dt.^deltaP))) + sqrt((1/2 + (eta0 .* (dt.^deltaP))).^2 + 2 .* (1 - alpha) ./ (sigma0^2 .*  (k0   .* (dt.^beta))));
a = (p / 2);
% p = (1/2 + (eta0 .* (cons.^deltaP))) + sqrt((1/2 + (eta0 .* (cons.^deltaP))).^2 + 2 .* (1 - alpha) ./ (sigma0^2 .*  (k0   .* (cons.^beta))));
% a = (p / 2);




phi_cum = @(u,i) exp(Psi_t(u, dt(i)));




f1 = SimulateFromCF(@(u) phi_cum(u,1), 16, 0.001, a(1), Nsim);

df = zeros(n, Nsim);
df(1,:) = f1;

for i = 2:n-1
    % Usa CF incrementale corretta
    
    df(i,:) = SimulateFromCF(@(u) exp(Psi_t(u, dt(i)) - Psi_t(u, dt(i-1))), 16, 0.001, a(i), Nsim).';
    
end
df(n,:) = SimulateFromCF(@(u) phi_cum(u,6)/phi_cum(u,5), 17, 0.001, a(n), Nsim).';
%f_i = zeros(n, Nsim);
% for i = 2:n
%      df(i,:) = SimulateFromCF(@(u) exp(Psi_t(u,cons(i))), 16, 0.001, a(i), Nsim).';
% end



f_i = cumsum(df, 1);
% mean(f_i(6,:))


    % Parametri certificato
    Notional = 15e6;
    triggerLevel = 1.20;
    barrierLevel = 0.90;
    liquidationPct = [1.02, 1.03, 1.05, 1.10, 1.15, 1.20];
    addFinal = 0.23;
    factor = 0.90;

    price     = zeros(1,Nsim);
    price_up  = zeros(1,Nsim);
    price_dn  = zeros(1,Nsim);
    B_obs = getDisc(today, T, B_today, obsDates);
    Fwd_obs = spline(yearfrac(today,T,3),Fwd_today,yearfrac(today,obsDates,3));

    % --- Prezzo base ---
    for j = 1:Nsim

        check_ratio = Fwd_obs.*exp(f_i(:,j)) / SpotStart;
        % Autocall
        idxAC = find(check_ratio >= triggerLevel, 1, 'first');
        idxB = find(check_ratio <= barrierLevel,1,"first");
        barrierFlag = ~isempty(idxB);
        if ~isempty(idxAC) && idxAC ~= n
            price(j) = B_obs(idxAC)* Notional * liquidationPct(idxAC);
            continue
        end

        
        Rf = check_ratio(end);

        if (Rf >= triggerLevel) && ~barrierFlag
            price(j) = B_obs(end)*Notional * (1.20 + addFinal);
        elseif (Rf < triggerLevel) && ~barrierFlag
            price(j) = B_obs(end)*Notional * 1.20;
        elseif (Rf < triggerLevel) && barrierFlag
            price(j) = B_obs(end)*Notional * max(0, factor * Rf);
        else % (Rf >= triggerLevel) && barrierFlag
            price(j) = B_obs(end)*Notional * 1.10;
        end
    end

    price_MiamiCroco = mean(price);

    % --- Intervallo di confidenza ---
    stdPrice = std(price);
    SE = stdPrice / sqrt(Nsim);
    conf_level = 0.01; % 99%
    z = norminv(1 - conf_level/2, 0, 1);
    CI_low  = price_MiamiCroco - z * SE;
    CI_high = price_MiamiCroco + z * SE;

        
    if(nargout > 3)
        for j = 1:Nsim
            
            
            check_ratio = ((Fwd_obs).*exp(f_i(:,j))+bump) / SpotStart;
            idxAC = find(check_ratio >= triggerLevel, 1, 'first');
            if ~isempty(idxAC) && idxAC ~= n
                price_up(j) = B_obs(idxAC) *Notional * liquidationPct(idxAC);
                continue
            end
    
            barrierFlag = any(check_ratio <= barrierLevel);
            Rf = check_ratio(end);
    
            if (Rf >= triggerLevel) && ~barrierFlag
                price_up(j) = B_obs(end)*Notional * (1.20 + addFinal);
            elseif (Rf < triggerLevel) && ~barrierFlag
                price_up(j) = B_obs(end)*Notional * 1.20;
            elseif (Rf < triggerLevel) && barrierFlag
                price_up(j) = B_obs(end)*Notional * max(0, factor * Rf);
            else
                price_up(j) = B_obs(end)*Notional * 1.10;
            end
        end
    
       
        for j = 1:Nsim
            
          
            check_ratio = ((Fwd_obs).*exp(f_i(:,j))-bump) / SpotStart;
            
            idxAC = find(check_ratio >= triggerLevel, 1, 'first');
            if ~isempty(idxAC) && idxAC ~= n
                price_dn(j) = B_obs(idxAC)*Notional * liquidationPct(idxAC);
                continue
            end
    
            barrierFlag = any(check_ratio <= barrierLevel);
            Rf = check_ratio(end);
    
            if (Rf >= triggerLevel) && ~barrierFlag
                price_dn(j) = B_obs(end)*Notional * (1.20 + addFinal);
            elseif (Rf < triggerLevel) && ~barrierFlag
                price_dn(j) = B_obs(end)*Notional * 1.20;
            elseif (Rf < triggerLevel) && barrierFlag
                price_dn(j) = B_obs(end)*Notional * max(0, factor * Rf);
            else
                price_dn(j) = B_obs(end)*Notional * 1.10;
            end
        end
    
       
        price_up_mean = mean(price_up);
        price_dn_mean = mean(price_dn);
        n_delta_hedge = (price_up_mean - price_dn_mean) / (2*bump);
    end

end
