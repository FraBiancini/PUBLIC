function Lewis_MC(today,all_strikes,T,theta,price_handles,Fwd_today,B_today)

idx_plot = 5:7;
idx_strikes = 40;
dt = yearfrac(today,T,3);
sigma0 = theta(1);
eta0   = theta(2);
k0     = theta(3);
beta   = theta(4);
deltaP = theta(5);
alpha = 0.5;

L_t = @(z,t) exp(t./(k0.*(t.^beta)) .* (1 - sqrt(1 + 2*(k0.*(t.^beta))*sigma0^2.*z)));

% Definisci l'esponente cumulativo Psi_t(u) coerente con la tua phi_t
Psi_t = @(u,t) ...
    (-1i.*u.*log(L_t(eta0.*(t.^deltaP), t))) + ...
     log(L_t((u.^2 + 1i.*u.*(1 + 2.*eta0.*(t.^deltaP)))/2, t));

% Lewis shift per ogni t
p = (1/2 + (eta0 .* (dt.^deltaP))) + sqrt((1/2 + (eta0 .* (dt.^deltaP))).^2 + 2 .* (1 - alpha) ./ (sigma0^2 .*  (k0   .* (dt.^beta))));
a = p / 2;

% CF cumulativa e incrementale come exp(Psi)
phi_cum = @(u,i) exp(Psi_t(u, dt(i)));
rng(4);
for i = idx_plot
    theta3 = ats_local_params(theta,dt(i));
    price_lewis = price_handles{i}(theta3);
    price_lewis = price_lewis(idx_strikes);
    Nsim = 1e5:2e5:5e6;
    price_mc = zeros(1,numel(Nsim));
    
    for j = 1:numel(Nsim)
        N = Nsim(j);
        f = SimulateFromCF(@(u) phi_cum(u,i),16,0.0001,a(i-3),N);
        v = max(0,exp(f)-all_strikes{i}(idx_strikes)/Fwd_today(i));
        price_mc(j) = mean(v)*Fwd_today(i)*B_today(i);
    end

    figure;
    plot(Nsim, price_mc, 'o-', 'LineWidth', 1.5); hold on;
    yline(price_lewis, 'r--', 'LineWidth', 2);
    xlabel('Numero simulazioni N');
    ylabel('Prezzo');
    title(sprintf('Confronto Lewis vs MC - Expiry %s', datestr(T(i))));
    legend('Monte Carlo','Lewis ','Location','best');
    grid on;
    
end

end