function [muBL_contrib,contrib]=Ex2_Views_Effect(m,mu_mkt,P,q,Omega)
n_views=3;
n_assets = size(m.returns, 2);

% We decompose the impact of each view on the final BL expected returns. 
% This helps understand which view drives which asset’s return

contrib = zeros(n_assets, n_views);
for i = 1:n_views
    P_i = P(i,:)';
    Omega_i = Omega(i,i);
    contrib(:,i) = m.Cov_mat * P_i / (P_i' * m.Cov_mat * P_i + Omega_i) * (q(i) - P_i' * mu_mkt);
end
% each contrib(:,i) shows how the expected return of each asset is adjusted because of view i

% Total impact on expected returns
muBL_contrib = mu_mkt + sum(contrib,2);
