function [P,q,Omega] = Ex2_Build_Views(m,macroGroups,assetNames, tau)

n_views = 3; 
n_assets= size(m.returns, 2);
 
P = zeros(n_views, n_assets);  % pick matrix
q = zeros(n_views, 1);          % expected returns from views
Omega = zeros(n_views);         % uncertainty of views

cyclical_assets = find(macroGroups == "Cyclical");
neutral_assets = find(macroGroups == "Neutral");
defensive_assets = find(macroGroups == "Defensive");

asset_10_index = find(assetNames == "Asset10");
asset_2_index = find(assetNames == "Asset2");
asset_13_index = find(assetNames == "Asset13");

% View 1: Cyclical assets expected to outperform Neutral ones by +2% annualized
P(1, cyclical_assets) = 1/length(cyclical_assets);   % cyclical assets
P(1, neutral_assets) = -1/length(neutral_assets);   % neutral assets  
q(1) = 0.02; % +2% annualized

% View 2: Asset_10 expected to underperform average Defensive group by -0.7% annualized
P(2, asset_10_index) = 1;                           % Asset_10
P(2, defensive_assets) = -1/length(defensive_assets); % average of defensive assets
q(2) = -0.007; % -0.7% annualized

% View 3: Asset_2 expected to outperform Asset_13 by +1% annualized  
P(3, asset_2_index) = 1;
P(3, asset_13_index) = -1;
q(3) = 0.01; % +1% annualized

% Compute Omega as tau*P*Cov*P' (diagonal approximation)
% We assume views are independent here, which is why Omega is diagonal. 
% Correlated views would require a full covariance matrix
% Smaller variance → higher confidence → more influence on posterior returns
for i = 1:n_views
    Omega(i,i) = tau * P(i,:) * m.Cov_mat * P(i,:)';
end

% from annual view to daily view -> We convert annual views to daily to 
% match the frequency of returns in the dataset
daysPerYear = 252;
q = q / daysPerYear;
Omega = Omega / daysPerYear;