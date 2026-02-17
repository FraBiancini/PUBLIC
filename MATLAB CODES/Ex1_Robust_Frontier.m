function [w_MVP_Rob, w_sh_Rob, meanRet, meanRisk, RetPtfSim, RiskPtfSim] = Ex1_Robust_Frontier(m,groups,nm)
NumAssets=16;
x0 = ones(NumAssets,1)/NumAssets;      % initial guess = equal weights
lb = zeros(1,NumAssets);               % lower bound
ub = 0.3*ones(1,NumAssets);                % upper bound
def_ass=strcmp(groups, 'Defensive')';
neu_ass=strcmp(groups, 'Neutral')';
cyc_ass=strcmp(groups, 'Cyclical')';
A=[def_ass;-neu_ass];% constraints on defensive and neutral assets
b=[0.45;-0.20];% constraints on defensive and neutral assets


p = Portfolio('AssetList', nm);        % nm = cell array of asset names
p = setDefaultConstraints(p);
p.UpperBound = ub;
p = setInequality(p, A, b);
p = setAssetMoments(p, m.expected_returns, m.Cov_mat);
N = 100;        % number of simulations
nPort = 100;    % points on each frontier

RiskPtfSim = zeros(nPort, N);
RetPtfSim  = zeros(nPort, N);
Weights    = zeros(NumAssets, nPort); % store weights for each simulation

rng(42)

for n = 1:N
    % Simulate asset returns from multivariate normal
    R_sim = mvnrnd(m.expected_returns, m.Cov_mat);
    % Simulate covariance matrix using inverse Wishart
    Cov_sim = iwishrnd(m.Cov_mat, NumAssets);
    % Create Portfolio object with simulated moments
    P_sim = setAssetMoments(p, R_sim, Cov_sim);
    % Estimate efficient frontier
    w_sim = estimateFrontier(P_sim, nPort);
    % Store weights and corresponding portfolio moments
    Weights = Weights + w_sim;
    [pf_risk, pf_ret] = estimatePortMoments(P_sim, w_sim);
    RiskPtfSim(:,n) = pf_risk;
    RetPtfSim(:,n)  = pf_ret;
end


% Compute mean frontier and confidence intervals
meanRisk = mean(RiskPtfSim,2);            % mean volatility across simulations
meanRet  = mean(RetPtfSim,2);             % mean expected return

% Compute average weights portfolio
P_avg = Portfolio('AssetList', nm);
P_avg = setDefaultConstraints(P_avg);
P_avg = setAssetMoments(P_avg, m.expected_returns, m.Cov_mat);


% Compute risk and return of average weights portfolio
meanWeights = Weights./N;            

[~,i_MVP_Rob]=min(meanRisk);
w_MVP_Rob=meanWeights(:,i_MVP_Rob); 
meanSharp=meanRet./meanRisk;
[~,i_sh_Rob]=max(meanSharp);
w_sh_Rob=meanWeights(:,i_sh_Rob);