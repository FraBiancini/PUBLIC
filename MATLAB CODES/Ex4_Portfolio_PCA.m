function     [w_sharpe, fval_sharpe] = Ex4_Portfolio_PCA(m,covar_PCA, muR)

n_assets = size(m.returns,2);

x0 = ones(n_assets,1)/n_assets;
lb = zeros(1,n_assets);
ub = 0.25 * ones(1,n_assets);
Aeq = ones(1,n_assets);
beq = 1;

% Additional volatility constraint

% Additional volatility constraint: maximum 14% annualized

max_vol_annual = 0.14;
max_vol_daily = max_vol_annual/sqrt(252); % Convert annual to daily

% Define nonlinear constraint for volatility
nonlcon = @(x) nonlinConstr(x, covar_PCA, max_vol_daily);

% Maximum Sharpe Ratio Optimization (PCA model)
func_sharpe = @(x) - ((muR*x) / sqrt(x'*covar_PCA*x));
[w_sharpe, fval_sharpe] = fmincon(func_sharpe, x0, [],[],Aeq,beq,lb,ub,nonlcon);
