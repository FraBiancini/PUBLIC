function [w_Entropy,w_DR] = Ex3_Diversification_Based_Optimization(m,groups,nm)

def_ass=strcmp(groups, 'Defensive')';
neu_ass=strcmp(groups, 'Neutral')';
cyc_ass=strcmp(groups, 'Cyclical')';
NumAssets=16;
x0 = ones(NumAssets,1)/NumAssets;
fun_DR = @(w) -compute_DR(w,m.expected_returns,m.Cov_mat);
fun_ERC = @(w) -compute_ERC(w,m.expected_returns,m.Cov_mat);
lb = zeros(1,NumAssets);
ub = 0.25*ones(1,NumAssets);
A=[def_ass;-cyc_ass];
b=[0.5;-0.2];
Aeq = ones(1,NumAssets);
beq = 1;
w_DR = fmincon(fun_DR,x0,A,b,Aeq,beq,lb,ub);
w_Entropy = fmincon(fun_ERC,x0,A,b,Aeq,beq,lb,ub);

Volatility = @(w) sqrt(252.*w'*m.Cov_mat*w);
SR = @(w) w'*m.expected_returns' .*252./ Volatility(w);

Metrics = [compute_DR(w_DR,m.expected_returns,m.Cov_mat), Volatility(w_DR), SR(w_DR), w_DR'*w_DR;
    compute_DR(w_Entropy,m.expected_returns,m.Cov_mat), Volatility(w_Entropy), SR(w_Entropy), w_Entropy'*w_Entropy;
    compute_DR(x0,m.expected_returns,m.Cov_mat), Volatility(x0), SR(x0), x0'*x0];

RowNames = {'Portfolio G (DR)', 'Portfolio H (ERC)', 'Benchmark (EW)'};
VarNames = {'Diversification Ratio', 'Volatility', 'Sharpe Ratio', 'Herfindal Index'};

ResultsTable = table(Metrics(:,1), Metrics(:,2), Metrics(:,3), Metrics(:,4), ...
    'RowNames', RowNames, 'VariableNames', VarNames);

disp('---------- Diversification Comparison ----------');
disp(ResultsTable);