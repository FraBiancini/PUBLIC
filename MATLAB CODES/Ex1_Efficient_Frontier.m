function [w_MVP , w_sh, FrontierRet, FrontierVola] = Ex1_Efficient_Frontier(m,groups)
NumAssets=16;
N = 100000;
RetPtfs = zeros(1,N);
VolaPtfs = zeros(1,N);
SharpePtfs = zeros(1,N);

for n = 1:N
    w = rand(1,NumAssets);
    w = w./sum(w); % normalize weights

    RetPtfs(n) = w*m.expected_returns';
    VolaPtfs(n) = sqrt(w*m.Cov_mat*w');
    SharpePtfs(n) = RetPtfs(n)/VolaPtfs(n);
end

fun = @(x) x'*m.Cov_mat*x;                      % objective = variance
ret_range = linspace(min(RetPtfs), max(RetPtfs),100);
x0 = ones(NumAssets,1)/NumAssets;      % initial guess = equal weights
lb = zeros(1,NumAssets);               % lower bound
ub = 0.3*ones(1,NumAssets);                % upper bound
def_ass=strcmp(groups, 'Defensive')';
neu_ass=strcmp(groups, 'Neutral')';
cyc_ass=strcmp(groups, 'Cyclical')';
A=[def_ass;-neu_ass];% constraints on defensive and neutral assets
b=[0.45;-0.20];% constraints on defensive and neutral assets


FrontierVola = zeros(1,length(ret_range));
FrontierRet  = zeros(1,length(ret_range));
w_opt = zeros(NumAssets,length(ret_range));

for i = 1:length(ret_range)
    r = ret_range(i);
    Aeq = [ones(1,NumAssets); m.expected_returns]; % constraints: sum weights =1, target return
    beq = [1; r];
    w_opt(:,i) = fmincon(fun, x0, A, b, Aeq, beq, lb, ub);
    FrontierVola(i) = sqrt(w_opt(:,i)'*m.Cov_mat*w_opt(:,i));
    FrontierRet(i)  = w_opt(:,i)'*m.expected_returns';
end


%%
[~,i_MVP]=min(FrontierVola);
w_MVP=w_opt(:,i_MVP); 

FrontierSharp=FrontierRet./FrontierVola;
[~,i_sh]=max(FrontierSharp);
w_sh=w_opt(:,i_sh);