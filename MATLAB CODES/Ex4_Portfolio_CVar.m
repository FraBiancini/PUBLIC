function  [w_CVaR] = Ex4_Portfolio_CVar(m, alpha)

t = size(m.returns, 1); % number of scenarios
N = size(m.returns, 2); % number of assets

% Variables: [w(1:N), eta, ui(1:t)]
f = [zeros(N,1); 1; (1/((1-alpha)*t)) * ones(t,1)];

% Linear constraints
A1 = [-m.returns, -ones(t,1), -eye(t)];
A2 = [zeros(t,N+1), -eye(t)];
A = [A1; A2];
b = zeros(2*t,1);

% Sum w = 1
Aeq = [ones(1,N), 0, zeros(1,t)];
beq = 1;

% Bounds
lb = [zeros(N,1); -Inf; zeros(t,1)];
ub = [0.25*ones(N,1); Inf; Inf(t,1)];

% Initial vector
x0 = [ones(N,1)/N; 0; zeros(t,1)];

options = optimoptions('fmincon','Display','iter','Algorithm','interior-point');

% Call optimizer
[x, ~] = fmincon(@(x) f'*x, x0, A, b, Aeq, beq, lb, ub, @(x) nonlinC(x,m), options);

% Extract optimal weights
w_CVaR = x(1:N);

% Compute CVaR value
eta = x(N+1);
u = x(N+2:end);
CVaR_value = eta + (1/((1-alpha)*t))*sum(u);

disp(['Optimized CVaR (95%): ', num2str(CVaR_value)]);

end

function [c, ceq] = nonlinC(x, m)
    annual_factor=252;
    N = size(m.returns, 2);
    w = x(1:N);
    ceq = sqrt(annual_factor*(w' * m.Cov_mat * w)) - 0.1;   
    c = [];
end

