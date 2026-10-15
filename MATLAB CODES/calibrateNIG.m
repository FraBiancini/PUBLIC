function [theta_hat, price_handles] = calibrateNIG(T, today, Fwd_today, B_today, C_mat, P_mat, all_strikes,theta0,price_handles)
% Calibra il modello NIG su tutte le expiry
% Restituisce:
%   theta_hat     : parametri calibrati [sigma, eta, k]
%   price_handles : cella di handle per pricing per expiry

    

    

    % Objective function
    f = @(theta) objfun(theta, price_handles, C_mat, P_mat, all_strikes, Fwd_today, B_today);

    % Guess iniziale e bounds
    % theta0 = [0.2, 0.5, 1.0];
    lb = [1e-4, -5, 1e-4];
    ub = [2.0, 30, 5.0];

    % Opzioni fmincon
    opts = optimoptions('fmincon', ...
        'Display','iter', ...
        'Algorithm','interior-point', ...
        'MaxFunctionEvaluations',5e4, ...
        'MaxIterations',1e3, ...
        'OptimalityTolerance',1e-8, ...
        'StepTolerance',1e-10);

    % Minimizzazione
    theta_hat = fmincon(f, theta0, [], [], [], [], lb, ub, [], opts);

    disp('Parametri NIG calibrati:');
    disp(theta_hat);
end
