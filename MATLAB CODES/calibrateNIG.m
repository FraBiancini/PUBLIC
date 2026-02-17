function theta_hat = calibrateNIG(tenors, Fwd, vol_mat, strikes,theta0,price_handles,B)
% Calibra il modello NIG su tutte le expiry
% Restituisce:
%   theta_hat     : parametri calibrati [sigma, eta, k]
%   price_handles : cella di handle per pricing per expiry

    

    

    % Objective function
    f = @(theta) objfun(theta, price_handles, vol_mat, strikes, Fwd, tenors, B);

    % Guess iniziale e bounds
    % theta0 = [0.2, 0.5, 1.0];
    lb = [1e-4, -5, 1e-4,-1];
    ub = [2.0, 30, 5.0,1];

    % Opzioni fmincon
    options = optimoptions('lsqnonlin', ...
    'Algorithm', 'trust-region-reflective', ...
    'Display', 'iter-detailed', ...
    'MaxIterations', 100, ...
    'MaxFunctionEvaluations', 5000, ...
    'FunctionTolerance', 1e-8, ...
    'StepTolerance', 1e-10, ...
    'OptimalityTolerance', 1e-8, ...
    'SpecifyObjectiveGradient', false);


    % Minimizzazione
    theta_hat = lsqnonlin(f, theta0, lb, ub,options);

    disp('Parametri NIG calibrati:');
    disp(theta_hat);
end
