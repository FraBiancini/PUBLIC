function theta_hat = calibrateNIGTot(tenor,tenor28,Fwd27,Fwd29,filtered_surface, filtered_surface28,filtered_strikes,filtered_strikes28,theta0,price_handles,price_handles28,B,B28)

% Calibra il modello NIG su tutte le expiry
% Restituisce:
%   theta_hat     : parametri calibrati [sigma, eta, k]
%   price_handles : cella di handle per pricing per expiry

    

    

    % Objective function
    f = @(theta) objfunTot(theta, price_handles, price_handles28, filtered_surface, filtered_surface28, filtered_strikes,filtered_strikes28, Fwd27, Fwd29, tenor, tenor28, B, B28);

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
