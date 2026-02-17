function theta_hat = calibrateATS(T, stop_date, Fwd_today, B_today, all_strikes, C_mat, P_mat,price_handles,theta0)
% Calibra il modello ATS power-law [sigma, eta0, k0, beta, delta]
% Restituisce theta_hat


    % --- Guess iniziale e bounds ---
    alpha    = 1/2;
    beta_max = 1 / (1 - alpha/2);           % vincolo teorico su beta
   
    lb = [1e-4, -5.0, 1e-4, 0.0,  -2.0];
    ub = [ 2.0,  50.0, 20.0,  beta_max, 0.0];
   

    % --- Opzioni fmincon ---
    opts = optimoptions('fmincon', 'Display','iter', 'Algorithm','interior-point', ...
        'MaxFunctionEvaluations',1e3, 'MaxIterations',2e3, ...
        'OptimalityTolerance',1e-6, 'StepTolerance',1e-7);
    % opts = optimoptions('fmincon',...
    % 'Algorithm','sqp',...
    % 'MaxIterations',200,...
    % 'MaxFunctionEvaluations',1000,...
    % 'TolFun',1e-4,...
    % 'TolX',1e-4,...
    % 'Display','none');
    % --- Minimizzazione ---
    theta_hat = fmincon(@(th) objfun_ATS_otm(th, price_handles, C_mat, P_mat, all_strikes, Fwd_today, B_today, T, stop_date), ...
                        theta0, [], [], [], [], lb, ub, @(th) ats_constraints(th, alpha), opts);

    % --- Risultato ---
    disp('ATS power-law calibrato [sigma, eta0, k0, beta, delta]:');
    disp(theta_hat);
end

% --- Vincoli teorici su beta e delta ---
function [c, ceq] = ats_constraints(theta5, alpha)
    beta  = theta5(4);
    delta = theta5(5);

    % Lower bound teorico di delta
    if alpha == 0
        lower_delta = -beta;  % VG
    else
        lower_delta = -min(beta, (1 - beta*(1 - alpha))/alpha);
    end

    beta_max = 1 / (1 - alpha/2);

    % c(x) <= 0
    c = [
        beta - beta_max;        % beta <= beta_max
        -(delta - lower_delta); % delta >= lower_delta
        % delta <= 0 è gestito da ub
    ];
    ceq = [];
end
