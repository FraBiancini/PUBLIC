function model = makeNIGModelHandles(Dt, F0, B, alpha)
% Returns parameterized handles for NIG model with theta = [sigma, eta, k].
% Uses your Lewis/FFT pricing function 'PriceCallOption' and sampler 'SimulateFromCF'.
    
    % Shift/damping from your example, computed from theta inside phi
    function a = damping_from_theta(theta)
        sigma = theta(1); eta = theta(2); k = theta(3);
        p = (1/2 + eta) + sqrt((1/2 + eta)^2 + 2 * (1 - alpha) / (sigma^2 * k));
        a = p / 2;
    end

    % Laplace exponent L(z; theta)
    function Lz = L_of_z(z, theta)
        sigma = theta(1); k = theta(3);
        if alpha == 1/2
            Lz = exp(Dt / k * (1 - sqrt(1 + 2 * k * z * sigma^2)));
        else
            Lz = exp(-Dt / k * log(1 + k * z * sigma^2)); % alternative parametrization
        end
    end

    % Characteristic function phi(u; theta) under risk-neutral
    model.phi = @(u, theta) ...
        exp(-1i * u * log(L_of_z(theta(2), theta))) .* ...                 % martingale adjustment
        L_of_z(((u).^2 + 1i * u * (1 + 2 * theta(2))) / 2, theta);         % core CF

    % Pricing via Lewis/FFT (uses your helper PriceCallOption)
    model.price_calls = @(K, theta, M, dz) ...
        PriceCallOption(K, F0, B, @(u) model.phi(u, theta), M, dz, damping_from_theta(theta));

    % Monte Carlo pricing via CF sampler (optional)
    model.price_calls_mc = @(K, theta, M, dz, Nsim) ...
        (B * F0) * mean(max(exp(SimulateFromCF(@(u) model.phi(u, theta), M, dz, damping_from_theta(theta), Nsim)) - K./F0, 0), 1);

    % Convenience: implied vol smile handle
    model.implied_vol = @(K, theta, M, dz) ...
        blkimpv(F0, K, 0, Dt, model.price_calls(K, theta, M, dz));
end
