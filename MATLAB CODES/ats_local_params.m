function theta3 = ats_local_params(theta5, tau)
% theta5 = [sigma, eta0, k0, beta, delta]  =>  theta3 = [sigma, eta(tau), k(tau)]
    sigma = theta5(1);
    eta0  = theta5(2);
    k0    = theta5(3);
    beta  = theta5(4);
    delta = theta5(5);

    eta_t = eta0 * tau.^delta;
    k_t   = k0   * tau.^beta;

    theta3 = [sigma, eta_t, k_t];
end
