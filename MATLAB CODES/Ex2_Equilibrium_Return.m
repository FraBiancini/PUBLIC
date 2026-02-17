function [mu_mkt, C, tau]=Ex2_Equilibrium_Return(m,weights,lambda)

w_mkt = weights;      % market weights
tau = 1/length(m.returns);
mu_mkt = lambda * m.Cov_mat * w_mkt;               % equilibrium returns
C = tau * m.Cov_mat;                              % scaled prior covariance


