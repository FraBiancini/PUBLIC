function m = measures(prices, method)
if(method=="Continuous")
    returns=tick2ret(prices,'Method','Continuous');
else
    returns=tick2ret(prices);
end

m.returns =returns;
m.expected_returns = mean(returns);
m.Cov_mat = cov(returns);
m.variances = (diag(m.Cov_mat))';
m.inv_V = m.Cov_mat \ eye(size(m.Cov_mat));
m.corr_mat = corrcoef(returns);



