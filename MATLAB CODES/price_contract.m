function price = price_contract(theta,Fwd,K,B)
n = 100;
alpha = 0.5;
sigma = theta(1); eta = theta(2); k = theta(3); Y = theta(4);
p = (1/2 + eta) + sqrt((1/2 + eta)^2 + 2 * (1 - alpha) / (sigma^2 * k));
a = p / (2*Y);
Nsim = 1e6;
phi = {};
phi = cell(1,n);
for i = 1:n
    model = makeNIGModelHandles(i/n,Fwd,1);
    phi{i} = model.phi;
end

df = zeros(n,Nsim);
df(1,:) = SimulateFromCF(@(u) phi{1}(u,theta),16,0.0001,a,Nsim)';
for i = 1:n-1
    df(i,:) = SimulateFromCF(@(u) phi{i+1}(u,theta)/phi{i}(u,theta),16,0.0001,a,Nsim)';
    

end

f_t = cumsum(df,1);
maxf_t = max(f_t);

price_sim = Fwd*B*max(0,exp(maxf_t)-K/Fwd);
price = mean(price_sim);
end