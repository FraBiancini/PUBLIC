function E = compute_ERC(w,e,V)

num = w.*V*w;
den = w'*V*w;
num = num( num > 0);
theta = abs(num)./den;
E = -sum(theta .* log(theta));