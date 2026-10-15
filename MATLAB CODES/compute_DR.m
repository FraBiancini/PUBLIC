function DR = compute_DR(w,e,V)

sigma = sqrt(diag(V));
num = w'*sigma;
den = sqrt (w'*V*w);
DR = num./den;