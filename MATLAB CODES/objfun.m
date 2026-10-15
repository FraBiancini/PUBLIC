function err = objfun(theta, price_handles, vol_mat,strikes, Fwd, tenors,B)
n = numel(tenors);
err=[];
    for i = 1:n
        ttm = tenors(i);
        C_model = price_handles{i}(theta);
        C_mat = arrayfun(@(K,vol) B(i)*blkprice(Fwd,K,0,ttm,vol) , strikes{i},vol_mat{i});
        err = [err; C_model - C_mat];
    end
end