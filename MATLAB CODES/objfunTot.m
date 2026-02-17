function err=objfunTot(theta, price_handles, price_handles28, filtered_surface, filtered_surface28, filtered_strikes,filtered_strikes28, Fwd27, Fwd29, tenor, tenor28, B, B28)

n = numel(tenor);
err=[];

for i = 1:n
        ttm = tenor(i);
        C_model = price_handles{i}(theta);
        C_mat = arrayfun(@(K,vol) B(i)*blkprice(Fwd27,K,0,ttm,vol) , filtered_strikes{i},filtered_surface{i});
        err = [err; C_model - C_mat];

end

n = numel(tenor28);
    
for i = 1:n
        ttm28 = tenor28(i);
        C_model28 = price_handles28{i}(theta);
        C_mat28 = arrayfun(@(K,vol) B28(i)*blkprice(Fwd29,K,0,ttm28,vol) , filtered_strikes28{i},filtered_surface28{i});
        err = [err; C_model28 - C_mat28];
end


        