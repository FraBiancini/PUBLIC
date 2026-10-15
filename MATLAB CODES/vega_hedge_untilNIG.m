function [daily_pnl,Q, MtM_start,MtM_end,N_delta_hedge] = vega_hedge_untilNIG(dataset)

m = numel(dataset);
Q = zeros(m,10);
MtM_start = zeros(1,m);
MtM_end = zeros(1,m);
daily_pnl = zeros(1,m);
N_delta_hedge = zeros(1,m);
theta0 = [0.2, 0.5, 1.0];
 
for j = 1:m
    today = dataset(j);
       
        
        cutoffDate = datetime('2018-02-17','InputFormat','yyyy-MM-dd');
        [B_today, Fwd_today, all_strikes, C_mat, P_mat, Spot_today, mkt_today, T] = loadMarket(today, cutoffDate);


        price_handles = makeNIG_handles_per_expiry(T, today, Fwd_today, B_today, 1/2, all_strikes, 15, 0.001);
        
        thetaNIG_hat = calibrateNIG(T, today, Fwd_today, B_today, C_mat, P_mat, all_strikes,theta0);
        theta0 = thetaNIG_hat;
        idx_plot_ATS = 1:numel(all_strikes);
        VolSurf_idxplot = plotVolSmiles(idx_plot_ATS, T, today, Fwd_today, B_today, all_strikes, C_mat,P_mat, price_handles, thetaNIG_hat, 'NIG');
        Notional = 15*1e6;
        Nsim = 5*1e6;
        rng(357);
        [price_MiamiCroco, CI_low, CI_high,n_delta_hedge] = MiamiCrocodilePrice(thetaNIG_hat, today, T, B_today,Fwd_today,Spot_today, Nsim,Spot_today*8e-3,'NIG');
        
        
        bumpSize = mean(VolSurf_idxplot{4})*5e-2;
        if(j>1)
            MtM_start(1,j) = computeMTMstart(...
                today,price_MiamiCroco,Spot_today,n_spot, ...
                    Fwd_today,T,all_strikes,q_straddle,C_mat,P_mat,K,VolSurf_idxplot,B_today);
            daily_pnl(1,j) = MtM_start(1,j)-MtM_end(1,j-1)
        end
        vega_exp = computeExpiryVegas(today, all_strikes, T, Fwd_today,B_today, VolSurf_idxplot, bumpSize, Spot_today, Nsim, price_MiamiCroco,price_handles,theta0,C_mat,P_mat);

        [q_straddle,delta_straddle] = hedgeWithStraddleVolSurface(today,vega_exp,Fwd_today,B_today,T,all_strikes,VolSurf_idxplot,Spot_today,C_mat,P_mat);
        n_spot = round(n_delta_hedge-sum(q_straddle*delta_straddle'));
        % Q(j,1:length(q_straddle)) = q_straddle;
        N_delta_hedge(1,j) = n_spot;



        [mtm_total,mtm_cert,mtm_spot,mtm_straddle,K] = computeMTMend( ...
        price_MiamiCroco,Spot_today,n_spot, ...
        Fwd_today,T,all_strikes,q_straddle,C_mat,P_mat);

        MtM_end(1,j) = mtm_total;


end


end

