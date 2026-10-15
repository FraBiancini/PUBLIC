function [daily_pnl,var_cert,daily_pnl_delta] = vega_hedge_until(dataset)

m = numel(dataset);
MtM_start = zeros(1,m);
MtM_end = zeros(1,m);
daily_pnl = zeros(1,m);
daily_pnl_delta = zeros(1,m);
N_delta_hedge = zeros(1,m);
theta0   = [0.1, 16, 1.5, 0.3, -0.0001]; 



% straddle_dates = [datetime(2018,9,21)
%     datetime(2018,12,21)
%     datetime(2019,1,18)
%     datetime(2019,6,21)
%     datetime(2019,12,20)];
obsDates = [ ...
        datetime(2018,4,9); ...
        datetime(2018,8,8); ...
        datetime(2018,12,10); ...
        datetime(2019,4,8); ...
        datetime(2019,8,8); ...
        datetime(2019,12,9) ...
    ];
var_cert = zeros(1,m);

for j = 1:m
    today = dataset(j);
    
       
        
        cutoffDate = datetime('2018-01-16','InputFormat','yyyy-MM-dd');
        [B_today, Fwd_today, all_strikes, C_mat, P_mat, Spot_today, ~, T] = loadMarket(today, cutoffDate);

        if(j == 1)
            SpotStart = Spot_today;
        end
        N_available = numel(T);
        if(N_available < 5)
            selected_dates = T;
        else
            selected_dates = T(end-4:end);
            
        end
        min_days_to_exp = 30;
        time_to_exp = days(selected_dates-today);
        straddle_dates = selected_dates(time_to_exp>min_days_to_exp);
        
        
        price_handles = makeNIG_handles_per_expiry(T, today, Fwd_today, B_today, 1/2, all_strikes, 15, 0.001);
        thetaATS_hat = calibrateATS(T, today, Fwd_today, B_today, all_strikes, C_mat, P_mat, price_handles,theta0);
        theta0 = thetaATS_hat;

        idx_plot_ATS = 1:numel(all_strikes);
        VolSurf_idxplot = plotVolSmiles(idx_plot_ATS, T, today, Fwd_today, B_today, all_strikes, C_mat,P_mat, price_handles, thetaATS_hat, 'ATS');

        
        Nsim = 5e6;
        rng(357);
        [price_MiamiCroco, C1, C2,n_delta_hedge] = MiamiCrocodilePrice(thetaATS_hat, today, T, B_today,Fwd_today,SpotStart, Nsim,Spot_today*8e-3,'ATS');
        
        
        bumpSize = mean(VolSurf_idxplot{4})*5e-3;
        if(j>1)
            [MtM_start(1,j),ps] = computeMTMstart(...
                today,price_MiamiCroco,Spot_today,n_spot, ...
                    Fwd_today,T,all_strikes,q_straddle,C_mat,P_mat,K,VolSurf_idxplot,straddle_dates_prec,B_today,thetaATS_hat);
            daily_pnl(1,j) = MtM_start(1,j)-MtM_end(1,j-1);
            daily_pnl_delta(1,j) = (ps-pe) + m*(Spot_today-Spot_yesterday);
            var_cert(1,j) = ps-pe;
            if(ismember(today,obsDates) && (Spot_today >= SpotStart)) %autocall
                break
            end
            
        end
        vega_exp = computeExpiryVegas(today, all_strikes, T, Fwd_today,B_today, VolSurf_idxplot, bumpSize, SpotStart, Nsim, price_MiamiCroco,price_handles,theta0,straddle_dates);

        [q_straddle,delta_straddle] = hedgeWithStraddleVolSurface(today,vega_exp,Fwd_today,T,all_strikes,VolSurf_idxplot,Spot_today,C_mat,P_mat,straddle_dates);
        n_spot = round(n_delta_hedge-sum(q_straddle*delta_straddle'));
        N_delta_hedge(1,j) = n_spot;
        m = n_delta_hedge;
        Spot_yesterday = Spot_today;


        [mtm_total,pe,~,~,K] = computeMTMend( ...
        price_MiamiCroco,Spot_today,n_spot, ...
        Fwd_today,T,all_strikes,q_straddle,C_mat,P_mat,straddle_dates);

        MtM_end(1,j) = mtm_total;
        straddle_dates_prec = straddle_dates;


end


end

