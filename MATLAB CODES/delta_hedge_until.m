% function [daily_pnl,MtM_start,MtM_end,N_delta_hedge] = delta_hedge_until(dataset)
% 
% m = numel(dataset);
% 
% MtM_start = zeros(1,m);
% MtM_end = zeros(1,m);
% daily_pnl = zeros(1,m);
% N_delta_hedge = zeros(1,m);
% % theta0 = [0.2, 0.5, 1.0]; 
% theta0   = [0.1, 16, 1.5, 0.3, -0.0001]; 
% for j = 1:m
%     stop_date = dataset(j);
%         %% --- Simulation Settings ---
%                       % 1 for S&P 500, 0 for EUROSTOXX
%         DK = 1;                     % Minimum difference between strikes in the dataset
% 
%         % --- Date Settings ---
%         start_date = datetime('2017-06-08', 'InputFormat', 'yyyy-MM-dd');
%         % stop_date  = datetime('2017-12-08', 'InputFormat', 'yyyy-MM-dd');
%         obsDates = [ ...
%                 datetime(2018,4,9); ...
%                 datetime(2018,8,8); ...
%                 datetime(2018,12,10); ...
%                 datetime(2019,4,8); ...
%                 datetime(2019,8,8); ...
%                 datetime(2019,12,9) ...
%             ];
%         target_maturity = datetime('15-Nov-2019'); % Maturity of the option we are hedging
% 
%         % --- Physics & Thresholds ---
%         weekend_mask = [1, 0, 0, 0, 0, 0, 1]; % 1 = Weekend       
%         tresh_bid_ask = 0.6;
%         tresh_penny = 0.1;
% 
% 
% 
% 
%         % --- File Paths ---
%         % Use fullfile for robust path handling (avoids issues with '\' vs '/')
%         base_location = 'C:\Users\Utente\Desktop\Comp Finance\Hedging Competition\';
% 
% 
%         % S&P 500 Configuration
%         surf_name = "S&P";
%         sub_dir = fullfile('Dati Train');
%         namefile_spot = fullfile(base_location, 'spot_SPX_hist.csv');
%         full_sub_path = fullfile(base_location, sub_dir);
% 
%         %% 3. Simulation Loop
%         date_vec=[];
%         date_range = start_date:stop_date;
%         today = stop_date;
%         B_mat = {};
%         F_mat = {};
%         for i = length(date_range)
%             current_date = date_range(i);
% 
%             % Construct daily filename
%             date_str = datestr(current_date, 'yyyy-mm-dd');
%             filename_opt = [date_str, '.csv'];
%             path_opt = fullfile(full_sub_path, filename_opt);
% 
%             % Check if Business Day AND File Exists
%             is_bus_day = (business_day(current_date, weekend_mask) == current_date);
% 
%             if is_bus_day && isfile(path_opt)
% 
%                 date_vec=[date_vec,current_date];
% 
%                 % Option Extraction
%                 mkt = Option_estraction_hist(path_opt, namefile_spot, tresh_bid_ask, tresh_penny, DK);
%                 if(i == length(date_range) )
%                     mkt_today = mkt;
%                 end
% 
%                 [B_today,Fwd_today] = Disc_Forw_calib(mkt);
% 
%                 B_mat{end+1} = B_today;
%                 F_mat{end+1} = Fwd_today;
%             end
%         end
% 
%         %Spot_today = 2651.5;
%         Spot_today = mkt_today.spot;
%         %Spot_today = 2664.11;
%         T = mkt_today.datesExpiry;
%         nExp = length(mkt_today.datesExpiry);
%         C_mat = {};
%         P_mat = {};
%         all_strikes = {};
%         for i=1:nExp
%             C_mat{i} = (mkt_today.callAsk(i).prices + mkt_today.callBid(i).prices)/2;
%             P_mat{i} = (mkt_today.putAsk(i).prices + mkt_today.putBid(i).prices)/2;
%             all_strikes{i} = mkt_today.strikes(i).value;
%         end
%         price_handles = makeNIG_handles_per_expiry(T, today, Fwd_today, B_today, 1/2, all_strikes, 16, 0.001);
%         thetaATS_hat = calibrateATS(T, today, Fwd_today, B_today, all_strikes, C_mat, P_mat, price_handles,theta0);
%         % thetaNIG_hat = calibrateNIG(T, today, Fwd_today, B_today, C_mat, P_mat, all_strikes,theta0);
%         theta0 = thetaATS_hat;
%         idx_plot_ATS = 1:numel(all_strikes);
%         VolSurf_idxplot = plotVolSmiles(idx_plot_ATS, T, today, Fwd_today, B_today, all_strikes, C_mat,P_mat, price_handles, thetaATS_hat, 'ATS');
%         % Notional = 15*1e6;
%         Nsim = 5*1e6;
%         % rng(357);
%         [price_MiamiCroco, CI_low, CI_high,n_delta_hedge] = MiamiCrocodilePrice(thetaATS_hat, today, T, B_today,Fwd_today,Spot_today, Nsim,10,'ATS');
%         bumpSize = 1e-3;
% 
% 
%         if(j>1)
%             MtM_start(1,j) = -price_MiamiCroco + N_delta_hedge(1,j-1)*Spot_today;
%             daily_pnl(1,j) = MtM_start(1,j)-MtM_end(1,j-1)
%         end
% 
%         N_delta_hedge(1,j) = round(n_delta_hedge);
% 
% 
%         mtm_total = -price_MiamiCroco+N_delta_hedge(1,j)*Spot_today;
% 
%         MtM_end(1,j) = mtm_total;
% 
% 
% end
% 
% 
% end

function [daily_pnl, MtM_start, MtM_end, N_delta_hedge] = delta_hedge_until(dataset)

m = numel(dataset);

MtM_start     = zeros(1,m);
MtM_end       = zeros(1,m);
daily_pnl     = zeros(1,m);
N_delta_hedge = zeros(1,m);

% Stato portafoglio iniziale
q_cert = -1;   % sei short 1 certificato
q_spot = 0;    % nessuna posizione spot iniziale

% Warm start parametri ATS
theta0 = [0.1, 16, 1.5, 0.3, -0.0001];

% Variabili per salvare prezzi del giorno precedente
price_yday = NaN;
Spot_yday  = NaN;

for j = 1:m
    stop_date = dataset(j);

    %% --- Carica mercato del giorno j ---
   stop_date = dataset(j);
        %% --- Simulation Settings ---
                      % 1 for S&P 500, 0 for EUROSTOXX
        DK = 1;                     % Minimum difference between strikes in the dataset

        % --- Date Settings ---
        start_date = datetime('2017-06-08', 'InputFormat', 'yyyy-MM-dd');
        % stop_date  = datetime('2017-12-08', 'InputFormat', 'yyyy-MM-dd');
        obsDates = [ ...
                datetime(2018,4,9); ...
                datetime(2018,8,8); ...
                datetime(2018,12,10); ...
                datetime(2019,4,8); ...
                datetime(2019,8,8); ...
                datetime(2019,12,9) ...
            ];
        target_maturity = datetime('15-Nov-2019'); % Maturity of the option we are hedging

        % --- Physics & Thresholds ---
        weekend_mask = [1, 0, 0, 0, 0, 0, 1]; % 1 = Weekend       
        tresh_bid_ask = 0.6;
        tresh_penny = 0.1;




        % --- File Paths ---
        % Use fullfile for robust path handling (avoids issues with '\' vs '/')
        base_location = 'C:\Users\Utente\Desktop\Comp Finance\Hedging Competition\';


        % S&P 500 Configuration
        surf_name = "S&P";
        sub_dir = fullfile('Dati Train');
        namefile_spot = fullfile(base_location, 'spot_SPX_hist.csv');
        full_sub_path = fullfile(base_location, sub_dir);

        %% 3. Simulation Loop
        date_vec=[];
        date_range = start_date:stop_date;
        today = stop_date;
        B_mat = {};
        F_mat = {};
        for i = length(date_range)
            current_date = date_range(i);

            % Construct daily filename
            date_str = datestr(current_date, 'yyyy-mm-dd');
            filename_opt = [date_str, '.csv'];
            path_opt = fullfile(full_sub_path, filename_opt);

            % Check if Business Day AND File Exists
            is_bus_day = (business_day(current_date, weekend_mask) == current_date);

            if is_bus_day && isfile(path_opt)

                date_vec=[date_vec,current_date];

                % Option Extraction
                mkt = Option_estraction_hist(path_opt, namefile_spot, tresh_bid_ask, tresh_penny, DK);
                if(i == length(date_range) )
                    mkt_today = mkt;
                end

                [B_today,Fwd_today] = Disc_Forw_calib(mkt);

                B_mat{end+1} = B_today;
                F_mat{end+1} = Fwd_today;
            end
        end

        %Spot_today = 2651.5;
        Spot_today = mkt_today.spot;
        %Spot_today = 2664.11;
        T = mkt_today.datesExpiry;
        nExp = length(mkt_today.datesExpiry);
        C_mat = {};
        P_mat = {};
        all_strikes = {};
        for i=1:nExp
            C_mat{i} = (mkt_today.callAsk(i).prices + mkt_today.callBid(i).prices)/2;
            P_mat{i} = (mkt_today.putAsk(i).prices + mkt_today.putBid(i).prices)/2;
            all_strikes{i} = mkt_today.strikes(i).value;
        end

    % Calibra modello ATS
    price_handles = makeNIG_handles_per_expiry(T, stop_date, Fwd_today, B_today, 1/2, all_strikes, 16, 0.001);
    thetaATS_hat  = calibrateATS(T, stop_date, Fwd_today, B_today, all_strikes, C_mat, P_mat, price_handles, theta0);
    theta0        = thetaATS_hat; % warm start per il giorno successivo

    % Prezzo certificato e delta rispetto allo spot
    Nsim = 2e5;
    bumpSpot = 0.005 * Spot_today; % bump di 0.5% dello spot
    [price_today, ~, ~, delta_spot] = MiamiCrocodilePrice(thetaATS_hat, stop_date, T, B_today, Fwd_today, Spot_today, Nsim, bumpSpot, 'ATS');

    %% --- Calcolo MtM e PnL ---
    if j == 1
        % Giorno iniziale: costruisci hedge
        q_spot = +delta_spot;   % hedge delta: se sei short cert con delta+, compri spot
        MtM_end(j) = q_cert * price_today + q_spot * Spot_today;
        N_delta_hedge(j) = q_spot;
    else
        % MtM con posizioni di ieri
        MtM_start(j) = q_cert * price_yday + q_spot * Spot_yday;
        MtM_end(j)   = q_cert * price_today + q_spot * Spot_today;

        % PnL giornaliero = variazione valore vecchie posizioni
        daily_pnl(j) = MtM_end(j) - MtM_start(j);

        % Re-hedge: aggiorna posizioni spot per domani
        q_spot = +delta_spot;
        N_delta_hedge(j) = q_spot;
    end

    % Salva prezzi per giorno successivo
    price_yday = price_today;
    Spot_yday  = Spot_today;
end

end
