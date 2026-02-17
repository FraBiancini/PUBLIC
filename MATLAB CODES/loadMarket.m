function [B_today, Fwd_today, all_strikes, C_mat, P_mat, Spot_today, mkt_today, T] = loadMarket(today, cutoffDate)
% Carica il mercato alla data specificata e restituisce i dati calibrati
% filtrando le expiry da cutoffDate in poi.
%
% INPUT:
%   today      : datetime della data da analizzare
%   cutoffDate : datetime, filtra le expiry successive a questa data
%
% OUTPUT:
%   today       : la data richiesta
%   B_today     : discount factors filtrati
%   Fwd_today   : forward prices filtrati
%   all_strikes : cell array con gli strike per ogni expiry filtrata
%   C_mat       : cell array con i prezzi call (mid) filtrati
%   P_mat       : cell array con i prezzi put (mid) filtrati
%   Spot_today  : spot S&P500 alla data
%   mkt_today   : struttura completa estratta dal file
%   T           : expiry filtrate

    % --- Parametri ---
    DK = 1;                     
    weekend_mask = [1,0,0,0,0,0,1];
    tresh_bid_ask = 0.6;
    tresh_penny   = 0.1;

    % --- File Paths ---
    base_location = 'C:\Users\Utente\Desktop\Comp Finance\Hedging Competition\';
    sub_dir       = fullfile('Dati Train');
    namefile_spot = fullfile(base_location, 'spot_SPX_hist.csv');
    full_sub_path = fullfile(base_location, sub_dir);

    % --- Costruzione filename ---
    date_str   = datestr(today, 'yyyy-mm-dd');
    filename_opt = [date_str, '.csv'];
    path_opt   = fullfile(full_sub_path, filename_opt);

    % --- Check Business Day + File ---
    is_bus_day = (business_day(today, weekend_mask) == today);
    if ~is_bus_day || ~isfile(path_opt)
        error('La data %s non è business day o il file non esiste.', date_str);
    end

    % --- Option Extraction ---
    mkt_today = Option_estraction_hist(path_opt, namefile_spot, tresh_bid_ask, tresh_penny, DK);

    % --- Discount & Forward calibration ---
    [B_today, Fwd_today] = Disc_Forw_calib(mkt_today);

    % --- Spot ---
    Spot_today = mkt_today.spot;

    % --- Expiries e strikes ---
    T_all = mkt_today.datesExpiry;
    idxdates = (T_all > cutoffDate) & (yearfrac(today,T_all,3) > 30/365);
      
    shiftidx = find(idxdates);

    T        = T_all(idxdates);
    B_today  = B_today(idxdates);
    Fwd_today= Fwd_today(idxdates);

    nExp = numel(T);
    C_mat = cell(1,nExp);
    P_mat = cell(1,nExp);
    all_strikes = cell(1,nExp);

    for i = 1:nExp
        C_mat{i} = (mkt_today.callAsk(shiftidx(i)).prices + mkt_today.callBid(shiftidx(i)).prices)/2;
        P_mat{i} = (mkt_today.putAsk(shiftidx(i)).prices + mkt_today.putBid(shiftidx(i)).prices)/2;
        all_strikes{i} = mkt_today.strikes(shiftidx(i)).value;
    end
end
