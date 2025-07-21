clc;
clearvars;

filename = 'MarketData_12.xlsx';

% Import "Spot"
Spot = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B4:B4');

% Import "T"
T = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B6:I6');

% Import "DiscFact" (discount rates at LV expiries)
DiscFact= readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B7:I7');

% Import "Fwd"
Fwd = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B8:I8');

% Import "K"
K = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B24:I30');

% Import "V" as computed in point 1.1
V=[0.195476283515957,0.379772625451878,0.455386379494530,0.357671837516963,0.390327744493720,0.352952281548773,0.377894795565677,0.338738474749795;0.0957440825575601,0.154258948070803,0.175033273399337,0.206110623767820,0.223673911745795,0.219065566550840,0.222370555075528,0.206276336696954;0.102700030916652,0.123601425059334,0.148192842697685,0.165981210639962,0.175276001254465,0.183573954985500,0.195875411173798,0.189634109201290;0.0978648707912609,0.116267256500267,0.131788013868629,0.156475151791944,0.162936405139269,0.171934707429229,0.182042115161874,0.183398613391302;0.0933746827324163,0.111401592334084,0.125216299929212,0.136371081148767,0.152535930375864,0.163648430506965,0.175632998867256,0.183329285827324;0.0936590824673852,0.105416175601035,0.118730605260143,0.124943240483760,0.141571917857593,0.148323466775238,0.169657940443727,0.188610598203197;0.0934767428677501,0.103872433444586,0.126734910214556,0.111906015652444,0.161343501773318,0.171810078249219,0.186336627258255,0.204724622459494];

%Calculate q and r
[r,q]=calibrate_r_q(Spot,T,DiscFact,Fwd);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRICING SPOT START CALL OPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
% spot start option data
Expiry = 0.5;

% model forwards at T
Fwd = zeros(1,length(T));
for i=1:length(T)
   Fwd(i) = forward(Spot,T,r,q,T(i));
end

% normalized LV strikes
[rows, cols] = size(K);
K_norm = K ./ repmat(Fwd, rows, 1);

% Dupire solver details
Lt = 10;
Lh = 200;
K_min = 0.1;
K_max = 3;
Scheme = 'cn';

% compute price of spot-start call options 
[ k, C ] = solve_dupire( T, K_norm, V, Expiry, Lt, Lh, K_min, K_max, Scheme);    

% compute model implied volatilities
perc_strikes_spot_start=[];
model_impl_vol_spot_start=[];
for i=1:length(k)
    if k(i)>0.8 && k(i)<1.2
        perc_strikes_spot_start = [perc_strikes_spot_start k(i)];
        model_impl_vol_spot_start = [model_impl_vol_spot_start blsimpv(1,k(i),0,Expiry,C(i))];
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRICING FORWARD START CALL OPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% forward-start option data
expiry(1) = 2;
expiry(2) = expiry(1) + 0.5;

% additional market data
discount_factor = discount(T,r,expiry(2));
fwd(1) = forward(Spot,T,r,q,expiry(1));
fwd(2) = forward(Spot,T,r,q,expiry(2));

N=1000000; %MC simulations
M=100; %timesteps

% MC simulation
S = lv_simulation_log(T,Spot,r,q,V,K,N,M,expiry);

% option prices
perc_strikes = 0.8:0.05:1.2;
model_impl_vol=[];
for x = perc_strikes
    P = discount_factor*mean(max(S(2,:) - x*S(1,:),0));
    model_impl_vol = [model_impl_vol, blsimpv(fwd(2),x*fwd(1),0,expiry(2)-expiry(1),P/discount_factor)];
end

plot(perc_strikes_spot_start,model_impl_vol_spot_start,perc_strikes,model_impl_vol);
title('Model implied vol');
legend('Spot impl vol','Fwd impl vol (starts in 2y)')

