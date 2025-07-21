clc;
clearvars;
filename = 'MarketData_12.xlsx';
% spot
Spot = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B4:B4');

% expiries of the LV matrix
T = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B6:I6');

DiscFact= readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B7:I7');

Fwd = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B8:I8');

[r,q]=calibrate_r_q(Spot,T,DiscFact,Fwd);


% LV strikes
K = readmatrix(filename, 'Sheet', 'E CORP', 'Range', 'B24:I30');

% LV matrix
V=[0.195476283515957,0.379772625451878,0.455386379494530,0.357671837516963,0.390327744493720,0.352952281548773,0.377894795565677,0.338738474749795;0.0957440825575601,0.154258948070803,0.175033273399337,0.206110623767820,0.223673911745795,0.219065566550840,0.222370555075528,0.206276336696954;0.102700030916652,0.123601425059334,0.148192842697685,0.165981210639962,0.175276001254465,0.183573954985500,0.195875411173798,0.189634109201290;0.0978648707912609,0.116267256500267,0.131788013868629,0.156475151791944,0.162936405139269,0.171934707429229,0.182042115161874,0.183398613391302;0.0933746827324163,0.111401592334084,0.125216299929212,0.136371081148767,0.152535930375864,0.163648430506965,0.175632998867256,0.183329285827324;0.0936590824673852,0.105416175601035,0.118730605260143,0.124943240483760,0.141571917857593,0.148323466775238,0.169657940443727,0.188610598203197;0.0934767428677501,0.103872433444586,0.126734910214556,0.111906015652444,0.161343501773318,0.171810078249219,0.186336627258255,0.204724622459494];
% option data
expiry = 0.5;
strike = 0.9*Spot;

% solve dupire: find prices C of options with given expiry and strikes k
% normalized market strikes
Lt = 100; Lh = 1000; K_min = 0.01; K_max = 3.5; scheme = 'cn';
[ k, C ] = solve_dupire(T,K,V,expiry,Lt,Lh,K_min,K_max,scheme);

% normalized option price
fwd_at_expiry = forward(Spot,T,r,q,expiry);
norm_strike = strike/fwd_at_expiry;
norm_price = interp1(k,C,norm_strike);
model_impl_vol_1 = blsimpv(1,norm_strike,0,expiry,norm_price);

% option price
disc_fact_at_expiry = discount(T,r,expiry);
price = fwd_at_expiry * disc_fact_at_expiry * norm_price;
model_impl_vol_2 = blsimpv(fwd_at_expiry,strike,0,expiry,price/disc_fact_at_expiry);

