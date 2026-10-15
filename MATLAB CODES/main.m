%% Project E Group 10

clear all
close all
clc

%% Read data

[prices, surface26, surface28, Disc]=read_data('DATA_FREEX.xlsx'); %read the excel file

[volClean26] = filterSurface(surface26.surface, 5, 6);
[volClean28] = filterSurface(surface28.surface, 5, 6);

strikes = surface26.strikes;
strikes_cell = repmat({strikes}, 7, 1);
surface26_cell = mat2cell(surface26.surface, ones(size(surface26.surface,1),1), size(surface26.surface,2));


%%
today = datetime(2025,11,4);

Fwd27 = prices.price(16);
T_disc= Disc.dates;


B_today = Disc.discounts;
tenor = surface26.tenor;
strikes = surface26.strikes;
vol_mat = surface26.surface;
zerorates = -log(B_today)./yearfrac(today,T_disc,3);
zstar = interp1(yearfrac(today,T_disc,3),zerorates,tenor);
B = exp(-zstar.*tenor);

z_y = interp1(yearfrac(today,T_disc,3),zerorates,1);
By = exp(-z_y);

[filtered_surface, filtered_strikes] = filter_surface_cells(Fwd27, tenor, strikes, volClean26, B, 0.75);

%%

price_handles = makeNIG_handles_per_expiry(tenor,Fwd27,B,filtered_strikes,17,0.00001);
theta0 = [2,-0.5,0,0.3];
thetaNIG = calibrateNIG(tenor,Fwd27,filtered_surface,filtered_strikes,theta0,price_handles,B);

%%

for i = 1:7
    C_model = price_handles{i}(thetaNIG);
    C_mat = arrayfun(@(K,vol) B(i)*blkprice(Fwd27,K,0,surface26.tenor(i),vol) , filtered_strikes{i},filtered_surface{i});
    figure(i)
    plot(filtered_strikes{i}, C_mat, 'o-', 'LineWidth', 1.5, 'DisplayName', 'Market');
    hold on;
    plot(filtered_strikes{i}, C_model, 'o-', 'LineWidth', 2, 'DisplayName', 'Model NIG');
    hold off;
    xlabel('Strike');
    ylabel('Call Price');
    title(sprintf('Call Prices 2027- Tenor %.2fY', surface26.tenor(i)));
    legend('show');
    grid on;
end


%%
price = price_contract(thetaNIG,Fwd27,300,By);

%% Plot results


[mape27,rmse27] = plotVol(tenor, Fwd27, B, filtered_strikes, filtered_surface, price_handles, thetaNIG);
fprintf('2027 surface Calibration 2027 -> RMSE: %.4f, MAPE: %.2f%%\n', rmse27, mape27);


%% calibration 2

Fwd29= prices.price(18);
tenor28 = surface28.tenor;
strikes28 = surface28.strikes;
vol_mat28 = surface28.surface;
zstar = interp1(yearfrac(today,T_disc,3),zerorates,tenor28);
B28 = exp(-zstar.*tenor28);



[filtered_surface28, filtered_strikes28] = filter_surface_cells(Fwd29, tenor28, strikes28, volClean28, B28, 0.75);
price_handles28 = makeNIG_handles_per_expiry(tenor28,Fwd29,B28,filtered_strikes28,17,0.00001);

theta0 = [2,-0.5,0,0.3];

%%
thetaNIGTot = calibrateNIGTot(tenor,tenor28,Fwd27,Fwd29,filtered_surface, filtered_surface28,filtered_strikes,filtered_strikes28,theta0,price_handles,price_handles28,B, B28);

%%
[mape2729,rmse2729] = plotVol(tenor, Fwd27, B, filtered_strikes, filtered_surface, price_handles, thetaNIGTot);
fprintf('2027 surface  Calibratition 2027-2029-> RMSE: %.4f, MAPE: %.2f%%\n', rmse2729, mape2729);


[mape29,rmse29] = plotVol(tenor28, Fwd29, B28, filtered_strikes28, filtered_surface28, price_handles28, thetaNIGTot);
fprintf('2029 surface Calibratition 2027-2029-> RMSE: %.4f, MAPE: %.2f%%\n', rmse29, mape29);

%%

for i = 1:7
    C_model = price_handles{i}(thetaNIGTot);
    C_mat = arrayfun(@(K,vol) B(i)*blkprice(Fwd27,K,0,surface26.tenor(i),vol) , filtered_strikes{i},filtered_surface{i});
    figure(i)
    plot(filtered_strikes{i}, C_mat, 'o-', 'LineWidth', 1.5, 'DisplayName', 'Market');
    hold on;
    plot(filtered_strikes{i}, C_model, 'o-', 'LineWidth', 2, 'DisplayName', 'Model NIG');
    hold off;
    xlabel('Strike');
    ylabel('Call Price');
    title(sprintf('Call Prices 2027 27-29 - Tenor %.2fY', surface26.tenor(i)));
    legend('show');
    grid on;
end
%%
for i=1:numel(tenor28)
    C_model28 = price_handles28{i}(thetaNIGTot);
    C_mat28 = arrayfun(@(K,vol) B28(i)*blkprice(Fwd29,K,0,surface28.tenor(i),vol) , filtered_strikes28{i},filtered_surface28{i});
    figure(i)
    plot(filtered_strikes28{i}, C_mat28, 'o-', 'LineWidth', 1.5, 'DisplayName', 'Market');
    hold on;
    plot(filtered_strikes28{i}, C_model28, 'o-', 'LineWidth', 2, 'DisplayName', 'Model NIG');
    hold off;
    xlabel('Strike');
    ylabel('Call Price');
    title(sprintf('Call Prices 2029- Tenor %.2fY', surface28.tenor(i)));
    legend('show');
    grid on;

end

%%
ttm1Y = 1;
F0 = Fwd27;
zstar1Y = interp1(yearfrac(today,T_disc,3),zerorates,ttm1Y);
B1Y = exp(-zstar1Y.*ttm1Y);

model = makeNIGModelHandles(ttm1Y,F0,B1Y);
price_call = model.price_calls(300,thetaNIGTot,17,0.00001);
