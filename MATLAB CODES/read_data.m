function [prices, surface26, surface28, Disc]=read_data(filename)
% Function build for read the excel 'DATA_FREEX.xlsx'

%% First sheet
prices = struct();
table1 = readtable(filename, 'Sheet',1);
prices.expiries = table2array(table1(1:end,1));
prices.names = table2array(table1(1:end,2));
prices.price = table2array(table1(1:end,3));


%% Second sheet
surface26 = struct();
table2 = readtable(filename, 'Sheet',2);
surface26.tenor = table2array(table2(2:end,1));
surface26.strikes = table2array(table2(1, 2:end));
surface26.surface = table2array(table2(2:end, 2:end));


%% Third sheet
surface28 = struct();
table3 = readtable(filename, 'Sheet',3);
surface28.tenor = table2array(table3(2:end,1));
surface28.strikes = table2array(table3(1, 2:end));
surface28.surface = table2array(table3(2:end, 2:end));


%% Fourth sheet
Disc = struct();
table4 = readtable(filename, 'Sheet',4);
Disc.dates = table2array(table4(1,:));
Disc.discounts = table2array(table4(2,:));
Disc.dates = datetime(Disc.dates, 'ConvertFrom', 'excel', 'Format', '1904');
Disc.dates.Format = 'dd/MM/yyyy';