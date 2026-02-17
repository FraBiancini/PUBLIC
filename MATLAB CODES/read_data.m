function [prices , dates]=read_data(filename)

table_prices = readtable(filename);
prices = table2array(table_prices(1:end,2:end));
dates = table2array(table_prices(1:end,1));

