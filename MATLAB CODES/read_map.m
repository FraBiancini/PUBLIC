function [groups,nm]=read_map(filename)

table_map = readtable(filename);
groups= table2array(table_map(1:end,2));
nm= table2array(table_map(1:end,1));