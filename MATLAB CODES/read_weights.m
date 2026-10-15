function weights = read_weights(filename)
    
    % Read the CSV file into a table
    table_weights = readtable(filename);
    
    weights = table2array(table_weights(:,3));
    
end
