function err = pnl_metric(daily_pnl)
T = length(daily_pnl);
daily_pnl = daily_pnl.^2;
err = sum(daily_pnl) / T;
end