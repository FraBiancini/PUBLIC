function Bt = getDisc(today,dates,B,t)
zerorates = -log(B)'./yearfrac(today,dates,2);
z = interp1(dates,zerorates,t);
Bt = exp(-z .* yearfrac(today, t, 2));
end