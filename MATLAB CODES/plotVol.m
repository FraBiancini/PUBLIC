function [mape,rmse] = plotVol(T, F0, B, filtered_strikes, filtered_surface, price_handles, theta)


    iv_err_all = [];
    iv_mkt_all = [];
    for j = 1:numel(filtered_strikes)
        K   = filtered_strikes{j};
        
        DF  = B(j);
        
       
        C_model = price_handles{j}(theta);
       
      
       
        vol_mkt = filtered_surface{j};
        

        
        iv_mkt   = vol_mkt;
        
        iv_model = nan(size(K));
        if(isempty(K)==0)
            for kk = 1:length(K)
                
                    % OTM call
                    if(C_model(kk)>0)
                    iv_model(kk) = blkimpv(F0, K(kk), -log(DF)/T(j), T(j), C_model(kk), 'Class', {'Call'});
                    end
                       
            end
        end
        % Grafico separato per ogni expiry
        
        iv_err_all = [iv_err_all,(iv_model - iv_mkt)'];
        iv_mkt_all = [iv_mkt_all,iv_mkt'];

        figure(j);
        plot(K, iv_mkt, 'o-', 'LineWidth', 1.5, 'DisplayName', 'Market');
        hold on;
        plot(K, iv_model, '--', 'LineWidth', 2, 'DisplayName', 'Model NIG');
        hold off;
        xlabel('Strike');
        ylabel('Implied Volatility');
        title(sprintf('Call Prices - Tenor %.2fY', T(j)));
        legend('show');
        grid on;
    end


nExp = numel(filtered_strikes);

K_common = unique( vertcat( filtered_strikes{:} ) );
K_common = K_common(:);   % forza colonna


IV_mkt   = nan(nExp, numel(K_common));
IV_model = nan(nExp, numel(K_common));

for j = 1:nExp
    K = filtered_strikes{j};
    DF = B(j);
    C_model = price_handles{j}(theta);
    vol_mkt = filtered_surface{j};

    iv_mod = nan(size(K));
    for kk = 1:numel(K)
        if C_model(kk) > 0
            iv_mod(kk) = blkimpv( ...
                F0, K(kk), -log(DF)/T(j), T(j), C_model(kk), ...
                'Class', {'Call'});
        end
    end

    % Mappo gli strike locali nella griglia globale
    [~, ia, ib] = intersect(K, K_common);

    IV_mkt(j, ib)   = vol_mkt(ia);
    IV_model(j, ib) = iv_mod(ia);
end
[KK, TT] = meshgrid(K_common, T);

figure;
surf(KK, TT, IV_mkt);
shading interp;
title('Market Vol Surface');
%hold on
figure;
surf(KK, TT, IV_model);
shading interp;
title('Model Vol Surface');


figure;
surf(KK, TT, IV_mkt);
shading interp;
hold on
surf(KK, TT, IV_model);
shading interp;

rmse = sqrt(mean(iv_err_all.^2, 'omitnan'));
mape = mean(abs(iv_err_all ./ iv_mkt_all), 'omitnan') * 100;

end