function [ptf_mon,dates_mon] = Ex5_Monthly_Readjusted_PTF(in_sample_dates,prices_in_sample,x0)

prev = 1;
i_signed  = 1;
ptf_mon = x0;
dates_mon=in_sample_dates(1);
w_mon = [0.15,0.15,0.15,0.15,0.0375,0.0375,0.0375,0.0375,0.0375,0.0375,0.0375,0.0375,0.025,0.025,0.025,0.025]';

for i=1:length(in_sample_dates)
    months = month(in_sample_dates(i));
    if( months - prev ~= 0)
        m_mon = measures(prices_in_sample((i_signed:i),:),"Continuous");
        sh_rt = m_mon.expected_returns./sqrt(m_mon.variances);
        [~,I] = sort(sh_rt,'descend');
        [~,I1] = sort(I);
        ptf_mon = [ptf_mon,0.75*w_mon(I1)+0.25*ptf_mon(:,end)];
        dates_mon=[dates_mon, in_sample_dates(i)];
        i_signed = i;
    end
    prev = months;
end

