function [rt_mon,sigma_mon,Calmar_ratio,sh_rt_mon,mon_drawdown] = Ex5_Time_Evaluation_PTF(m,in_sample_dates,prices_in_sample, pft_mon)

prev = 1;
i_signed  = 1;
rt_mon=m.expected_returns*pft_mon(:,1);
sigma_mon=sqrt(pft_mon(:,1)'*m.Cov_mat*pft_mon(:,1));
P_max=prices_in_sample(1,:)*pft_mon(:,1);
P_min=prices_in_sample(1,:)*pft_mon(:,1);
Calmar_ratio=[];
j=1;

for i=1:length(in_sample_dates)
    months = month(in_sample_dates(i));
    if( months - prev ~= 0)
        m_mon = measures(prices_in_sample((i_signed:i),:),"Continuous");
        sigma_mon=[sigma_mon,sqrt(pft_mon(:,j)'*m_mon.Cov_mat*pft_mon(:,j))];
        rt_mon=[rt_mon,m_mon.expected_returns*pft_mon(:,j)];
        P_max=[P_max,max(prices_in_sample(i_signed:i,:)*pft_mon(:,j))];
        P_min=[P_min,min(prices_in_sample(i_signed:i,:)*pft_mon(:,j))];
        Calmar_ratio=[Calmar_ratio, rt_mon(end)*252./(P_max(end)-P_min(end))*2];
        i_signed = i;
        j=j+1;
    end
    prev = months;
end

sh_rt_mon = rt_mon./sigma_mon;

mon_drawdown = 100.*(P_max-P_min)./P_max;
