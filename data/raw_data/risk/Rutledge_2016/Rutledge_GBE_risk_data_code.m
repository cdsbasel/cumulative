% Risky decision and happiness task
%
% Robb Rutledge
% December 2020

load('Rutledge_GBE_risk_data.mat');


%% Does risk taking change across the lifespan?
%
% Risk taking in gain trials (e.g., +50 vs +100/0) gradually decreased with
% age. Risk taking in mixed trials (e.g., 0 vs +100/-60) showed a modest
% increase with age. Risk taking in loss trials (e.g., -50 vs 0/-100) did
% not change at all despite the large sample (n=47,067). Surprisingly, risk
% taking was highest in the small sample (n=500) that said they were over
% the age of 70. Decreased risk taking for reward with age was reported in
% a smaller early sample (n=25,189) in Rutledge et al. (2016) Current
% Biology.

for s=1:length(subjData)
    t=subjData(s).data{1}(:,1:7);
    subjData(s).gainGam  = 100*mean(t(t(:,3)>0,7));
    subjData(s).mixedGam = 100*mean(t(t(:,3)==0,7));
    subjData(s).lossGam  = 100*mean(t(t(:,3)<0,7));
end
age      = [subjData.age];
gainGam  = [subjData.gainGam]; gainGam   = gainGam(:);
mixedGam = [subjData.mixedGam]; mixedGam = mixedGam(:);
lossGam  = [subjData.lossGam]; lossGam   = lossGam(:);
ageGML = zeros(6,3); ageGMLsem = zeros(6,3);
for s = 1:6 %average risk taking for each age group
    ageGML(s,:)    = [mean(gainGam(age==s)) mean(mixedGam(age==s)) mean(lossGam(age==s))];
    ageGMLsem(s,:) = [std(gainGam(age==s)) std(mixedGam(age==s)) std(lossGam(age==s))]/sqrt(sum(age==s));
end
figure('color',[1 1 1]); hold on; errorbar(1:6,ageGML(:,1),ageGMLsem(:,1),'k'); title(sprintf('n=%d',length(age))); 
errorbar(1:6,ageGML(:,2),ageGMLsem(:,2),'g'); errorbar(1:6,ageGML(:,3),ageGMLsem(:,1),'r');
axis([0 7 50 75]); legend('Gain','Mixed','Loss'); ylabel('Gambles chosen (percent)'); xlabel('Age group');
set(gca,'ytick',50:5:75,'xtick',1:6,'xticklabel',{'18-24','25-29','30-39','40-49','50-59','60-69'});
[r,pval]=corr(age(:),gainGam(:)); %r=-0.11, p<10^-129
fprintf(1,'Age vs risk taking in gain trials (n=%d) r= %.3f, p=10^%.1f\n',length(age),r,log10(pval)); 
[r,pval]=corr(age(:),mixedGam(:)); %r=0.02, p=0.00004
fprintf(1,'Age vs risk taking in mixed trials (n=%d) r= %.3f, p=%.5f\n',length(age),r,pval); 
[r,pval]=corr(age(:),lossGam(:)); %r=-0.006, p=0.22
fprintf(1,'Age vs risk taking in loss trials (n=%d) r= %.3f, p=%.5f\n',length(age),r,pval); 


%% Do expectations affect happiness in a risky decision task?
%
% Approximately 75 percent of participants both won and lost a gain gamble
% (e.g., +100/0), both won and lost a loss gamble (e.g., 0/-100), and did
% not give an identical rating (e.g., 50) every time. For this subset of
% participants (n=35,219), average z-scored happiness reported on the next
% rating after winning a loss gamble (getting zero) was higher than the
% average rating after losing a gain gamble (getting zero) despite
% identical outcomes. This pattern of happiness after winning and losing
% gain and loss gambles is consistent with an effect of expectations on
% happpiness reported in a smaller early sample (n=18,420) in Rutledge et
% al. (2014) PNAS.

for s = 1:length(subjData)
    t       = subjData(s).data{1}(:,1:10);
    ind     = find(~isnan(t(:,10))); ind = ind(2:end);    %toss first rating
    t(:,10) = (t(:,10)-nanmean(t(:,10)))/nanstd(t(:,10)); %z-score including first rating, all nan if no variability    
    t(:,10) = t(repelem(ind,diff([0; ind])),10);          %fill all trials between ratings with the next rating
    t       = t(t(:,7)==1,:);                             %trials where gamble chosen
    subjData(s).wlHappy = [mean(t(t(:,3)>0&t(:,8)>0,10)) mean(t(t(:,3)>0&t(:,8)==0,10)) ...
        mean(t(t(:,3)<0&t(:,8)==0,10)) mean(t(t(:,3)<0&t(:,8)<0,10))]; %w/l gainGam, w/l lossGam
end
wlHappy = vertcat(subjData.wlHappy);
wlHappy = wlHappy(sum(isnan(wlHappy),2)==0,:);            %participants with at least one w/l from gain and loss gambles
figure('color',[1 1 1]); hold on; bar(mean(wlHappy)); 
errorbar(1:4,mean(wlHappy),std(wlHappy)/sqrt(size(wlHappy,1)),'r.'); 
ylabel('Happiness (z-score)'); axis square; ylim([-0.5 0.5]); title(sprintf('n=%d',size(wlHappy,1))); 
set(gca,'xtick',1:4,'xticklabel',{'Gain gamble win','Gain gamble zero','Loss gamble zero','Loss gamble loss'}); 
signrank(wlHappy(:,2),wlHappy(:,3))                       %participants are happier after avoiding a loss compared to not getting a reward


%% Is happiness lower during risky decision making in depression?
% 
% The average happiness reported during a risky decision task was lower in
% individuals (n=1,858) reporting greater depressive symptoms. A
% relationship between depressive symptoms and happiness during risky
% decision making was reported in Rutledge et al. (2017) JAMA Psychiatry.

for s = 1:length(depData)
    depData(s).meanHappy = nanmean(depData(s).data{1}(:,10));
end
meanHappy  = [depData.meanHappy];
bdiTotal   = [depData.bdiTotal];
bdiBin     = [0 3 6 9 15 20 30 40 61];
bdiHappy   = zeros(1,length(bdiBin)-1); bdiHappysem = bdiHappy;
for s = 1:length(bdiBin)-1
    temp           = meanHappy(bdiTotal>=bdiBin(s) & bdiTotal<bdiBin(s+1));
    bdiHappy(s)    = mean(temp);
    bdiHappysem(s) = std(temp)/sqrt(length(temp));
end
figure('color',[1 1 1]); hold on; 
errorbar(1:8,bdiHappy,bdiHappysem,'k.'); axis([0 9 30 70]); plot(xlim,[50 50],'k--');
ylabel('Average happiness'); xlabel('Symptom severity (BDI)'); title(sprintf('n=%d',length(bdiTotal)));
set(gca,'ytick',30:10:70,'xtick',1:8,'xticklabel',{'0-2','3-5','6-8','9-14','15-19','20-29','30-39','40-60'});
[rho,pval] = corr(bdiTotal(:),meanHappy(:),'type','spearman');
fprintf(1,'Depression severity vs average task happiness (n=%d) rho=%.3f, p=10^%.1f\n',...
    length(bdiTotal),rho,log10(pval)); %rho=-0.34, p<10^-50
