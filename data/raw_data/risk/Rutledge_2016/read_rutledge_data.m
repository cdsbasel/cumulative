
% READ THE MAT FILE
load('Rutledge_GBE_risk_data.mat')



% calculate the prop. of risk choices in the 1st play
% adapted from the original script (Rutledge_GBE_risk_data_code)
for s=1:length(subjData)
    t=subjData(s).data{1}(:,1:7);
    subjData(s).gainGam  = mean(t(t(:,3)>0,7));
    subjData(s).mixedGam = mean(t(t(:,3)==0,7));
    subjData(s).lossGam  = mean(t(t(:,3)<0,7));
end
id       = [subjData.id].';
age      = [subjData.age].';
gender   = [subjData.isFemale].';
gainGam  = [subjData.gainGam]; gainGam   = gainGam(:);
mixedGam = [subjData.mixedGam]; mixedGam = mixedGam(:);
lossGam  = [subjData.lossGam]; lossGam   = lossGam(:);

final_dat = table(id, age, gender, gainGam, mixedGam,lossGam);


writetable(final_dat, 'rutledge_data.csv')
