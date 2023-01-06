Tfull = table();
for iGroup=1:2
    if iGroup==1
        subjs=[1:9,11:23]; % 10 excluded...?
        groupStr='ya';
    elseif iGroup==2
        subjs=[101,102,103,104,105,106,107,...
            110,111,112,115,116,118,119,120,... %113 & 114 does not exist
            122,123,124,125];
        groupStr='oa';
    end
    
    
    for iSubj=subjs
    
     load(['/Users/abagaini/Documents/PROJECTS/Cumul_MA/cumulative/data/raw_data/effort/Chen_2020/OFS_LossAversionPD/saveMats/IP_onlyValid_' num2str(iSubj) '.mat'])
    
     
     subid = repmat(iSubj,12,1);
     age_group = repmat(groupStr,12,1);
     indif_point = [IP(1,:).'; IP(2,:).'];
     force_lvl = [(1:6).'; (1:6).'];
     cond_domain = [repmat({'reward'},6,1); repmat({'punish'},6,1)];
    
     T = table(subid,age_group,indif_point,force_lvl,cond_domain);
    
     
     Tfull = [Tfull; T];
    end
end


writetable(Tfull,'/Users/abagaini/Documents/PROJECTS/Cumul_MA/cumulative/data/raw_data/effort/Chen_2020/chen_2020_indifpoint_dat.csv');