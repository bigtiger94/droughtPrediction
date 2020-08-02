dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

LMabsres_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))
LMnrmse_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))
LMnrmse = c()
bsncase = 1
for (bsncase in c(1:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     CI.df = read.csv(file.path("./likelihood", paste0("LMCI_0.01_",dindexname,"_",bsnname,".csv")), 
                        row.names=1) %>% xts(., ymd(rownames(.)))
   
     temp_absres = abs(CI.df$PRED-CI.df$OBS)
     
     for (MONTH in c(1:12)){
          monthly_absres = month(temp_absres) %in% MONTH %>% temp_absres[.]
          monthly_OBS = month(CI.df) %in% MONTH %>% CI.df$OBS[.]
          LMabsres_monthly[bsncase, MONTH] = monthly_absres %>% sum(.)
          LMnrmse_monthly[bsncase, MONTH] = monthly_absres^2 %>% mean(.) %>% sqrt(.)/mean(monthly_OBS)
     }
     
     LMnrmse[bsncase] = sqrt(mean((temp_absres)^2))/abs(sd(CI.df$OBS))
}
LMabsres_monthly$total = apply(LMabsres_monthly, 1, sum)
LMabsres_monthly$avg = apply(LMabsres_monthly, 1, mean)
LMnrmse_monthly$total = apply(LMnrmse_monthly, 1, sum)
LMnrmse_monthly$avg = apply(LMnrmse_monthly, 1, mean)

names(LMnrmse) = bsnnames
rownames(LMabsres_monthly) = bsnnames
rownames(LMnrmse_monthly) = bsnnames

LMabsres_monthly
LMnrmse_monthly
LMnrmse
write.csv(LMnrmse_monthly, file.path("./likelihood", paste0("LM_NRMSE_monthly",dindexname,".csv")))
