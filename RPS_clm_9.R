# load prediction data ----------------------------------------------------
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

bsncase = 1
for (bsncase in c(7:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     obsdate = read.csv(file.path("./predictResult", "k1", "date.csv"), row.names=1)
     obs.sri.dphase.ts = read.csv(file.path("./predictResult", "k1", paste0("obs_",dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(obsdate$x))
     obs.sri.dphase.ts = year(obs.sri.dphase.ts) %in% c(2001:2017) %>% obs.sri.dphase.ts[.]
     clmpredict = c(pnorm(0), pnorm(0)-pnorm(-1), pnorm(-1)-pnorm(-1.5), pnorm(-1.5)-pnorm(-2), pnorm(-2))
     clmpredict.mat = matrix(clmpredict, ncol = length(clmpredict), nrow=nrow(obs.sri.dphase.ts), byrow = T) %>% 
          xts(., date(obs.sri.dphase.ts)) %>% setNames(colnames(obs.sri.dphase.ts))
     
     
     temp.obscum.dphase = apply(obs.sri.dphase.ts, 1, cumsum) %>% t(.)
     temp.clmcum.dphase = apply(clmpredict.mat, 1, cumsum) %>% t(.)
     
     RPS.clm.ts = as.vector(apply((temp.clmcum.dphase - temp.obscum.dphase)^2, 1, sum) / (ncol(temp.clmcum.dphase)-1)) %>% xts(., date(clmpredict.mat))
     # BSfilepath = file.path("./predictResult", "BSresult")
     # BS.tot.det_up.df %>% write.csv(., file.path(BSfilepath, paste0("BS_det_up_",dindexname,"_",bsnname,".csv")))
     
     data.frame(DATE=date(clmpredict.mat), RPS=RPS.clm.ts) %>% 
          write.csv(., file.path("./predictResult", paste0("RPS_clm_",dindexname,"_",bsnname,".csv")), row.names=F)
}
