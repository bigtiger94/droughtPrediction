# load prediction data ----------------------------------------------------
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = ""
for (bsncase in c(1:length(bsncodes))){
  targetbsn = bsncodes[bsncase];
  bsnname = bsnnames[bsncase];
  
  for (kk in c(1:4)){
    kfold = paste0("k", kk)
    predictfilepath = file.path("./predictResult", kfold)
    
    RPSfilepath = file.path(predictfilepath, "RPSresult") 
    if(!file.exists(RPSfilepath)) dir.create(RPSfilepath);
    
    predictdate = read.csv(file.path(predictfilepath, paste0(apcctype,"date.csv")), row.names=1)
    obsdate = read.csv(file.path(predictfilepath, "date.csv"), row.names=1)
    prob.rawenssri.dphase.each.ts = read.csv(file.path(predictfilepath, paste0("probforecast_",apcctype,"rawEDP_",dindexname,"_multi_",bsnname,".csv")),
                                             row.names=1) %>% xts(., ymd(predictdate$x))
    prob.upenssri.dphase.each.ts = read.csv(file.path(predictfilepath, paste0("probforecast_",apcctype,"upEDP_",dindexname,"_multi_",bsnname,".csv")),
                                            row.names=1) %>% xts(., ymd(predictdate$x))
    rawensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_",apcctype,"rawEDP_",dindexname,"_",bsnname,".csv")),
                                        row.names=1) %>% xts(., ymd(predictdate$x))
    upensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_",apcctype,"upEDP_",dindexname,"_",bsnname,".csv")),
                                       row.names=1) %>% xts(., ymd(predictdate$x))
    obs.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("obs_",dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(obsdate$x))
    
    obs.sri.dphase.ts = date(obs.sri.dphase.ts) %in% ymd(predictdate$x) %>% obs.sri.dphase.ts[.]
    
    
    
    temp.obscum.dphase = apply(obs.sri.dphase.ts, 1, cumsum) %>% t(.)
    temp.probcum.raw.dphase = apply(prob.rawenssri.dphase.each.ts, 1, cumsum) %>% t(.)
    temp.probcum.up.dphase = apply(prob.upenssri.dphase.each.ts, 1, cumsum) %>% t(.)
    temp.detcum.raw.dphase = apply(rawensmean.sri.dphase.ts, 1, cumsum) %>% t(.)
    temp.detcum.up.dphase = apply(upensmean.sri.dphase.ts, 1, cumsum) %>% t(.)
    
    RPS.prob_raw.ts = as.vector(apply((temp.probcum.raw.dphase - temp.obscum.dphase)^2, 1, sum) / (ncol(temp.probcum.raw.dphase)-1))%>%xts(., ymd(predictdate$x))
    RPS.prob_up.ts = as.vector(apply((temp.probcum.up.dphase - temp.obscum.dphase)^2, 1, sum) / (ncol(temp.probcum.up.dphase)-1))%>%xts(., ymd(predictdate$x))
    RPS.det_raw.ts = as.vector(apply((temp.detcum.raw.dphase - temp.obscum.dphase)^2, 1, sum) / (ncol(temp.detcum.raw.dphase)-1))%>%xts(., ymd(predictdate$x))
    RPS.det_up.ts = as.vector(apply((temp.detcum.up.dphase - temp.obscum.dphase)^2, 1, sum) / (ncol(temp.detcum.up.dphase)-1))%>%xts(., ymd(predictdate$x))
    
    # RPS.prob_raw.monthly = c()
    # RPS.prob_up.monthly = c()
    # 
    # for (mm in c(1:12)){
    #   RPS.prob_raw.monthly[mm] = month(RPS.prob_raw.ts) %in% mm %>% RPS.prob_raw.ts[.] %>% mean(.)
    #   RPS.prob_up.monthly[mm] = month(RPS.prob_up.ts) %in% mm %>% RPS.prob_up.ts[.] %>% mean(.)
    # }
    
    # RPS.prob_raw.monthly.df = data.frame(RPS = RPS.prob_raw.monthly)
    # RPS.prob_up.monthly.df = data.frame(RPS = RPS.prob_up.monthly)
    
    
    # BSfilepath = file.path("./predictResult", "BSresult")
    # BS.tot.det_up.df %>% write.csv(., file.path(BSfilepath, paste0("BS_det_up_",dindexname,"_",bsnname,".csv")))
    
    # data.frame(DATE=predictdate$x, RPS=RPS.prob_raw.ts) %>% write.csv(., file.path(RPSfilepath, paste0("RPS_prob_",apcctype,"raw_",dindexname,"_",bsnname,".csv")), row.names=F)
    # data.frame(DATE=predictdate$x, RPS=RPS.prob_up.ts) %>% write.csv(., file.path(RPSfilepath, paste0("RPS_prob_",apcctype,"up_",dindexname,"_",bsnname,".csv")), row.names=F)
    data.frame(DATE=predictdate$x, RPS=RPS.det_raw.ts) %>% write.csv(., file.path(RPSfilepath, paste0("RPS_det_",apcctype,"raw_",dindexname,"_",bsnname,".csv")), row.names=F)
    data.frame(DATE=predictdate$x, RPS=RPS.det_up.ts) %>% write.csv(., file.path(RPSfilepath, paste0("RPS_det_",apcctype,"up_",dindexname,"_",bsnname,".csv")), row.names=F)
    
    
    # data.frame(MONTH=c(1:12), RPS=RPS.prob_raw.monthly.df) %>% write.csv(., file.path(RPSfilepath, paste0("RPS_prob_",apcctype,"raw_monthly_",dindexname,"_",bsnname,".csv")), row.names=F)
    # data.frame(MONTH=c(1:12), RPS=RPS.prob_up.monthly.df)  %>% write.csv(., file.path(RPSfilepath, paste0("RPS_prob_",apcctype,"up_monthly_",dindexname,"_",bsnname,".csv")), row.names=F)
  }
}