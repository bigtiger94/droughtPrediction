# load prediction data ----------------------------------------------------
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)

bsncase = 4
for (bsncase in c(7:length(bsncodes))){
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)


BS.df = data.frame()

BSfilepath = file.path("./predictResult", "BSresult")
if(!file.exists(BSfilepath)) dir.create(BSfilepath);

for (kk in c(1:4)){
  kfold = paste0("k", kk)
  predictfilepath = file.path("./predictResult", kfold)
  calibyear = kfoldinfo[kk,]
  
  predictdate = read.csv(file.path(predictfilepath, "date.csv"), row.names=1)
  
  prob.rawenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_rawEDP_",dindexname,"_binary_",bsnname,".csv")),
                                           row.names=1) %>% xts(., ymd(predictdate$x))
  prob.upenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_upEDP_",dindexname,"_binary_",bsnname,".csv")),
                                          row.names=1) %>% xts(., ymd(predictdate$x))
  rawensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_rawEDP_",dindexname,"_",bsnname,".csv")),
                                      row.names=1) %>% xts(., ymd(predictdate$x))
  upensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_upEDP_",dindexname,"_",bsnname,".csv")),
                                     row.names=1) %>% xts(., ymd(predictdate$x))
  obs.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("obs_",dindexname,"_",bsnname,".csv")),
                               row.names=1) %>% xts(., ymd(predictdate$x))
  
  validyear = setdiff(year(predictdate$x), calibyear)
  validindex = year(predictdate$x) %in% validyear

  
  # cal BS over D0 ----------------------------------------------------------
  phase = "D0"; 
  period = "all"
  # over 2
  
  temp.obs.dphase = obs.sri.dphase.ts[,-1] %>% apply(., 1, sum) %>% xts(., ymd(names(.)))#
  
  temp.det.raw.dphase = rawensmean.sri.dphase.ts[,-1] %>% apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.det.up.dphase = upensmean.sri.dphase.ts[,-1] %>% apply(., 1, sum) %>% xts(., ymd(names(.)))#
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,2]
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,2]
  
  temp.BS = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.prob.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "irrigation"
  # temp.BS.det
  
  temp..obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.,]
  # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "non-irrigation"
  temp..obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.,]
  
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  # cal BS over D1 ----------------------------------------------------------
  phase = "D1"; 
  period = "all"
  # over 2
  
  temp.obs.dphase = obs.sri.dphase.ts[,-1:-2] %>% 
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.det.raw.dphase = rawensmean.sri.dphase.ts[,-1:-2]%>%
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.det.up.dphase = upensmean.sri.dphase.ts[,-1:-2]%>%
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,3]
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,3]#
  
  
  temp.BS = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.prob.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "irrigation"
  # temp.BS.det
  
  temp..obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.,]
  # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "non-irrigation"
  temp..obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.,]
  
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  # cal BS over D2 ------------------------------------------------------------------
  phase = "D2"
  period = "all"
  
  temp.obs.dphase = obs.sri.dphase.ts[,-1:-3] %>% 
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.det.raw.dphase = rawensmean.sri.dphase.ts[,-1:-3]%>%
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.det.up.dphase = upensmean.sri.dphase.ts[,-1:-3]%>%
    apply(., 1, sum) %>% xts(., ymd(names(.)))#
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,4]
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,4]#
  
  temp.BS = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.prob.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "irrigation"
  # temp.BS.det
  
  temp..obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.,]
  # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "non-irrigation"
  temp..obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.,]
  
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  # cal BS over D3 ------------------------------------------------------------------
  phase = "D3"
  period = "all"
  
  temp.obs.dphase = obs.sri.dphase.ts[,5] 
  temp.det.raw.dphase = rawensmean.sri.dphase.ts[,5]
  temp.det.up.dphase = upensmean.sri.dphase.ts[,5]
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,5]
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,5]#
  
  
  temp.BS = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.prob.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp.det.up.dphase, temp.obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "irrigation"
  # temp.BS.det
  
  temp..obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.,]
  # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  
  period = "non-irrigation"
  temp..obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.,] 
  # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
  
  temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.,]
  temp..det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.,]
  # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
  
  temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.,]
  temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.,]
  
  
  temp.BS = calBS(temp..prob.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..prob.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="prob", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.raw.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="raw", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  temp.BS = calBS(temp..det.up.dphase, temp..obs.dphase)
  temp.BS.df = data.frame(K=kfold, Period=period, Enstype="up", Predicttype="det", D=phase, temp.BS[1,])
  BS.df = rbind(BS.df, temp.BS.df)
  
  #  ------------------------------------------------------------------------
  
  
  
  # graphics.off()
}
rownames(BS.df) = c(1:nrow(BS.df))
BS.df$BSS = (BS.df$RES-BS.df$REL)/BS.df$UNC

BS.df %>% write.csv(., file.path(BSfilepath, paste0("BS_allperiod_",dindexname,"_",bsnname,".csv")))
}