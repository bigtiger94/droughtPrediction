# classifying drought phase and computing the probability from ensemble
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
bsncase = 7
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

sdi.thl = c(0, -1, -1.5, -2) # threhold from NOAA and GIDMaPS
thlname = c("No", "D0", "D1", "D2", "D3")

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)

kk=1
for (kk in c(1:4)){
  kfold = paste0("k", kk)
  
  predictsavefilepath = file.path("./predictResult", kfold) 
  if(!file.exists(predictsavefilepath)) dir.create(predictsavefilepath);
  
  calibyear = as.numeric(kfoldinfo[kk,])

  # obsfilepath = file.path("./observations")
  # sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
  # sricolidx = which(colnames(sri.df)==toupper(dindexname))
  
  ensfilepath = file.path("./ensSRI")
  rawensdate = read.csv(file.path(ensfilepath, "apccupensdate.csv"), row.names=1)
  upensdate = read.csv(file.path(ensfilepath, kfold, "apccupensdate.csv"), row.names=1)
  
  rawens.df = read.csv(file.path(ensfilepath, paste0("meansd_apccrawens_", dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rawensdate$x))
  upens.df = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_", dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(upensdate$x))
  
  validyear = setdiff(year(ymd(rawensdate$x)), calibyear)
  
  bcalibyear = intersect(calibyear[-1],year(ymd(rawensdate$x)))
  bvalidyear = validyear
  
  # temp_sri.ts = year(sri.df) %in% union(bcalibyear, bvalidyear) %>% sri.df[.,sricolidx]
  temp_rawenssri.stat.df = rawens.df
  temp_upenssri.stat.df = upens.df
  
  rawens.mean.ts = temp_rawenssri.stat.df$YEAR %in% union(bcalibyear, bvalidyear) %>% temp_rawenssri.stat.df$MEAN[.]
  rawens.sd.ts = temp_rawenssri.stat.df$YEAR %in% union(bcalibyear, bvalidyear) %>% temp_rawenssri.stat.df$SD[.]
  upens.mean.ts = temp_upenssri.stat.df$YEAR %in% union(bcalibyear, bvalidyear) %>% temp_upenssri.stat.df$MEAN[.]
  upens.sd.ts = temp_upenssri.stat.df$YEAR %in% union(bcalibyear, bvalidyear) %>% temp_upenssri.stat.df$SD[.]
  
  
  prob.rawenssri.dphase.over = data.frame()
  prob.rawenssri.dphase.each = data.frame()
  prob.upenssri.dphase.over = data.frame()
  prob.upenssri.dphase.each = data.frame()
  
  # obs.sri.dphase = as.data.frame(matrix(0, nrow=length(temp_sri.ts), ncol=1+length(sdi.thl)))
  rawensmean.sri.dphase = as.data.frame(matrix(0, nrow=length(rawens.mean.ts), ncol=1+length(sdi.thl)))
  upensmean.sri.dphase =  as.data.frame(matrix(0, nrow=length(upens.mean.ts), ncol=1+length(sdi.thl)))
  
  ###integrate(dnorm, mean=-0.5, sd=1, -1.5, -1)$value
  for (jj in c(1:nrow(temp_rawenssri.stat.df))){
    temp.prob.rawens.dphase.over = c()
    temp.prob.rawens.dphase.over[1] = 1-pnorm(sdi.thl[1], mean=rawens.mean.ts[jj], sd=rawens.sd.ts[jj])
    temp.prob.rawens.dphase.over[2:5] = pnorm(sdi.thl[1:4], mean=rawens.mean.ts[jj], sd=rawens.sd.ts[jj])
    temp.prob.rawens.dphase.over[is.na(temp.prob.rawens.dphase.over)] = 0
    prob.rawenssri.dphase.over = rbind(prob.rawenssri.dphase.over, temp.prob.rawens.dphase.over)
    
    # probability of occurrence for each phase from raw ensemble
    temp.prob.rawens.dphase.each = c()
    temp.prob.rawens.dphase.each[1] = 1-pnorm(sdi.thl[1], mean=rawens.mean.ts[jj], sd=rawens.sd.ts[jj])
    temp.prob.rawens.dphase.each[2:5] = temp.prob.rawens.dphase.over[2:5] - c(temp.prob.rawens.dphase.over[3:5],0)
    temp.prob.rawens.dphase.each[is.na(temp.prob.rawens.dphase.each)] = 0
    prob.rawenssri.dphase.each = rbind(prob.rawenssri.dphase.each, temp.prob.rawens.dphase.each)
    
    # probability of occurrence over threshold from updated ensemble
    temp.prob.upens.dphase.over = c()
    temp.prob.upens.dphase.over[1] = 1-pnorm(sdi.thl[1], mean=upens.mean.ts[jj], sd=upens.sd.ts[jj])
    temp.prob.upens.dphase.over[2:5] =  pnorm(sdi.thl[1:4], mean=upens.mean.ts[jj], sd=upens.sd.ts[jj])
    temp.prob.upens.dphase.over[is.na(temp.prob.upens.dphase.over)] = 0
    prob.upenssri.dphase.over = rbind(prob.upenssri.dphase.over, temp.prob.upens.dphase.over)
    
    # probability of occurrence for each phase from updated ensemble
    temp.prob.upens.dphase.each = c()
    temp.prob.upens.dphase.each[1] = 1-pnorm(sdi.thl[1], mean=upens.mean.ts[jj], sd=upens.sd.ts[jj])
    temp.prob.upens.dphase.each[2:5] = temp.prob.upens.dphase.over[2:5] - c(temp.prob.upens.dphase.over[3:5],0)
    temp.prob.upens.dphase.each[is.na(temp.prob.upens.dphase.each)] = 0
    prob.upenssri.dphase.each = rbind(prob.upenssri.dphase.each, temp.prob.upens.dphase.each)
    
    
    # drought phase from ensemble mean
    if(rawens.mean.ts[jj]>sdi.thl[1]) {
      rawensmean.sri.dphase[jj,1] = 1
    }else if(rawens.mean.ts[jj]<=sdi.thl[1]&rawens.mean.ts[jj]>sdi.thl[2]){
      rawensmean.sri.dphase[jj,2] = 1
    }else if(rawens.mean.ts[jj]<=sdi.thl[2]&rawens.mean.ts[jj]>sdi.thl[3]){
      rawensmean.sri.dphase[jj,3] = 1
    }else if(rawens.mean.ts[jj]<=sdi.thl[3]&rawens.mean.ts[jj]>sdi.thl[4]){
      rawensmean.sri.dphase[jj,4] = 1
    }else{
      rawensmean.sri.dphase[jj,5] = 1
    }
    
    
    # drought phase from ensemble mean
    if(upens.mean.ts[jj]>sdi.thl[1]) {
      upensmean.sri.dphase[jj,1] = 1
    }else if(upens.mean.ts[jj]<=sdi.thl[1]&upens.mean.ts[jj]>sdi.thl[2]){
      upensmean.sri.dphase[jj,2] = 1
    }else if(upens.mean.ts[jj]<=sdi.thl[2]&upens.mean.ts[jj]>sdi.thl[3]){
      upensmean.sri.dphase[jj,3] = 1
    }else if(upens.mean.ts[jj]<=sdi.thl[3]&upens.mean.ts[jj]>sdi.thl[4]){
      upensmean.sri.dphase[jj,4] = 1
    }else{
      upensmean.sri.dphase[jj,5] = 1
    }
    
    # # drought phase from observation
    # if(temp_sri.ts[jj]>sdi.thl[1]) {
    #   obs.sri.dphase[jj,1] = 1
    # }else if(temp_sri.ts[jj]<=sdi.thl[1]&temp_sri.ts[jj]>sdi.thl[2]){
    #   obs.sri.dphase[jj,2] = 1
    # }else if(temp_sri.ts[jj]<=sdi.thl[2]&temp_sri.ts[jj]>sdi.thl[3]){
    #   obs.sri.dphase[jj,3] = 1
    # }else if(temp_sri.ts[jj]<=sdi.thl[3]&temp_sri.ts[jj]>sdi.thl[4]){
    #   obs.sri.dphase[jj,4] = 1
    # }else{
    #   obs.sri.dphase[jj,5] = 1
    # }
  }
  
  colnames(prob.rawenssri.dphase.over) = thlname; prob.rawenssri.dphase.over.ts = xts(prob.rawenssri.dphase.over, date(temp_upenssri.stat.df)) %>% round(., digit=5);
  colnames(prob.rawenssri.dphase.each) = thlname; prob.rawenssri.dphase.each.ts = xts(prob.rawenssri.dphase.each, date(temp_upenssri.stat.df)) %>% round(., digit=5);
  colnames(prob.upenssri.dphase.over) = thlname; prob.upenssri.dphase.over.ts = xts(prob.upenssri.dphase.over, date(temp_upenssri.stat.df)) %>% round(., digit=5);
  colnames(prob.upenssri.dphase.each) = thlname; prob.upenssri.dphase.each.ts = xts(prob.upenssri.dphase.each, date(temp_upenssri.stat.df)) %>% round(., digit=5);
  # colnames(obs.sri.dphase) = thlname; obs.sri.dphase.ts = xts(obs.sri.dphase, date(temp_sri.ts));
  colnames(rawensmean.sri.dphase) = thlname; rawensmean.sri.dphase.ts = xts(rawensmean.sri.dphase, date(temp_upenssri.stat.df));
  colnames(upensmean.sri.dphase) = thlname; upensmean.sri.dphase.ts = xts(upensmean.sri.dphase, date(temp_upenssri.stat.df));
  
  tail(prob.rawenssri.dphase.over.ts, 12)
  apply(prob.rawenssri.dphase.each, 1, sum)
  apply(prob.upenssri.dphase.each, 1, sum)
  which(apply(prob.rawenssri.dphase.each, 1, sum)<0.9999)
  (erridx=which(apply(prob.upenssri.dphase.over, 1, sum)<1))
  
  # View(prob.rawenssri.dphase.over)
  
  rawens.mean.ts[erridx]
  rawens.sd.ts[erridx]
  prob.rawenssri.dphase.over[erridx,]
  
  
  predictsavefilepath = file.path("./predictResult", kfold) 
  if(!file.exists(predictsavefilepath)) dir.create(predictsavefilepath);
  
  
  prob.rawenssri.dphase.over.ts %>% write.csv(., file.path(predictsavefilepath, paste0("probforecast_apccrawEDP_",dindexname,"_binary_",bsnname,".csv")))
  prob.rawenssri.dphase.each.ts %>% write.csv(., file.path(predictsavefilepath, paste0("probforecast_apccrawEDP_",dindexname,"_multi_",bsnname,".csv")))
  prob.upenssri.dphase.over.ts %>% write.csv(., file.path(predictsavefilepath, paste0("probforecast_apccupEDP_",dindexname,"_binary_",bsnname,".csv")))
  prob.upenssri.dphase.each.ts %>% write.csv(., file.path(predictsavefilepath, paste0("probforecast_apccupEDP_",dindexname,"_multi_",bsnname,".csv")))
  rawensmean.sri.dphase.ts %>% write.csv(., file.path(predictsavefilepath, paste0("detforecast_apccrawEDP_",dindexname,"_",bsnname,".csv")))
  upensmean.sri.dphase.ts %>% write.csv(., file.path(predictsavefilepath, paste0("detforecast_apccupEDP_",dindexname,"_",bsnname,".csv")))
  # obs.sri.dphase.ts %>% write.csv(., file.path(predictsavefilepath, paste0("obs_",dindexname,"_",bsnname,".csv")))
  # date(temp_upenssri.stat.df) %>% write.csv(., file.path(predictsavefilepath, "apccdate.csv"))
}
