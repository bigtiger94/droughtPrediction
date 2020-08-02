# load prediction data ----------------------------------------------------
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju")
bsncodes = c(1012, 3008, 2001, 4001, 1003)
bsncase = 5
apcctype = ""
for (bsncase in c(1:5)){

targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];


kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
kk=1
for (kk in c(1:4)){
  
  kfold = paste0("k", kk)
  predictfilepath = file.path("./predictResult", kfold)
  
  BSfilepath = file.path(predictfilepath, "BSresult")
  if(!file.exists(BSfilepath)) dir.create(BSfilepath);
  
  
  predictdate = read.csv(file.path(predictfilepath, paste0(apcctype,"date.csv")), row.names=1)
  obsdate = read.csv(file.path(predictfilepath, "date.csv"), row.names=1)
  prob.rawenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_",apcctype,"rawEDP_",dindexname,"_binary_",bsnname,".csv")),
                                           row.names=1) %>% xts(., ymd(predictdate$x))
  prob.upenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_",apcctype,"upEDP_",dindexname,"_binary_",bsnname,".csv")),
                                           row.names=1) %>% xts(., ymd(predictdate$x))
  rawensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_",apcctype,"rawEDP_",dindexname,"_",bsnname,".csv")),
                                      row.names=1) %>% xts(., ymd(predictdate$x))
  upensmean.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("detforecast_",apcctype,"upEDP_",dindexname,"_",bsnname,".csv")),
                                     row.names=1) %>% xts(., ymd(predictdate$x))
  obs.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("obs_",dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(obsdate$x))
  
  obs.sri.dphase.ts = date(obs.sri.dphase.ts) %in% ymd(predictdate$x) %>% obs.sri.dphase.ts[.]

  # cal BS over D0 ----------------------------------------------------------
  phase = "D0"
  BS.prob_raw.df = data.frame()
  BS.prob_up.df = data.frame()
  BS.det_raw.df = data.frame()
  BS.det_up.df = data.frame()
  
  # over 2
  
  temp.obs.dphase = apply(obs.sri.dphase.ts[,-1], 1, sum)#
  # temp.obs.sri.dphase = obs.sri.dphase[,5]
  temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1], 1, sum)#
  temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1], 1, sum)#
  # temp.det.sri.dphase = ensmean.sri.dphase[,5]
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,2]#
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,2]#
  # temp.prob.sri.dphase = prob.sri.dphase.over[,5]#
  
  temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
  # temp.BS.prob
  
  temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
  # temp.BS.det
  
  BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
  BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
  BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
  BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  
  
  
  
  for (mm in c(1:12)){
    temp.obs.dphase = month(obs.sri.dphase.ts) %in% mm %>% obs.sri.dphase.ts[.,-1] %>% apply(., 1, sum)#
    # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
    
    temp.det.raw.dphase = month(rawensmean.sri.dphase.ts) %in% mm %>% rawensmean.sri.dphase.ts[.,-1] %>% apply(., 1, sum)#
    temp.det.up.dphase = month(upensmean.sri.dphase.ts) %in% mm %>% upensmean.sri.dphase.ts[.,-1] %>% apply(., 1, sum)#
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp.prob.raw.dphase = month(prob.rawenssri.dphase.over.ts) %in% mm %>% prob.rawenssri.dphase.over.ts[.,2]
    temp.prob.up.dphase = month(prob.upenssri.dphase.over.ts) %in% mm %>% prob.upenssri.dphase.over.ts[.,2]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
    temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
    temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
    temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  }
  
  rownames(BS.prob_raw.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_raw.df) = c("ALL", seq(1, 12, 1))
  
  rownames(BS.prob_up.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_up.df) = c("ALL", seq(1, 12, 1))
  
  which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1
  
  which(BS.prob_up.df$bs < BS.det_up.df$bs) -1
  
  BS.prob_up.df
  BS.det_up.df
  
  
  plotname = paste(toupper(dindexname), "monthly BS for", phase, "at", bsnname)
  jpeg(file.path(BSfilepath, paste0(plotname,"_",apcctype,".jpg")), width=600, height=400)
  plot(c(1:13), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
       ylim=c(0, 1), ylab="Brier Score", main=plotname,
       type="b", pch="x", col="grey", lty=2)
  lines(c(1:13), BS.prob_up.df$bs, type="b", pch="x", col="red")
  lines(c(1:13), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
  axis(side=1, at=1:13, labels=rownames(BS.det_up.df))
  grid()
  legend("topleft", legend=c(paste0("PP (EDP+",apcctype,")"), "PP (EDP)", paste0("DP (EDP+",apcctype,")")), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))
  dev.off()
  
  BS.tot.det_raw.df = data.frame(BS.det_raw.df$bs)
  BS.tot.det_up.df = data.frame(BS.det_up.df$bs)
  BS.tot.prob_raw.df = data.frame(BS.prob_raw.df$bs)
  BS.tot.prob_up.df = data.frame(BS.prob_up.df$bs)
  BSS.tot.det_raw.df = data.frame((BS.det_raw.df$RES-BS.det_raw.df$REL)/BS.det_raw.df$UNC)
  BSS.tot.det_up.df = data.frame((BS.det_up.df$RES-BS.det_up.df$REL)/BS.det_up.df$UNC)
  BSS.tot.prob_raw.df = data.frame((BS.prob_raw.df$RES-BS.prob_raw.df$REL)/BS.prob_raw.df$UNC)
  BSS.tot.prob_up.df = data.frame((BS.prob_up.df$RES-BS.prob_up.df$REL)/BS.prob_up.df$UNC)
  
  # cal BS over D1 ----------------------------------------------------------
  phase = "D1"
  BS.prob_raw.df = data.frame()
  BS.prob_up.df = data.frame()
  BS.det_raw.df = data.frame()
  BS.det_up.df = data.frame()
  
  # over 3
  
  temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-2], 1, sum)#
  # temp.obs.sri.dphase = obs.sri.dphase[,5]
  temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1:-2], 1, sum)#
  temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1:-2], 1, sum)#
  # temp.det.sri.dphase = ensmean.sri.dphase[,5]
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,3]#
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,3]#
  # temp.prob.sri.dphase = prob.sri.dphase.over[,5]#
  
  temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
  # temp.BS.prob
  
  temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
  # temp.BS.det
  
  BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
  BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
  BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
  BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  
  
  
  
  for (mm in c(1:12)){
    temp.obs.dphase = month(obs.sri.dphase.ts) %in% mm %>% obs.sri.dphase.ts[.,-1:-2] %>% apply(., 1, sum)#
    # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
    
    temp.det.raw.dphase = month(rawensmean.sri.dphase.ts) %in% mm %>% rawensmean.sri.dphase.ts[.,-1:-2] %>% apply(., 1, sum)#
    temp.det.up.dphase = month(upensmean.sri.dphase.ts) %in% mm %>% upensmean.sri.dphase.ts[.,-1:-2] %>% apply(., 1, sum)#
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp.prob.raw.dphase = month(prob.rawenssri.dphase.over.ts) %in% mm %>% prob.rawenssri.dphase.over.ts[.,3]
    temp.prob.up.dphase = month(prob.upenssri.dphase.over.ts) %in% mm %>% prob.upenssri.dphase.over.ts[.,3]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
    temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
    temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
    temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  }
  
  rownames(BS.prob_raw.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_raw.df) = c("ALL", seq(1, 12, 1))
  
  rownames(BS.prob_up.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_up.df) = c("ALL", seq(1, 12, 1))
  
  which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1
  
  which(BS.prob_up.df$bs < BS.det_up.df$bs) -1
  
  BS.prob_up.df
  BS.det_up.df
  
  plotname = paste(toupper(dindexname), "monthly BS for", phase, "at", bsnname)
  jpeg(file.path(BSfilepath, paste0(plotname,"_",apcctype,".jpg")), width=600, height=400)
  plot(c(1:13), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
       ylim=c(0, 1), ylab="Brier Score", main=plotname,
       type="b", pch="x", col="grey", lty=2)
  lines(c(1:13), BS.prob_up.df$bs, type="b", pch="x", col="red")
  lines(c(1:13), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
  axis(side=1, at=1:13, labels=rownames(BS.det_up.df))
  grid()
  legend("topleft", legend=c(paste0("PP (EDP+",apcctype,")"), "PP (EDP)", paste0("DP (EDP+",apcctype,")")), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))
  dev.off()
  
  BS.tot.det_raw.df = cbind(BS.tot.det_raw.df, BS.det_raw.df$bs)
  BS.tot.det_up.df = cbind(BS.tot.det_up.df, BS.det_up.df$bs)
  BS.tot.prob_raw.df = cbind(BS.tot.prob_raw.df, BS.prob_raw.df$bs)
  BS.tot.prob_up.df = cbind(BS.tot.prob_up.df, BS.prob_up.df$bs)
  BSS.tot.det_raw.df = cbind(BSS.tot.det_raw.df,(BS.det_raw.df$RES-BS.det_raw.df$REL)/BS.det_raw.df$UNC)
  BSS.tot.det_up.df = cbind(BSS.tot.det_up.df,(BS.det_up.df$RES-BS.det_up.df$REL)/BS.det_up.df$UNC)
  BSS.tot.prob_raw.df = cbind(BSS.tot.prob_raw.df,(BS.prob_raw.df$RES-BS.prob_raw.df$REL)/BS.prob_raw.df$UNC)
  BSS.tot.prob_up.df = cbind(BSS.tot.prob_up.df,(BS.prob_up.df$RES-BS.prob_up.df$REL)/BS.prob_up.df$UNC)
  
  # cal BS over D2 ------------------------------------------------------------------
  phase = "D2"
  BS.prob_raw.df = data.frame()
  BS.prob_up.df = data.frame()
  BS.det_raw.df = data.frame()
  BS.det_up.df = data.frame()
  # over 4
  
  
  
  
  temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-3], 1, sum)#
  # temp.obs.sri.dphase = obs.sri.dphase[,5]
  temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1:-3], 1, sum)#
  temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1:-3], 1, sum)#
  # temp.det.sri.dphase = ensmean.sri.dphase[,5]
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,4]#
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,4]#
  # temp.prob.sri.dphase = prob.sri.dphase.over[,5]#
  
  temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
  # temp.BS.prob
  
  temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
  # temp.BS.det
  
  BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
  BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
  BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
  BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  
  
  
  
  for (mm in c(1:12)){
    temp.obs.dphase = month(obs.sri.dphase.ts) %in% mm %>% obs.sri.dphase.ts[.,-1:-3] %>% apply(., 1, sum)#
    # temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)
    
    temp.det.raw.dphase = month(rawensmean.sri.dphase.ts) %in% mm %>% rawensmean.sri.dphase.ts[.,-1:-3] %>% apply(., 1, sum)#
    temp.det.up.dphase = month(upensmean.sri.dphase.ts) %in% mm %>% upensmean.sri.dphase.ts[.,-1:-3] %>% apply(., 1, sum)#
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp.prob.raw.dphase = month(prob.rawenssri.dphase.over.ts) %in% mm %>% prob.rawenssri.dphase.over.ts[.,4]
    temp.prob.up.dphase = month(prob.upenssri.dphase.over.ts) %in% mm %>% prob.upenssri.dphase.over.ts[.,4]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
    temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
    temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
    temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  }
  
  rownames(BS.prob_raw.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_raw.df) = c("ALL", seq(1, 12, 1))
  
  rownames(BS.prob_up.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_up.df) = c("ALL", seq(1, 12, 1))
  
  which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1
  
  which(BS.prob_up.df$bs < BS.det_up.df$bs) -1
  
  BS.prob_up.df
  BS.det_up.df
  
  
  plotname = paste(toupper(dindexname), "monthly BS for", phase, "at", bsnname)
  jpeg(file.path(BSfilepath, paste0(plotname,"_",apcctype,".jpg")), width=600, height=400)
  plot(c(1:13), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
       ylim=c(0, 1), ylab="Brier Score", main=plotname,
       type="b", pch="x", col="grey", lty=2)
  lines(c(1:13), BS.prob_up.df$bs, type="b", pch="x", col="red")
  lines(c(1:13), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
  axis(side=1, at=1:13, labels=rownames(BS.det_up.df))
  grid()
  legend("topleft", legend=c(paste0("PP (EDP+",apcctype,")"), "PP (EDP)", paste0("DP (EDP+",apcctype,")")), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))
  dev.off()
  
  BS.tot.det_raw.df = cbind(BS.tot.det_raw.df, BS.det_raw.df$bs)
  BS.tot.det_up.df = cbind(BS.tot.det_up.df, BS.det_up.df$bs)
  BS.tot.prob_raw.df = cbind(BS.tot.prob_raw.df, BS.prob_raw.df$bs)
  BS.tot.prob_up.df = cbind(BS.tot.prob_up.df, BS.prob_up.df$bs)
  BSS.tot.det_raw.df = cbind(BSS.tot.det_raw.df,(BS.det_raw.df$RES-BS.det_raw.df$REL)/BS.det_raw.df$UNC)
  BSS.tot.det_up.df = cbind(BSS.tot.det_up.df,(BS.det_up.df$RES-BS.det_up.df$REL)/BS.det_up.df$UNC)
  BSS.tot.prob_raw.df = cbind(BSS.tot.prob_raw.df,(BS.prob_raw.df$RES-BS.prob_raw.df$REL)/BS.prob_raw.df$UNC)
  BSS.tot.prob_up.df = cbind(BSS.tot.prob_up.df,(BS.prob_up.df$RES-BS.prob_up.df$REL)/BS.prob_up.df$UNC)
  
  # cal BS over D3 ------------------------------------------------------------------
  phase = "D3"
  BS.prob_raw.df = data.frame()
  BS.prob_up.df = data.frame()
  BS.det_raw.df = data.frame()
  BS.det_up.df = data.frame()
  # over 5
  
  temp.obs.dphase = obs.sri.dphase.ts[,5]
  temp.det.raw.dphase = rawensmean.sri.dphase.ts[,5]
  temp.det.up.dphase = upensmean.sri.dphase.ts[,5]
  
  temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,5]
  temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,5]
  
  temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
  temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
  # temp.BS.prob
  
  temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
  temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
  # temp.BS.det
  
  BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
  BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
  BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
  BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  
  
  
  
  for (mm in c(1:12)){
    temp.obs.dphase = month(obs.sri.dphase.ts) %in% mm %>% obs.sri.dphase.ts[.,5]
    
    temp.det.raw.dphase = month(rawensmean.sri.dphase.ts) %in% mm %>% rawensmean.sri.dphase.ts[.,5] 
    temp.det.up.dphase = month(upensmean.sri.dphase.ts) %in% mm %>% upensmean.sri.dphase.ts[.,5]
    
    temp.prob.raw.dphase = month(prob.rawenssri.dphase.over.ts) %in% mm %>% prob.rawenssri.dphase.over.ts[.,5]
    temp.prob.up.dphase = month(prob.upenssri.dphase.over.ts) %in% mm %>% prob.upenssri.dphase.over.ts[.,5]
    
    temp.BS_raw.prob = calBS(temp.prob.raw.dphase, temp.obs.dphase)
    temp.BS_up.prob = calBS(temp.prob.up.dphase, temp.obs.dphase)
    temp.BS_raw.det = calBS(temp.det.raw.dphase, temp.obs.dphase)
    temp.BS_up.det = calBS(temp.det.up.dphase, temp.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
  }
  
  rownames(BS.prob_raw.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_raw.df) = c("ALL", seq(1, 12, 1))
  
  rownames(BS.prob_up.df) = c("ALL", seq(1, 12, 1)) 
  rownames(BS.det_up.df) = c("ALL", seq(1, 12, 1))
  
  
  which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1
  
  which(BS.prob_up.df$bs < BS.det_up.df$bs) -1
  
  BS.prob_up.df
  BS.det_up.df
  
  
  plotname = paste(toupper(dindexname), "monthly BS for", phase, "at", bsnname)
  jpeg(file.path(BSfilepath, paste0(plotname,"_",apcctype,".jpg")), width=600, height=400)
  plot(c(1:13), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
       ylim=c(0, 1), ylab="Brier Score", main=plotname,
       type="b", pch="x", col="grey", lty=2)
  lines(c(1:13), BS.prob_up.df$bs, type="b", pch="x", col="red")
  lines(c(1:13), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
  axis(side=1, at=1:13, labels=rownames(BS.det_up.df))
  grid()
  legend("topleft", legend=c(paste0("PP (EDP+",apcctype,")"), "PP (EDP)", paste0("DP (EDP+",apcctype,")")), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))
  dev.off()
  
  BS.tot.det_raw.df = cbind(BS.tot.det_raw.df, BS.det_raw.df$bs)
  BS.tot.det_up.df = cbind(BS.tot.det_up.df, BS.det_up.df$bs)
  BS.tot.prob_raw.df = cbind(BS.tot.prob_raw.df, BS.prob_raw.df$bs)
  BS.tot.prob_up.df = cbind(BS.tot.prob_up.df, BS.prob_up.df$bs)
  
  
  BSS.tot.det_raw.df = cbind(BSS.tot.det_raw.df,(BS.det_raw.df$RES-BS.det_raw.df$REL)/BS.det_raw.df$UNC)
  BSS.tot.det_up.df = cbind(BSS.tot.det_up.df,(BS.det_up.df$RES-BS.det_up.df$REL)/BS.det_up.df$UNC)
  BSS.tot.prob_raw.df = cbind(BSS.tot.prob_raw.df,(BS.prob_raw.df$RES-BS.prob_raw.df$REL)/BS.prob_raw.df$UNC)
  BSS.tot.prob_up.df = cbind(BSS.tot.prob_up.df,(BS.prob_up.df$RES-BS.prob_up.df$REL)/BS.prob_up.df$UNC)
  
  
  #  ------------------------------------------------------------------------
  
  colnames(BS.tot.det_raw.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BS.tot.det_raw.df) = rownames(BS.prob_up.df)
  colnames(BS.tot.det_up.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BS.tot.det_up.df) = rownames(BS.prob_up.df)
  colnames(BS.tot.prob_raw.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BS.tot.prob_raw.df) = rownames(BS.prob_up.df)
  colnames(BS.tot.prob_up.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BS.tot.prob_up.df) = rownames(BS.prob_up.df)
  
  colnames(BSS.tot.det_raw.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BSS.tot.det_raw.df) = rownames(BS.prob_up.df)
  colnames(BSS.tot.det_up.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BSS.tot.det_up.df) = rownames(BS.prob_up.df)
  colnames(BSS.tot.prob_raw.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BSS.tot.prob_raw.df) = rownames(BS.prob_up.df)
  colnames(BSS.tot.prob_up.df) = colnames(prob.rawenssri.dphase.over.ts)[2:5]
  rownames(BSS.tot.prob_up.df) = rownames(BS.prob_up.df)
  

  BS.tot.det_up.df %>% write.csv(., file.path(BSfilepath, paste0("BS_monthly_det_",apcctype,"up_",dindexname,"_",bsnname,".csv")))
  BS.tot.det_raw.df %>% write.csv(., file.path(BSfilepath, paste0("BS_monthly_det_",apcctype,"raw_",dindexname,"_",bsnname,".csv")))
  BS.tot.prob_raw.df %>% write.csv(., file.path(BSfilepath, paste0("BS_monthly_prob_",apcctype,"raw_",dindexname,"_",bsnname,".csv")))
  BS.tot.prob_up.df %>% write.csv(., file.path(BSfilepath, paste0("BS_monthly_prob_",apcctype,"up_",dindexname,"_",bsnname,".csv")))

  BSS.tot.det_up.df %>% write.csv(., file.path(BSfilepath, paste0("BSS_monthly_det_",apcctype,"up_",dindexname,"_",bsnname,".csv")))
  BSS.tot.det_raw.df %>% write.csv(., file.path(BSfilepath, paste0("BSS_monthly_det_",apcctype,"raw_",dindexname,"_",bsnname,".csv")))
  BSS.tot.prob_raw.df %>% write.csv(., file.path(BSfilepath, paste0("BSS_monthly_prob_",apcctype,"raw_",dindexname,"_",bsnname,".csv")))
  BSS.tot.prob_up.df %>% write.csv(., file.path(BSfilepath, paste0("BSS_monthly_prob_",apcctype,"up_",dindexname,"_",bsnname,".csv")))


# graphics.off()
}
}