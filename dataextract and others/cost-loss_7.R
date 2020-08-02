# load prediction data ----------------------------------------------------
# kk=4
kk=4
kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
dindexname = "sri3"
bsnname = "daecheong"

kfold = paste0("k", kk)
predictfilepath = file.path("./predictResult", kfold)
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


# C-L for D0 ----------------------------------------------------------
phase = "D0"
CT.prob_raw.df = data.frame()
CT.prob_up.df = data.frame()
CT.det_raw.df = data.frame()
CT.det_up.df = data.frame()

# over 2

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,2] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,2] %>% as.vector() %>% xts(., ymd(predictdate$x))#

CT.prob_raw.df = cal_contingency(temp.prob.raw.dphase, temp.obs.dphase) %>% rbind(CT.prob_raw.df, .)
CT.prob_up.df = cal_contingency(temp.prob.up.dphase, temp.obs.dphase) %>% rbind(CT.prob_up.df, .)
CT.det_raw.df = cal_contingency(temp.det.raw.dphase, temp.obs.dphase) %>% rbind(CT.det_raw.df, .)
CT.det_up.df = cal_contingency(temp.det.up.dphase, temp.obs.dphase) %>% rbind(CT.det_up.df, .)

CT.all.df = rbind(CT.prob_raw.df, CT.prob_up.df, CT.det_raw.df, CT.det_up.df)


for (ii in c(1:nrow(CT.all.df))){
     if (ii==1) PEV.all.df = data.frame(cal_PEV(CT.all.df[ii,], temp.obs.dphase))
     else PEV.all.df = merge(PEV.all.df, cal_PEV(CT.all.df[ii,], temp.obs.dphase), by="RATIO")
}
colnames(PEV.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0, 1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))
detach(PEV.all.df)


period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)

####################irrigation period
temp2.obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.]
temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.]
temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.]
temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.]
temp2.det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.]
CT2.prob_raw.df = data.frame()
CT2.prob_up.df = data.frame()
CT2.det_raw.df = data.frame()
CT2.det_up.df = data.frame()


CT2.prob_raw.df = cal_contingency(temp2.prob.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_raw.df, .)
CT2.prob_up.df = cal_contingency(temp2.prob.up.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_up.df, .)
CT2.det_raw.df = cal_contingency(temp2.det.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.det_raw.df, .)
CT2.det_up.df = cal_contingency(temp2.det.up.dphase, temp2.obs.dphase) %>% rbind(CT2.det_up.df, .)

CT2.all.df = rbind(CT2.prob_raw.df, CT2.prob_up.df, CT2.det_raw.df, CT2.det_up.df)


for (ii in c(1:nrow(CT2.all.df))){
  if (ii==1) PEV2.all.df = data.frame(cal_PEV(CT2.all.df[ii,], temp2.obs.dphase))
  else PEV2.all.df = merge(PEV2.all.df, cal_PEV(CT2.all.df[ii,], temp2.obs.dphase), by="RATIO")
}
colnames(PEV2.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV2.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV2.all.df)

######################## non irrigation period
temp3.obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.]
temp3.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.]
temp3.prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.]
temp3.det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.]
temp3.det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.]
CT3.prob_raw.df = data.frame()
CT3.prob_up.df = data.frame()
CT3.det_raw.df = data.frame()
CT3.det_up.df = data.frame()


CT3.prob_raw.df = cal_contingency(temp3.prob.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_raw.df, .)
CT3.prob_up.df = cal_contingency(temp3.prob.up.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_up.df, .)
CT3.det_raw.df = cal_contingency(temp3.det.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.det_raw.df, .)
CT3.det_up.df = cal_contingency(temp3.det.up.dphase, temp3.obs.dphase) %>% rbind(CT3.det_up.df, .)

CT3.all.df = rbind(CT3.prob_raw.df, CT3.prob_up.df, CT3.det_raw.df, CT3.det_up.df)


for (ii in c(1:nrow(CT3.all.df))){
  if (ii==1) PEV3.all.df = data.frame(cal_PEV(CT3.all.df[ii,], temp3.obs.dphase))
  else PEV3.all.df = merge(PEV3.all.df, cal_PEV(CT3.all.df[ii,], temp3.obs.dphase), by="RATIO")
}
colnames(PEV3.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV3.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during non-irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV3.all.df)



# C-L for D1 ----------------------------------------------------------
phase = "D1"
CT.prob_raw.df = data.frame()
CT.prob_up.df = data.frame()
CT.det_raw.df = data.frame()
CT.det_up.df = data.frame()

# over 3

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-2], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1:-2], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1:-2], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,3] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,3] %>% as.vector() %>% xts(., ymd(predictdate$x))#


CT.prob_raw.df = cal_contingency(temp.prob.raw.dphase, temp.obs.dphase) %>% rbind(CT.prob_raw.df, .)
CT.prob_up.df = cal_contingency(temp.prob.up.dphase, temp.obs.dphase) %>% rbind(CT.prob_up.df, .)
CT.det_raw.df = cal_contingency(temp.det.raw.dphase, temp.obs.dphase) %>% rbind(CT.det_raw.df, .)
CT.det_up.df = cal_contingency(temp.det.up.dphase, temp.obs.dphase) %>% rbind(CT.det_up.df, .)

CT.all.df = rbind(CT.prob_raw.df, CT.prob_up.df, CT.det_raw.df, CT.det_up.df)


for (ii in c(1:nrow(CT.all.df))){
  if (ii==1) PEV.all.df = data.frame(cal_PEV(CT.all.df[ii,], temp.obs.dphase))
  else PEV.all.df = merge(PEV.all.df, cal_PEV(CT.all.df[ii,], temp.obs.dphase), by="RATIO")
}
colnames(PEV.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0, 1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))
detach(PEV.all.df)


period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)

####################irrigation period
temp2.obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.]
temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.]
temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.]
temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.]
temp2.det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.]
CT2.prob_raw.df = data.frame()
CT2.prob_up.df = data.frame()
CT2.det_raw.df = data.frame()
CT2.det_up.df = data.frame()


CT2.prob_raw.df = cal_contingency(temp2.prob.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_raw.df, .)
CT2.prob_up.df = cal_contingency(temp2.prob.up.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_up.df, .)
CT2.det_raw.df = cal_contingency(temp2.det.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.det_raw.df, .)
CT2.det_up.df = cal_contingency(temp2.det.up.dphase, temp2.obs.dphase) %>% rbind(CT2.det_up.df, .)

CT2.all.df = rbind(CT2.prob_raw.df, CT2.prob_up.df, CT2.det_raw.df, CT2.det_up.df)


for (ii in c(1:nrow(CT2.all.df))){
  if (ii==1) PEV2.all.df = data.frame(cal_PEV(CT2.all.df[ii,], temp2.obs.dphase))
  else PEV2.all.df = merge(PEV2.all.df, cal_PEV(CT2.all.df[ii,], temp2.obs.dphase), by="RATIO")
}
colnames(PEV2.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV2.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV2.all.df)

######################## non irrigation period
temp3.obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.]
temp3.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.]
temp3.prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.]
temp3.det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.]
temp3.det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.]
CT3.prob_raw.df = data.frame()
CT3.prob_up.df = data.frame()
CT3.det_raw.df = data.frame()
CT3.det_up.df = data.frame()


CT3.prob_raw.df = cal_contingency(temp3.prob.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_raw.df, .)
CT3.prob_up.df = cal_contingency(temp3.prob.up.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_up.df, .)
CT3.det_raw.df = cal_contingency(temp3.det.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.det_raw.df, .)
CT3.det_up.df = cal_contingency(temp3.det.up.dphase, temp3.obs.dphase) %>% rbind(CT3.det_up.df, .)

CT3.all.df = rbind(CT3.prob_raw.df, CT3.prob_up.df, CT3.det_raw.df, CT3.det_up.df)


for (ii in c(1:nrow(CT3.all.df))){
  if (ii==1) PEV3.all.df = data.frame(cal_PEV(CT3.all.df[ii,], temp3.obs.dphase))
  else PEV3.all.df = merge(PEV3.all.df, cal_PEV(CT3.all.df[ii,], temp3.obs.dphase), by="RATIO")
}
colnames(PEV3.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV3.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during non-irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV3.all.df)

# C-L for D2 ----------------------------------------------------------
phase = "D2"
CT.prob_raw.df = data.frame()
CT.prob_up.df = data.frame()
CT.det_raw.df = data.frame()
CT.det_up.df = data.frame()

# over 3

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-3], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[,-1:-3], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[,-1:-3], 1, sum) %>% as.vector() %>% xts(., ymd(predictdate$x))#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,4] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,4] %>% as.vector() %>% xts(., ymd(predictdate$x))#

CT.prob_raw.df = cal_contingency(temp.prob.raw.dphase, temp.obs.dphase) %>% rbind(CT.prob_raw.df, .)
CT.prob_up.df = cal_contingency(temp.prob.up.dphase, temp.obs.dphase) %>% rbind(CT.prob_up.df, .)
CT.det_raw.df = cal_contingency(temp.det.raw.dphase, temp.obs.dphase) %>% rbind(CT.det_raw.df, .)
CT.det_up.df = cal_contingency(temp.det.up.dphase, temp.obs.dphase) %>% rbind(CT.det_up.df, .)

CT.all.df = rbind(CT.prob_raw.df, CT.prob_up.df, CT.det_raw.df, CT.det_up.df)


for (ii in c(1:nrow(CT.all.df))){
  if (ii==1) PEV.all.df = data.frame(cal_PEV(CT.all.df[ii,], temp.obs.dphase))
  else PEV.all.df = merge(PEV.all.df, cal_PEV(CT.all.df[ii,], temp.obs.dphase), by="RATIO")
}
colnames(PEV.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0, 1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))
detach(PEV.all.df)


period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)

####################irrigation period
temp2.obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.]
temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.]
temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.]
temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.]
temp2.det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.]
CT2.prob_raw.df = data.frame()
CT2.prob_up.df = data.frame()
CT2.det_raw.df = data.frame()
CT2.det_up.df = data.frame()


CT2.prob_raw.df = cal_contingency(temp2.prob.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_raw.df, .)
CT2.prob_up.df = cal_contingency(temp2.prob.up.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_up.df, .)
CT2.det_raw.df = cal_contingency(temp2.det.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.det_raw.df, .)
CT2.det_up.df = cal_contingency(temp2.det.up.dphase, temp2.obs.dphase) %>% rbind(CT2.det_up.df, .)

CT2.all.df = rbind(CT2.prob_raw.df, CT2.prob_up.df, CT2.det_raw.df, CT2.det_up.df)


for (ii in c(1:nrow(CT2.all.df))){
  if (ii==1) PEV2.all.df = data.frame(cal_PEV(CT2.all.df[ii,], temp2.obs.dphase))
  else PEV2.all.df = merge(PEV2.all.df, cal_PEV(CT2.all.df[ii,], temp2.obs.dphase), by="RATIO")
}
colnames(PEV2.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV2.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV2.all.df)

######################## non irrigation period
temp3.obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.]
temp3.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.]
temp3.prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.]
temp3.det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.]
temp3.det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.]
CT3.prob_raw.df = data.frame()
CT3.prob_up.df = data.frame()
CT3.det_raw.df = data.frame()
CT3.det_up.df = data.frame()


CT3.prob_raw.df = cal_contingency(temp3.prob.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_raw.df, .)
CT3.prob_up.df = cal_contingency(temp3.prob.up.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_up.df, .)
CT3.det_raw.df = cal_contingency(temp3.det.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.det_raw.df, .)
CT3.det_up.df = cal_contingency(temp3.det.up.dphase, temp3.obs.dphase) %>% rbind(CT3.det_up.df, .)

CT3.all.df = rbind(CT3.prob_raw.df, CT3.prob_up.df, CT3.det_raw.df, CT3.det_up.df)


for (ii in c(1:nrow(CT3.all.df))){
  if (ii==1) PEV3.all.df = data.frame(cal_PEV(CT3.all.df[ii,], temp3.obs.dphase))
  else PEV3.all.df = merge(PEV3.all.df, cal_PEV(CT3.all.df[ii,], temp3.obs.dphase), by="RATIO")
}
colnames(PEV3.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV3.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during non-irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV3.all.df)


# C-L for D3 ----------------------------------------------------------
phase = "D3"
CT.prob_raw.df = data.frame()
CT.prob_up.df = data.frame()
CT.det_raw.df = data.frame()
CT.det_up.df = data.frame()

# over 5

temp.obs.dphase = obs.sri.dphase.ts[,5] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.raw.dphase = rawensmean.sri.dphase.ts[,5] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.det.up.dphase = upensmean.sri.dphase.ts[,5] %>% as.vector() %>% xts(., ymd(predictdate$x))#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,5] %>% as.vector() %>% xts(., ymd(predictdate$x))#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,5] %>% as.vector() %>% xts(., ymd(predictdate$x))#

CT.prob_raw.df = cal_contingency(temp.prob.raw.dphase, temp.obs.dphase) %>% rbind(CT.prob_raw.df, .)
CT.prob_up.df = cal_contingency(temp.prob.up.dphase, temp.obs.dphase) %>% rbind(CT.prob_up.df, .)
CT.det_raw.df = cal_contingency(temp.det.raw.dphase, temp.obs.dphase) %>% rbind(CT.det_raw.df, .)
CT.det_up.df = cal_contingency(temp.det.up.dphase, temp.obs.dphase) %>% rbind(CT.det_up.df, .)

CT.all.df = rbind(CT.prob_raw.df, CT.prob_up.df, CT.det_raw.df, CT.det_up.df)


for (ii in c(1:nrow(CT.all.df))){
  if (ii==1) PEV.all.df = data.frame(cal_PEV(CT.all.df[ii,], temp.obs.dphase))
  else PEV.all.df = merge(PEV.all.df, cal_PEV(CT.all.df[ii,], temp.obs.dphase), by="RATIO")
}
colnames(PEV.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0, 1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))
detach(PEV.all.df)


period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)

####################irrigation period
temp2.obs.dphase = month(temp.obs.dphase) %in% period.irr %>% temp.obs.dphase[.]
temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.irr %>% temp.prob.raw.dphase[.]
temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% period.irr %>% temp.prob.up.dphase[.]
temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% period.irr %>% temp.det.raw.dphase[.]
temp2.det.up.dphase = month(temp.det.up.dphase) %in% period.irr %>% temp.det.up.dphase[.]
CT2.prob_raw.df = data.frame()
CT2.prob_up.df = data.frame()
CT2.det_raw.df = data.frame()
CT2.det_up.df = data.frame()


CT2.prob_raw.df = cal_contingency(temp2.prob.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_raw.df, .)
CT2.prob_up.df = cal_contingency(temp2.prob.up.dphase, temp2.obs.dphase) %>% rbind(CT2.prob_up.df, .)
CT2.det_raw.df = cal_contingency(temp2.det.raw.dphase, temp2.obs.dphase) %>% rbind(CT2.det_raw.df, .)
CT2.det_up.df = cal_contingency(temp2.det.up.dphase, temp2.obs.dphase) %>% rbind(CT2.det_up.df, .)

CT2.all.df = rbind(CT2.prob_raw.df, CT2.prob_up.df, CT2.det_raw.df, CT2.det_up.df)


for (ii in c(1:nrow(CT2.all.df))){
  if (ii==1) PEV2.all.df = data.frame(cal_PEV(CT2.all.df[ii,], temp2.obs.dphase))
  else PEV2.all.df = merge(PEV2.all.df, cal_PEV(CT2.all.df[ii,], temp2.obs.dphase), by="RATIO")
}
colnames(PEV2.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV2.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV2.all.df)

######################## non irrigation period
temp3.obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.]
temp3.prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.]
temp3.prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.]
temp3.det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.]
temp3.det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.]
CT3.prob_raw.df = data.frame()
CT3.prob_up.df = data.frame()
CT3.det_raw.df = data.frame()
CT3.det_up.df = data.frame()


CT3.prob_raw.df = cal_contingency(temp3.prob.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_raw.df, .)
CT3.prob_up.df = cal_contingency(temp3.prob.up.dphase, temp3.obs.dphase) %>% rbind(CT3.prob_up.df, .)
CT3.det_raw.df = cal_contingency(temp3.det.raw.dphase, temp3.obs.dphase) %>% rbind(CT3.det_raw.df, .)
CT3.det_up.df = cal_contingency(temp3.det.up.dphase, temp3.obs.dphase) %>% rbind(CT3.det_up.df, .)

CT3.all.df = rbind(CT3.prob_raw.df, CT3.prob_up.df, CT3.det_raw.df, CT3.det_up.df)


for (ii in c(1:nrow(CT3.all.df))){
  if (ii==1) PEV3.all.df = data.frame(cal_PEV(CT3.all.df[ii,], temp3.obs.dphase))
  else PEV3.all.df = merge(PEV3.all.df, cal_PEV(CT3.all.df[ii,], temp3.obs.dphase), by="RATIO")
}
colnames(PEV3.all.df) = c("RATIO", "prob_raw", "prob_up", "det_raw", "det_up")
attach(PEV3.all.df)
win.graph()
plot(RATIO, prob_raw, type="l", col="black", ylim=c(0,1), ylab="PEV", xlab="COST/LOSS ratio", main=paste("C-L analysis for", toupper(dindexname), phase, "on", bsnname, "during non-irrigation"))
lines(RATIO, prob_up, col="red")
lines(RATIO, det_raw, lty=2, col="black")
lines(RATIO, det_up, lty=2, col="red")
grid()
legend("topleft", legend=c("PP (raw EDP)", "PP (up-EDP)", "DP (raw EDP)", "DP (up-EDP)"), 
       col=c("black", "red", "black", "red"), lty=c(1,1,2,2))

detach(PEV3.all.df)

