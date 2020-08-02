# load prediction data ----------------------------------------------------
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju")
bsncodes = c(1012, 3008, 2001, 4001, 1003)

bsncase = 4
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
kk = 1
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


BS.tot.prob_up.df = data.frame()
BS.tot.det_up.df = data.frame()
BS.tot.prob_raw.df = data.frame()
BS.tot.det_raw.df = data.frame()


# cal BS over D0 ----------------------------------------------------------
phase = "D0"
BS.prob_raw.df = data.frame()
BS.prob_up.df = data.frame()
BS.det_raw.df = data.frame()
BS.det_up.df = data.frame()

# over 2

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1], 1, sum)#

# temp.obs.sri.dphase = obs.sri.dphase[,5]
dOccuridx = which(temp.obs.dphase==1) 
temp.obs.dphase = temp.obs.dphase[dOccuridx] %>% xts(., ymd(names(.)))

temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[dOccuridx,-1], 1, sum)%>% xts(., ymd(names(.)))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[dOccuridx,-1], 1, sum)%>% xts(., ymd(names(.)))#
# temp.det.sri.dphase = ensmean.sri.dphase[,5]

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[dOccuridx,2]#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[dOccuridx,2]#
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

BSrowname=c("ALL")


for (mm in c(1:12)){
  temp2.obs.dphase = month(temp.obs.dphase) %in% mm %>% temp.obs.dphase[.,]
  if(length(temp2.obs.dphase)){
    temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% mm %>% temp.det.raw.dphase[.]
    temp2.det.up.dphase = month(temp.det.up.dphase) %in% mm %>% temp.det.up.dphase[.]
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% mm %>% temp.prob.raw.dphase[.]
    temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% mm %>% temp.prob.up.dphase[.]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp2.prob.raw.dphase, temp2.obs.dphase)
    temp.BS_up.prob = calBS(temp2.prob.up.dphase, temp2.obs.dphase)
    temp.BS_raw.det = calBS(temp2.det.raw.dphase, temp2.obs.dphase)
    temp.BS_up.det = calBS(temp2.det.up.dphase, temp2.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
    BSrowname = cbind(BSrowname, mm)
  }
  
}

rownames(BS.prob_raw.df) = BSrowname 
rownames(BS.det_raw.df) = BSrowname

rownames(BS.prob_up.df) = BSrowname 
rownames(BS.det_up.df) = BSrowname

which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1

which(BS.prob_up.df$bs < BS.det_up.df$bs) -1

BS.prob_up.df
BS.det_up.df

win.graph()
plot(c(1:nrow(BS.det_up.df)), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
     ylim=c(0, 1), ylab="Brier Score", main=paste("BS for", phase),
     type="b", pch="x", col="grey", lty=2)
lines(c(1:nrow(BS.det_up.df)), BS.prob_up.df$bs, type="b", pch="x", col="red")
lines(c(1:nrow(BS.det_up.df)), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
axis(side=1, at=1:nrow(BS.det_up.df), labels=rownames(BS.det_up.df))
grid()
legend("topleft", legend=c("PP (up-EDP)", "PP (raw EDP)", "DP (up-EDP"), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))


# cal BS over D1 ----------------------------------------------------------
phase = "D1"
BS.prob_raw.df = data.frame()
BS.prob_up.df = data.frame()
BS.det_raw.df = data.frame()
BS.det_up.df = data.frame()

# over 3

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-2], 1, sum)#

# temp.obs.sri.dphase = obs.sri.dphase[,5]
dOccuridx = which(temp.obs.dphase==1) 
temp.obs.dphase = temp.obs.dphase[dOccuridx] %>% xts(., ymd(names(.)))

temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[dOccuridx,-1:-2], 1, sum)%>% xts(., ymd(names(.)))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[dOccuridx,-1:-2], 1, sum)%>% xts(., ymd(names(.)))#
# temp.det.sri.dphase = ensmean.sri.dphase[,5]

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[dOccuridx,3]#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[dOccuridx,3]#
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

BSrowname=c("ALL")

for (mm in c(1:12)){
  temp2.obs.dphase = month(temp.obs.dphase) %in% mm %>% temp.obs.dphase[.,]
  if(length(temp2.obs.dphase)){
    temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% mm %>% temp.det.raw.dphase[.]
    temp2.det.up.dphase = month(temp.det.up.dphase) %in% mm %>% temp.det.up.dphase[.]
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% mm %>% temp.prob.raw.dphase[.]
    temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% mm %>% temp.prob.up.dphase[.]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp2.prob.raw.dphase, temp2.obs.dphase)
    temp.BS_up.prob = calBS(temp2.prob.up.dphase, temp2.obs.dphase)
    temp.BS_raw.det = calBS(temp2.det.raw.dphase, temp2.obs.dphase)
    temp.BS_up.det = calBS(temp2.det.up.dphase, temp2.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
    BSrowname = cbind(BSrowname, mm)
  }
  
}

rownames(BS.prob_raw.df) = BSrowname 
rownames(BS.det_raw.df) = BSrowname

rownames(BS.prob_up.df) = BSrowname 
rownames(BS.det_up.df) = BSrowname

which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1

which(BS.prob_up.df$bs < BS.det_up.df$bs) -1

BS.prob_up.df
BS.det_up.df
win.graph()
plot(c(1:nrow(BS.det_up.df)), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
     ylim=c(0, 1), ylab="Brier Score", main=paste("BS for", phase),
     type="b", pch="x", col="grey", lty=2)
lines(c(1:nrow(BS.det_up.df)), BS.prob_up.df$bs, type="b", pch="x", col="red")
lines(c(1:nrow(BS.det_up.df)), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
axis(side=1, at=1:nrow(BS.det_up.df), labels=rownames(BS.det_up.df))
grid()
legend("topleft", legend=c("PP (up-EDP)", "PP (raw EDP)", "DP (up-EDP"), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))


# cal BS over D2 ------------------------------------------------------------------
phase = "D2"
BS.prob_raw.df = data.frame()
BS.prob_up.df = data.frame()
BS.det_raw.df = data.frame()
BS.det_up.df = data.frame()

# over 4

temp.obs.dphase = apply(obs.sri.dphase.ts[,-1:-3], 1, sum)#

# temp.obs.sri.dphase = obs.sri.dphase[,5]
dOccuridx = which(temp.obs.dphase==1) 
temp.obs.dphase = temp.obs.dphase[dOccuridx] %>% xts(., ymd(names(.)))

temp.det.raw.dphase = apply(rawensmean.sri.dphase.ts[dOccuridx,-1:-3], 1, sum)%>% xts(., ymd(names(.)))#
temp.det.up.dphase = apply(upensmean.sri.dphase.ts[dOccuridx,-1:-3], 1, sum)%>% xts(., ymd(names(.)))#
# temp.det.sri.dphase = ensmean.sri.dphase[,5]

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[dOccuridx,4]#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[dOccuridx,4]#
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

BSrowname=c("ALL")


for (mm in c(1:12)){
  temp2.obs.dphase = month(temp.obs.dphase) %in% mm %>% temp.obs.dphase[.,]
  if(length(temp2.obs.dphase)){
    temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% mm %>% temp.det.raw.dphase[.]
    temp2.det.up.dphase = month(temp.det.up.dphase) %in% mm %>% temp.det.up.dphase[.]
    # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
    
    temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% mm %>% temp.prob.raw.dphase[.]
    temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% mm %>% temp.prob.up.dphase[.]
    # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
    
    temp.BS_raw.prob = calBS(temp2.prob.raw.dphase, temp2.obs.dphase)
    temp.BS_up.prob = calBS(temp2.prob.up.dphase, temp2.obs.dphase)
    temp.BS_raw.det = calBS(temp2.det.raw.dphase, temp2.obs.dphase)
    temp.BS_up.det = calBS(temp2.det.up.dphase, temp2.obs.dphase)
    
    BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
    BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
    BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
    BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
    BSrowname = cbind(BSrowname, mm)
  }
  
}

rownames(BS.prob_raw.df) = BSrowname 
rownames(BS.det_raw.df) = BSrowname

rownames(BS.prob_up.df) = BSrowname 
rownames(BS.det_up.df) = BSrowname

which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1

which(BS.prob_up.df$bs < BS.det_up.df$bs) -1

BS.prob_up.df
BS.det_up.df

win.graph()
plot(c(1:nrow(BS.det_up.df)), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
     ylim=c(0, 1), ylab="Brier Score", main=paste("BS for", phase),
     type="b", pch="x", col="grey", lty=2)
lines(c(1:nrow(BS.det_up.df)), BS.prob_up.df$bs, type="b", pch="x", col="red")
lines(c(1:nrow(BS.det_up.df)), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
axis(side=1, at=1:nrow(BS.det_up.df), labels=rownames(BS.det_up.df))
grid()
legend("topleft", legend=c("PP (up-EDP)", "PP (raw EDP)", "DP (up-EDP"), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))




# cal BS over D3 ------------------------------------------------------------------
phase = "D3"
BS.prob_raw.df = data.frame()
BS.prob_up.df = data.frame()
BS.det_raw.df = data.frame()
BS.det_up.df = data.frame()

# over 5

temp.obs.dphase = obs.sri.dphase.ts[,5]#

dOccuridx = which(temp.obs.dphase==1) 
temp.obs.dphase = temp.obs.dphase[dOccuridx]

temp.det.raw.dphase = rawensmean.sri.dphase.ts[dOccuridx,5] #
temp.det.up.dphase = upensmean.sri.dphase.ts[dOccuridx,5]#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[dOccuridx,5]#
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[dOccuridx,5]#
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

BSrowname=c("ALL")


# for (mm in c(1:12)){
#   temp2.obs.dphase = month(temp.obs.dphase) %in% mm %>% temp.obs.dphase[.,]
#   if(length(temp2.obs.dphase)){
#     temp2.det.raw.dphase = month(temp.det.raw.dphase) %in% mm %>% temp.det.raw.dphase[.]
#     temp2.det.up.dphase = month(temp.det.up.dphase) %in% mm %>% temp.det.up.dphase[.]
#     # temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)
#     
#     temp2.prob.raw.dphase = month(temp.prob.raw.dphase) %in% mm %>% temp.prob.raw.dphase[.]
#     temp2.prob.up.dphase = month(temp.prob.up.dphase) %in% mm %>% temp.prob.up.dphase[.]
#     # temp.prob.sri.dphase = subset(prob.sri.dphase.over[,5], month(prob.sri.dphase.over[,5])==mm)#
#     
#     temp.BS_raw.prob = calBS(temp2.prob.raw.dphase, temp2.obs.dphase)
#     temp.BS_up.prob = calBS(temp2.prob.up.dphase, temp2.obs.dphase)
#     temp.BS_raw.det = calBS(temp2.det.raw.dphase, temp2.obs.dphase)
#     temp.BS_up.det = calBS(temp2.det.up.dphase, temp2.obs.dphase)
#     
#     BS.prob_raw.df = rbind(BS.prob_raw.df, temp.BS_raw.prob[1,])
#     BS.prob_up.df = rbind(BS.prob_up.df, temp.BS_up.prob[1,])
#     BS.det_raw.df = rbind(BS.det_raw.df, temp.BS_raw.det[1,])
#     BS.det_up.df = rbind(BS.det_up.df, temp.BS_up.det[1,])
#     BSrowname = cbind(BSrowname, mm)
#   }
#   
# }

rownames(BS.prob_raw.df) = BSrowname 
rownames(BS.det_raw.df) = BSrowname

rownames(BS.prob_up.df) = BSrowname 
rownames(BS.det_up.df) = BSrowname

which(BS.prob_up.df$bs > BS.prob_raw.df$bs) -1

which(BS.prob_up.df$bs < BS.det_up.df$bs) -1

BS.prob_up.df
BS.det_up.df
win.graph()

plot(c(1:nrow(BS.det_up.df)), BS.det_up.df$bs, xaxt = "n", xlab="Time [month]",
     ylim=c(0, 1), ylab="Brier Score", main=paste("BS for", phase),
     type="b", pch="x", col="grey", lty=2)
lines(c(1:nrow(BS.det_up.df)), BS.prob_up.df$bs, type="b", pch="x", col="red")
lines(c(1:nrow(BS.det_up.df)), BS.prob_raw.df$bs, type="b", pch="o", col="blue", lwd=0.1)
axis(side=1, at=1:nrow(BS.det_up.df), labels=rownames(BS.det_up.df))
grid()
legend("topleft", legend=c("PP (up-EDP)", "PP (raw EDP)", "DP (up-EDP"), col=c("red", "blue", "grey"), lty=c(1,1,2), pch=c("x", "o", "x"))

