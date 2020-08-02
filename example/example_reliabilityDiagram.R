dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

bsncase = 1
# for (bsncase in c(1:5)){
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)

kk=1
kfold = paste0("k", kk)
predictfilepath = file.path("./predictResult", kfold)
calibyear = kfoldinfo[kk,]

predictdate = read.csv(file.path(predictfilepath, "date.csv"), row.names=1)

prob.rawenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_rawEDP_",dindexname,"_binary_",bsnname,".csv")),
                                         row.names=1) %>% xts(., ymd(predictdate$x))
prob.upenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_upEDP_",dindexname,"_binary_",bsnname,".csv")),
                                        row.names=1) %>% xts(., ymd(predictdate$x))
obs.sri.dphase.ts = read.csv(file.path(predictfilepath, paste0("obs_",dindexname,"_",bsnname,".csv")),
                             row.names=1) %>% xts(., ymd(predictdate$x))

validyear = setdiff(year(predictdate$x), calibyear)
validindex = year(predictdate$x) %in% validyear
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)




# D0 ----------------------------------------------------------------------
phase= "D0"
period = "all"
temp.obs.dphase = obs.sri.dphase.ts[,-1] %>% apply(., 1, sum) %>% xts(., ymd(names(.)))#

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,2]
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,2]

temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                         bins = 7, plot=F)
temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                        bins = 7, plot=F)
win.graph()
plotname = paste("Reliability diagram for", phase, "at", bsnname)
obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
     main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue")
lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red")
abline(h=obs.freq, lty=2, col="grey")
abline(v=obs.freq, lty=2, col="grey")
# lines(c(0,1), c(obs.freq/2, (1+obs.freq)/2), lty=2)
grid()
legend("topleft", legend=c("EDP", "EDP+"), col=c("blue", "red"),
       lty=c(1,1,2), bg = "white")
 # graphics.off()


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


period = "non-irrigation"
temp..obs.dphase = month(temp.obs.dphase) %in% period.nonirr %>% temp.obs.dphase[.,] 
# temp.obs.sri.dphase = subset(obs.sri.dphase[,5], month(obs.sri.dphase)==mm)

temp..det.raw.dphase = month(temp.det.raw.dphase) %in% period.nonirr %>% temp.det.raw.dphase[.,]
temp..det.up.dphase = month(temp.det.up.dphase) %in% period.nonirr %>% temp.det.up.dphase[.,]
# temp.det.sri.dphase = subset(ensmean.sri.dphase[,5], month(ensmean.sri.dphase)==mm)

temp..prob.raw.dphase = month(temp.prob.raw.dphase) %in% period.nonirr %>% temp.prob.raw.dphase[.,]
temp..prob.up.dphase = month(temp.prob.up.dphase) %in% period.nonirr %>% temp.prob.up.dphase[.,]


#D1 ----------------------------------------------------------
phase = "D1"; 
period = "all"
# over 2

temp.obs.dphase = obs.sri.dphase.ts[,-1:-2] %>% 
     apply(., 1, sum) %>% xts(., ymd(names(.)))#
temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,3]
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,3]#


temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                         bins = 7, plot=F)
temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                        bins = 7, plot=F)
win.graph()
plotname = paste("Reliability diagram for", phase, "at", bsnname)
obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
     main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue")
lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red")
abline(h=obs.freq, lty=2, col="grey")
abline(v=obs.freq, lty=2, col="grey")
# lines(c(0,1), c(obs.freq/2, (1+obs.freq)/2), lty=2)
grid()
legend("topleft", legend=c("EDP", "EDP+", "No skill"), col=c("blue", "red", "black"),
       lty=c(1,1,2), bg = "white")
# graphics.off()

#  D2 ------------------------------------------------------------------
phase = "D2"
period = "all"

temp.obs.dphase = obs.sri.dphase.ts[,-1:-3] %>% 
     apply(., 1, sum) %>% xts(., ymd(names(.)))#
temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,4]
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,4]#


temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                         bins = 7, plot=F)
temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                        bins = 7, plot=F)
win.graph()
plotname = paste("Reliability diagram for", phase, "at", bsnname)
obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
     main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue")
lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red")
abline(h=obs.freq, lty=2, col="grey")
abline(v=obs.freq, lty=2, col="grey")
# lines(c(0,1), c(obs.freq/2, (1+obs.freq)/2), lty=2)
grid()
legend("topleft", legend=c("EDP", "EDP+", "No skill"), col=c("blue", "red", "black"),
       lty=c(1,1,2), bg = "white")
#  D3 ------------------------------------------------------------------
phase = "D3"
period = "all"

temp.obs.dphase = obs.sri.dphase.ts[,5] 

temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,5]
temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,5]#


temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                         bins = 7, plot=F)
temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                        bins = 7, plot=F)

win.graph()
plotname = paste("Reliability diagram for", phase, "at", bsnname)
obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
     main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue")
lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red")
abline(h=obs.freq, lty=2, col="grey")
abline(v=obs.freq, lty=2, col="grey")
# lines(c(0,1), c(obs.freq/2, (1+obs.freq)/2), lty=2)
grid()
legend("topleft", legend=c("EDP", "EDP+", "No skill"), col=c("blue", "red", "black"),
       lty=c(1,1,2), bg = "white")
