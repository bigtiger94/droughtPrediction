dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
RELdiagrampath = file.path("./predictResult", "reliabilityDiagrm")
bsncase = 1
for (bsncase in c(1:length(bsnnames))){
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
     
     apccpredictdate = read.csv(file.path(predictfilepath, "apccdate.csv"), row.names=1)
     prob.apccrawenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_apccrawEDP_",dindexname,"_binary_",bsnname,".csv")),
                                                  row.names=1) %>% xts(., ymd(apccpredictdate$x))
     prob.apccupenssri.dphase.over.ts = read.csv(file.path(predictfilepath, paste0("probforecast_apccupEDP_",dindexname,"_binary_",bsnname,".csv")),
                                                  row.names=1) %>% xts(., ymd(apccpredictdate$x))
     
     prob.rawenssri.dphase.over.ts = date(prob.rawenssri.dphase.over.ts) %in% ymd(apccpredictdate$x) %>% prob.rawenssri.dphase.over.ts[.]
     prob.upenssri.dphase.over.ts = date(prob.upenssri.dphase.over.ts) %in% ymd(apccpredictdate$x) %>% prob.upenssri.dphase.over.ts[.]
     obs.sri.dphase.ts = date(obs.sri.dphase.ts) %in% ymd(apccpredictdate$x) %>% obs.sri.dphase.ts[.]
     
     calibyear = intersect(as.numeric(calibyear), year(apccpredictdate$x))
     validyear = setdiff(year(apccpredictdate$x), calibyear)
     validindex = year(predictdate$x) %in% validyear
     period.irr = c(4:9)
     period.nonirr = setdiff(c(1:12), period.irr)
     
     
     
     
     # D0 ----------------------------------------------------------------------
     phase= "D0"
     period = "all"
     temp.obs.dphase = obs.sri.dphase.ts[,-1] %>% apply(., 1, sum) %>% xts(., ymd(names(.)))#
     
     temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,2]
     temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,2]
     
     temp.prob.apccraw.dphase = prob.apccrawenssri.dphase.over.ts[,2]
     temp.prob.apccup.dphase = prob.apccupenssri.dphase.over.ts[,2]
     
     temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                              bins = 7, plot=F)
     temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                             bins = 7, plot=F)
     
     temp.prob.apccraw.verif = ReliabilityDiagram(probs=temp.prob.apccraw.dphase, obs=temp.obs.dphase,
                                              bins = 7, plot=F)
     temp.prob.apccup.verif = ReliabilityDiagram(probs=temp.prob.apccup.dphase, obs=temp.obs.dphase,
                                             bins = 7, plot=F)
     
     
     plotname = paste("Reliability diagram of", toupper(dindexname), "with APCC for", phase, "at", bsnname)
     jpeg(file.path(RELdiagrampath, paste0(plotname,".jpg")))
     obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
     plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
          main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
     lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue", lwd=2)
     lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red", lwd=2)
     lines(temp.prob.apccraw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="black", lwd=1)
     lines(temp.prob.apccup.verif$p.avgs, temp.prob.up.verif$cond.probs, col="green", lwd=2)
     abline(h=obs.freq, lty=2, col="darkgrey")
     abline(v=obs.freq, lty=2, col="darkgrey")
     grid()
     legend("topleft", legend=c("EDP", "EDP+S", "EDP+A", "EDP+AS"), col=c("blue", "red", "black", "green"),
            lty=c(1,1,1, 1), bg = "white")
     dev.off()

     #D1 ----------------------------------------------------------
     phase = "D1"; 
     period = "all"
     # over 2
     
     temp.obs.dphase = obs.sri.dphase.ts[,-1:-2] %>% 
          apply(., 1, sum) %>% xts(., ymd(names(.)))#
     
     temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,3]
     temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,3]#
     
     temp.prob.apccraw.dphase = prob.apccrawenssri.dphase.over.ts[,3]
     temp.prob.apccup.dphase = prob.apccupenssri.dphase.over.ts[,3]
     
     temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                              bins = 7, plot=F)
     temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                             bins = 7, plot=F)
     
     temp.prob.apccraw.verif = ReliabilityDiagram(probs=temp.prob.apccraw.dphase, obs=temp.obs.dphase,
                                                  bins = 7, plot=F)
     temp.prob.apccup.verif = ReliabilityDiagram(probs=temp.prob.apccup.dphase, obs=temp.obs.dphase,
                                                 bins = 7, plot=F)
     
     plotname = paste("Reliability diagram of", toupper(dindexname), "with APCC for", phase, "at", bsnname)
     jpeg(file.path(RELdiagrampath, paste0(plotname,".jpg")))
     obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
     plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
          main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
     lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue", lwd=2)
     lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red", lwd=2)
     lines(temp.prob.apccraw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="black", lwd=1)
     lines(temp.prob.apccup.verif$p.avgs, temp.prob.up.verif$cond.probs, col="green", lwd=2)
     abline(h=obs.freq, lty=2, col="darkgrey")
     abline(v=obs.freq, lty=2, col="darkgrey")
     grid()
     legend("topleft", legend=c("EDP", "EDP+S", "EDP+A", "EDP+AS"), col=c("blue", "red", "black", "green"),
            lty=c(1,1,1, 1), bg = "white")
     dev.off()
     
     #  D2 ------------------------------------------------------------------
     phase = "D2"
     period = "all"
     
     temp.obs.dphase = obs.sri.dphase.ts[,-1:-3] %>% 
          apply(., 1, sum) %>% xts(., ymd(names(.)))#
     temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,4]
     temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,4]#
     
     temp.prob.apccraw.dphase = prob.apccrawenssri.dphase.over.ts[,4]
     temp.prob.apccup.dphase = prob.apccupenssri.dphase.over.ts[,4]
     
     temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                              bins = 7, plot=F)
     temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                             bins = 7, plot=F)
     
     temp.prob.apccraw.verif = ReliabilityDiagram(probs=temp.prob.apccraw.dphase, obs=temp.obs.dphase,
                                                  bins = 7, plot=F)
     temp.prob.apccup.verif = ReliabilityDiagram(probs=temp.prob.apccup.dphase, obs=temp.obs.dphase,
                                                 bins = 7, plot=F)
     
     plotname = paste("Reliability diagram of", toupper(dindexname), "with APCC for", phase, "at", bsnname)
     jpeg(file.path(RELdiagrampath, paste0(plotname,".jpg")))
     obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
     plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
          main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
     lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue", lwd=2)
     lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red", lwd=2)
     lines(temp.prob.apccraw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="black", lwd=1)
     lines(temp.prob.apccup.verif$p.avgs, temp.prob.up.verif$cond.probs, col="green", lwd=2)
     abline(h=obs.freq, lty=2, col="darkgrey")
     abline(v=obs.freq, lty=2, col="darkgrey")
     grid()
     legend("topleft", legend=c("EDP", "EDP+S", "EDP+A", "EDP+AS"), col=c("blue", "red", "black", "green"),
            lty=c(1,1,1, 1), bg = "white")
     dev.off()
     
     #  D3 ------------------------------------------------------------------
     phase = "D3"
     period = "all"
     
     temp.obs.dphase = obs.sri.dphase.ts[,5] 
     temp.prob.raw.dphase = prob.rawenssri.dphase.over.ts[,5]
     temp.prob.up.dphase = prob.upenssri.dphase.over.ts[,5]#
     
     temp.prob.apccraw.dphase = prob.apccrawenssri.dphase.over.ts[,3]
     temp.prob.apccup.dphase = prob.apccupenssri.dphase.over.ts[,3]
     
     temp.prob.raw.verif = ReliabilityDiagram(probs=temp.prob.raw.dphase, obs=temp.obs.dphase,
                                              bins = 7, plot=F)
     temp.prob.up.verif = ReliabilityDiagram(probs=temp.prob.up.dphase, obs=temp.obs.dphase,
                                             bins = 7, plot=F)
     
     temp.prob.apccraw.verif = ReliabilityDiagram(probs=temp.prob.apccraw.dphase, obs=temp.obs.dphase,
                                                  bins = 7, plot=F)
     temp.prob.apccup.verif = ReliabilityDiagram(probs=temp.prob.apccup.dphase, obs=temp.obs.dphase,
                                                 bins = 7, plot=F)

     plotname = paste("Reliability diagram of", toupper(dindexname), "with APCC for", phase, "at", bsnname)
     jpeg(file.path(RELdiagrampath, paste0(plotname,".jpg")))
     
     obs.freq = sum(temp.obs.dphase)/length(temp.obs.dphase)
     plot(seq(0, 1, 0.1), seq(0, 1, 0.1), type="l", lty=4, col="black", lwd=5, ylim=c(0,1), xlim=c(0,1),
          main = plotname, ylab="Observed Frequency", xlab="Forecast Probability")
     lines(temp.prob.raw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="blue", lwd=2)
     lines(temp.prob.up.verif$p.avgs, temp.prob.up.verif$cond.probs, col="red", lwd=2)
     lines(temp.prob.apccraw.verif$p.avgs, temp.prob.raw.verif$cond.probs, col="black", lwd=1)
     lines(temp.prob.apccup.verif$p.avgs, temp.prob.up.verif$cond.probs, col="green", lwd=2)
     abline(h=obs.freq, lty=2, col="darkgrey")
     abline(v=obs.freq, lty=2, col="darkgrey")
     grid()
     legend("topleft", legend=c("EDP", "EDP+S", "EDP+A", "EDP+AS"), col=c("blue", "red", "black", "green"),
            lty=c(1,1,1, 1), bg = "white")
     dev.off()
}

# graphics.off()
