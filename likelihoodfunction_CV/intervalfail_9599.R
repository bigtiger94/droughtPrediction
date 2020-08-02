dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

lmintervalpath = file.path("./likelihood", "interval9599")

CIfail95_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))
PIfail95_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))
CIfail99_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))
PIfail99_monthly = data.frame(matrix(NaN, ncol=12, nrow=length(bsncodes))) %>% setNames(., c(1:12))

for (bsncase in c(1:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     CI95.df = read.csv(file.path("./likelihood", paste0("LMCI_0.05_",dindexname,"_",bsnname,".csv")), 
                      row.names=1) %>% xts(., ymd(rownames(.)))
     PI95.df = read.csv(file.path("./likelihood", paste0("LMPI_0.05_",dindexname,"_",bsnname,".csv")), 
                      row.names=1) %>% xts(., ymd(rownames(.)))
     CI99.df = read.csv(file.path("./likelihood", paste0("LMCI_0.01_",dindexname,"_",bsnname,".csv")), 
                        row.names=1) %>% xts(., ymd(rownames(.)))
     PI99.df = read.csv(file.path("./likelihood", paste0("LMPI_0.01_",dindexname,"_",bsnname,".csv")), 
                        row.names=1) %>% xts(., ymd(rownames(.)))
     
     CI95.df$FAIL = (CI95.df$OBS>CI95.df$UB | CI95.df$OBS<CI95.df$LB) 
     PI95.df$FAIL = (PI95.df$OBS>PI95.df$UB | PI95.df$OBS<PI95.df$LB)
     
     CI99.df$FAIL = (CI99.df$OBS>CI99.df$UB | CI99.df$OBS<CI99.df$LB) 
     PI99.df$FAIL = (PI99.df$OBS>PI99.df$UB | PI99.df$OBS<PI99.df$LB)
     for (MONTH in c(1:12)){
          CIfail95_monthly[bsncase, MONTH] = month(CI95.df) %in% MONTH %>% CI95.df$FAIL[.] %>% sum(.)
          PIfail95_monthly[bsncase, MONTH] = month(PI95.df) %in% MONTH %>% PI95.df$FAIL[.] %>% sum(.)
          CIfail99_monthly[bsncase, MONTH] = month(CI99.df) %in% MONTH %>% CI99.df$FAIL[.] %>% sum(.)
          PIfail99_monthly[bsncase, MONTH] = month(PI99.df) %in% MONTH %>% PI99.df$FAIL[.] %>% sum(.)
          
     }
     # png(file.path(lmintervalpath, paste0("CI_9599_",dindexname,"_",bsnname,".png")), width=1600, height=900)
     # 
     # tsplotname = paste("Empirical model confidence interval on", bsnname)
     # 
     # print(plot(CI95.df$OBS, type="b", lwd=2, main=tsplotname, ylim=c(-0.5, 1.5), 
     #      cex.lab = 3, cex.axis=3))
     # print(lines(CI95.df$PRED, lty=1, lwd=2, col="red"))
     # print(lines(CI95.df$UB, lty=2, lwd=2, col="darkgrey"))
     # print(lines(CI95.df$LB, lty=2, lwd=2, col="darkgrey"))
     # print(lines(CI99.df$UB, lty=3, lwd=2, col="grey"))
     # print(lines(CI99.df$LB, lty=3, lwd=2, col="grey"))
     # 
     # print(addLegend(legend.loc = "topright", 
     #           legend.names = c("Obs.", "Pred.", "95% CI interval", "99% CI interval"), 
     #           lty=c(1,1,2,3), col=c("black", "red", "darkgrey", "grey"), 
     #           bg="white", bty="o", cex = 2.5))
     # dev.off() 
     # 
     # png(file.path(lmintervalpath, paste0("PI_9599_",dindexname,"_",bsnname,".png")), width=1600, height=900)
     # tsplotname = paste("Empirical model prediction interval on", bsnname)
     # 
     # print(plot(PI95.df$OBS, type="b", lwd=2, main=tsplotname, ylim=c(-0.5, 1.5), 
     #      cex.lab = 3, cex.axis=3))
     # print(lines(PI95.df$PRED, lty=1, lwd=2, col="red"))
     # print(lines(PI95.df$UB, lty=2, lwd=2, col="darkgrey"))
     # print(lines(PI95.df$LB, lty=2, lwd=2, col="darkgrey"))
     # print(lines(PI99.df$UB, lty=3, lwd=2, col="grey"))
     # print(lines(PI99.df$LB, lty=3, lwd=2, col="grey"))
     # 
     # print(addLegend(legend.loc = "topright", 
     #           legend.names = c("Obs.", "Pred.", "95% PI interval", "99% PI interval"), 
     #           lty=c(1,1,2,3), col=c("black", "red", "darkgrey", "grey"), 
     #           bg="white", bty="o", cex = 2.5))
     # dev.off()
}

CIfail95_monthly$total = apply(CIfail95_monthly[,1:12], 1, sum); rownames(CIfail95_monthly) = bsnnames
PIfail95_monthly$total = apply(PIfail95_monthly[,1:12], 1, sum); rownames(PIfail95_monthly) = bsnnames

CIfail99_monthly$total = apply(CIfail99_monthly[,1:12], 1, sum); rownames(CIfail99_monthly) = bsnnames
PIfail99_monthly$total = apply(PIfail99_monthly[,1:12], 1, sum); rownames(PIfail99_monthly) = bsnnames


write.csv(CIfail99_monthly, file.path("./likelihood", paste0("loocv_CI99%_fail_",dindexname,".csv")))
write.csv(PIfail99_monthly, file.path("./likelihood", paste0("loocv_PI99%_fail_",dindexname,".csv")))
