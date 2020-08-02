# SRI ensemble prediction with 1 month lead 
esplist = list.files("./esp1month_csv")

ensbasincode = readxl::read_excel("basin_numbers.xlsx");
ensbasincode$BSN_number

y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1
AREA = bsnarea[as.character(targetbsn)]

predicttime = data.frame(Y=year(sri.ts), M=month(sri.ts))

ens.sri.mat = data.frame()
# for (pt in which(as.numeric(sri.ts)<(-1))){
for (pt in c(1:(nrow(predicttime)))){
  pt=250
  ensname = esplist[intersect(grep("*_cms.csv$",esplist), which(substr(esplist, 5, 8)==predicttime$Y[pt]&substr(esplist, 9, 10)==sprintf("%02d", predicttime$M[pt])))]
  # ensname = paste0("ESP_", predicttime$Y[pt], sprintf("%02d", predicttime$M[pt]), "_31_cms.csv")
  
  ensdata = read.csv(file.path("esp1month_csv",ensname), header=F)
  colnames(ensdata) = c("Y", "M", "D", ensbasincode$BSN_number)
  ens_target = cbind(ensdata[,1], cms2mm(ensdata[as.character(targetbsn)], AREA)) %>% setNames(., c("Y", "x"))
  monthly_ens_target = ens_target %>% group_by(Y) %>% summarise(mean(x)) %>% setNames(., c("Y", "x"))
  monthly_ens_target = monthly_ens_target$Y %in% c(y_start:y_end) %>% monthly_ens_target[.,]
  nesp = nrow(monthly_ens_target)
  
  tempidx.input.predicttime = which(year(inputts_target) == predicttime$Y[pt] & month(inputts_target) == predicttime$M[pt])
  ens.cum = c()
  for (ee in c(1:nesp)){
    ens.cum[ee] = sum(monthly_ens_target$x[ee], as.vector(inputts_target[(tempidx.input.predicttime-timescale+1):(tempidx.input.predicttime-1)]))
  }
  
  evalname = paste0("p",fitfunc, "(ens.cum, distparam.sri[predicttime$M[pt],1], distparam.sri[predicttime$M[pt],2])")
  ens.sri = sdi.transform(eval(parse(text=evalname)))
  obs.sri = sri.ts[which(year(sri.ts)==predicttime$Y[pt] & month(sri.ts)==predicttime$M[pt])]
  ensmean.sri = mean(ens.sri)
  ens.sri.mat = rbind(ens.sri.mat, ens.sri)
  
  # ens.sri.dens = density(ens.sri)
  # plot(ens.sri.dens, xlim=c(-3,3), main=paste0("SRI", timescale, " at ",predicttime$Y[pt],sprintf("%02d", predicttime$M[pt]))); rug(ens.sri)
  # abline(v = as.numeric(obs.sri), col="red")
  # abline(v = ensmean.sri, col="blue")
  # 
  # legend("topright", legend = c("Obs.", "Ensemble", "Ensemble mean"), lty=c(1,1,1), col=c("red", "black", "blue"))
}


colnames(ens.sri.mat) = c(1:ncol(ens.sri.mat))
ensmean.sri.ts = apply(ens.sri.mat, 1, mean) %>% xts(., date(head(sri.ts, nrow(ens.sri.mat))))
sri.ts.clip = sri.ts[1:length(ensmean.sri.ts)]
cor(sri.ts.clip, ensmean.sri.ts)
cor(sri.ts.clip, ensmean.sri.ts_)
win.graph()
plot(sri.ts.clip, main=paste0("SRI",timescale)); lines(ensmean.sri.ts_, col = "red")
lines(ensmean.sri.ts, col="blue")
legend("topleft", legend = c("Obs.", "Ensemble mean"), lty=c(1,1), col=c("black", "red"))
integrate(approxfun(ens.sri.dens), lower=-1.5, upper = -0.5)
# 
(ens.sri.ts = as.data.frame(xts(ens.sri.mat, date(head(sri.ts, nrow(ens.sri.mat))))))
write.csv(ens.sri.ts, file.path("./ensSRI", paste0("ens_sri", timescale, "_", bsnname, ".csv")), row.names=T)
