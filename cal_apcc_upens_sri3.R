dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
bsncase = 1

targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

sri.df = read.csv(file.path("./observations", paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))


forecastfilepath = file.path("./observations/apcc_nc/forecastdata")
prfdata = read.csv(file.path(forecastfilepath,paste0("probprforecast_",bsnname,".csv")))
prf.ts = xts(prfdata[,3:5], ymd(prfdata$Predicttime))

ensfilepath = file.path("./ensSRI")
# list.files(ensfilepath)
ens_sri.df = read.csv(file.path(ensfilepath, paste0("ens_",dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))

temp_enssri.df = year(ens_sri.df) %in% year(prf.ts) %>% ens_sri.df[.,]

temp_enssri.stat.df = data.frame(year(temp_enssri.df), month(temp_enssri.df), apply(temp_enssri.df, 1, mean), apply(temp_enssri.df, 1, sd)) %>%
     setNames(., c("YEAR", "MONTH", "MEAN", "SD")) %>% xts(., ymd(rownames(.)))



sd. = temp_enssri.stat.df$SD / (qnorm(1-prf.ts$above/100)-qnorm(prf.ts$below/100))
mean. = temp_enssri.stat.df$MEAN-0.5*temp_enssri.stat.df$SD - (sd.*qnorm(prf.ts$below/100))

win.graph()
year(sri.df$SRI3)%in%year(mean.) %>% sri.df$SRI3[.] %>% plot(.)
lines(mean., col="blue")
lines(temp_enssri.stat.df$MEAN, col="red")
addLegend("topleft", legend.names=c("obs", "EDP w/ APCC", "raw EDP"), cex=1, bty="o",
          lty=c(1,1,1), col=c("black", "blue", "red"))


sqrt(mean((sri.df$SRI3-temp_enssri.stat.df$MEAN)^2))
sqrt(mean((sri.df$SRI3-mean.)^2))

meansd. = data.frame(YEAR=year(mean.), MONTH=month(mean.), MEAN=mean., SD=sd.) %>% xts(., date(mean.)) 
# meansd. %>% write.csv(., file.path(ensfilepath, paste0("meansd_rawens+apcc_",dindexname,"_",bsnname,".csv")))
# meansd_upens_sri3_andong



# update with bayes -------------------------------------------------------

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
kk = 1
for (kk in c(1:4)){
kfold = paste0("k", kk)
calibyear = as.numeric(kfoldinfo[kk,])

mergingindices = c("SMI")
## load observation
#### load obs SRI
obsfilepath = file.path("./observations")
sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
sricolidx = which(colnames(sri.df)==toupper(dindexname))

#### SMI from apcc
apccfilepath = file.path("./observations/apcc_nc")
smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
     xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))

temp_merged.ts = merge(sri.df, smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%###############
setNames(., c("YEAR", "MONTH", colnames(sri.df), mergingindices))

# likeihood function

validyear = setdiff(temp_merged.ts$YEAR, calibyear)
calib_merged.ts = temp_merged.ts$YEAR %in% calibyear %>% temp_merged.ts[.,]
valid_merged.ts = temp_merged.ts$YEAR %in% validyear %>% temp_merged.ts[.,]


mm = 1
calib_smi.mm = calib_merged.ts$SMI[month(calib_merged.ts) == (12)]
calib_sri.mm = calib_merged.ts[month(calib_merged.ts) == mm, 3:4]
temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,]) %>% setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
temp_lm = lm(SMI ~ SRI3_lead1, data=temp_df)

# lmplottitle = paste("Linear model at", bsnname, "on", mm)
# jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
# par(mfrow=c(2,2))
# plot(temp_lm)
# dev.off()
# print(mm)
# print(shapiro.test(temp_lm$residuals))
lm_smi2sri3.df = data.frame(MONTH=mm, as.list(temp_lm$coefficients), SD=sd(temp_lm$residuals)) %>% 
     setNames(., c("MONTH", "b0", "b1", "SD"))

for (mm in c(2:12)){
     calib_smi.mm = calib_merged.ts$SMI[month(calib_merged.ts) == (mm-1)]
     calib_sri.mm = calib_merged.ts[month(calib_merged.ts) == mm, 3:4]
     temp_df = data.frame( calib_smi.mm, calib_sri.mm) %>%setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
     temp_lm = lm(SMI ~ SRI3_lead1, data=temp_df)
     
     # lmplottitle = paste("Linear model at", bsnname, "on", mm)
     # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
     # par(mfrow=c(2,2))
     # plot(temp_lm)
     # dev.off()
     # print(mm)
     # print(shapiro.test(temp_lm$residuals))
     lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
}



# bayes theorem -----------------------------------------------------------
## prior: Ensemble SRI (EDP)
## likelihood: regression SRI(t) to SMI(t-1)
# temp_smi.ts
# temp_enssri.stat.df 
# lm_smi2sri3.df
bcalibyear = intersect(calibyear, year(meansd.))
bvalidyear = intersect(validyear, year(meansd.))
bcalib_idx = which(year(meansd.) %in% bcalibyear)
bvalid_idx = which(year(meansd.) %in% bvalidyear)
# bcalib_enssri.idx = which(temp_enssri.stat.df$YEAR %in% bcalibyear)

upens.sd = c()
upens.mean = c()
for (tt in union(bcalib_idx, bvalid_idx)){
     
     mm = as.vector(meansd.$MONTH[tt])
     yy = as.vector(meansd.$YEAR[tt])
     smiidx = which(year(smi.ts) == yy & month(smi.ts) == mm)
     as.vector(temp_merged.ts$SRI3[smiidx])*lm_smi2sri3.df$b1[mm] + lm_smi2sri3.df$b0[mm]
     smi.ts[smiidx-1]
     
     upens.est.lm = (as.vector(smi.ts[smiidx-1])-lm_smi2sri3.df$b0[mm])/lm_smi2sri3.df$b1[mm]
     
     upens.sd[tt] = sqrt(1/(1/meansd.$SD[tt]^2 + lm_smi2sri3.df$b1[mm]^2/lm_smi2sri3.df$SD[mm]^2))
     upens.mean[tt] = upens.sd[tt]^2 * (meansd.$MEAN[tt]/meansd.$SD[tt]^2 + upens.est.lm*lm_smi2sri3.df$b1[mm]^2/lm_smi2sri3.df$SD[mm]^2)
}
upens.sd.ts = xts(upens.sd, date(meansd.[union(bcalib_idx, bvalid_idx)]))
upens.mean.ts = xts(upens.mean, date(meansd.[union(bcalib_idx, bvalid_idx)]))



# save --------------------------------------------------------------------

enssavefilepath = file.path(ensfilepath, kfold) 
if(!file.exists(enssavefilepath)) dir.create(enssavefilepath);


temp_plotts = merge(temp_merged.ts$SRI3, temp_enssri.stat.df$MEAN, meansd.$MEAN, upens.mean.ts, all=F)

# graphics.off()
# 
# temp__plotts = year(temp_plotts) %in% union(bcalibyear, bvalidyear) %>% temp_plotts[.]
# sqrt(mean(as.vector(temp__plotts[,1] - temp__plotts[,2])^2))
# sqrt(mean(as.vector(temp__plotts[,1] - temp__plotts[,3])^2))

(RMSE_rawEDP = sqrt(mean(as.vector(temp_plotts[,1] - temp_plotts[,2])^2)))
(RMSE_apccrawEDP = sqrt(mean(as.vector(temp_plotts[,1] - temp_plotts[,3])^2)))
(RMSE_apccupEDP = sqrt(mean(as.vector(temp_plotts[,1] - temp_plotts[,4])^2)))

apccrawens.stat.df = meansd.
apccupens.stat.df = data.frame(YEAR=year(upens.mean.ts), MONTH=month(upens.mean.ts), MEAN=upens.mean.ts, SD=upens.sd.ts)

enssavefilepath = file.path(ensfilepath, kfold) 
if(!file.exists(enssavefilepath)) dir.create(enssavefilepath);
# rownames(apccupens.stat.df) %>% write.csv(., file.path(enssavefilepath, paste0("apccupensdate.csv")))
# apccrawens.stat.df %>% write.csv(., file.path("./ensSRI", paste0("meansd_apccrawens_",dindexname,"_",bsnname,".csv")))
# apccupens.stat.df %>% write.csv(., file.path(enssavefilepath, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")))
# write.csv(rownames(upens.stat.df), file.path(enssavefilepath, "upensdate.csv"))
# 
}