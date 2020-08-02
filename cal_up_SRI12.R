#  SRI12 with
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
bsncase = 1
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
kk = 4
# for (kk in c(1:4)){
kfold = paste0("k", kk)
calibyear = as.numeric(kfoldinfo[kk,])

## load observation
#### load obs SRI
obsfilepath = file.path("./observations")
sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
sricolidx = which(colnames(sri.df)==toupper(dindexname))

## load ensemble information
ensfilepath = file.path("./ensSRI")
# list.files(ensfilepath)
ens_sri.df = read.csv(file.path(ensfilepath, paste0("ens_",dindexname,"_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))

#### SMI from apcc
mergingindices = c("SMI")
apccfilepath = file.path("./observations/apcc_nc")
smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
  xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))


temp_ens = ens_sri.df################
temp_enssri.df = year(temp_ens) %in% c(calibyear[1]:2017) %>% temp_ens[.,]

temp_enssri.stat.df = data.frame(year(temp_enssri.df), month(temp_enssri.df), apply(temp_enssri.df, 1, mean), apply(temp_enssri.df, 1, sd)) %>%
  setNames(., c("YEAR", "MONTH", "MEAN", "SD")) %>% xts(., ymd(rownames(.)))


temp_merged.ts = merge(sri.df, smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%###############
setNames(., c("YEAR", "MONTH", colnames(sri.df), mergingindices))




# likelihood function for sri12 (k1) ------------------------------------------------

validyear = setdiff(temp_merged.ts$YEAR, calibyear)
calib_merged.ts = temp_merged.ts$YEAR %in% calibyear %>% temp_merged.ts[.,]
valid_merged.ts = temp_merged.ts$YEAR %in% validyear %>% temp_merged.ts[.,]

mergingindices

mm = 1
calib_smi.mm = calib_merged.ts$SMI[month(calib_merged.ts) == (12)]
calib_sri.mm = calib_merged.ts[month(calib_merged.ts) == mm, 3:4]
temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,]) %>% setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
temp_lm = lm(SMI ~ SRI12_lead1, data=temp_df)

# lmplottitle = paste("Linear model at", bsnname, "on", mm)
# jpeg(file=file.path("./likelihood", bsnname, "smi2sri12", paste0(lmplottitle,".jpeg")))
# par(mfrow=c(2,2))
# plot(temp_lm)
# dev.off()
# print(mm)
# print(shapiro.test(temp_lm$residuals))
lm_smi2sri12.df = data.frame(MONTH=mm, as.list(temp_lm$coefficients), SD=sd(temp_lm$residuals)) %>% 
  setNames(., c("MONTH", "b0", "b1", "SD"))

for (mm in c(2:12)){
  calib_smi.mm = calib_merged.ts$SMI[month(calib_merged.ts) == (mm-1)]
  calib_sri.mm = calib_merged.ts[month(calib_merged.ts) == mm, 3:4]
  temp_df = data.frame( calib_smi.mm, calib_sri.mm) %>%setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
  temp_lm = lm(SMI ~ SRI12_lead1, data=temp_df)
  
  # lmplottitle = paste("Linear model at", bsnname, "on", mm)
  # jpeg(file=file.path("./likelihood", bsnname, "smi2sri12", paste0(lmplottitle,".jpeg")))
  # par(mfrow=c(2,2))
  # plot(temp_lm)
  # dev.off()
  # print(mm)
  # print(shapiro.test(temp_lm$residuals))
  lm_smi2sri12.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
}



# bayes theorem -----------------------------------------------------------
## prior: Ensemble SRI (EDP)
## likelihood: regression SRI(t) to SMI(t-1)
# temp_merged.ts
# temp_enssri.stat.df 
# lm_smi2sri12.df

bcalibyear = calibyear[-1]
bvalidyear = validyear
bcalib_idx = which(year(temp_merged.ts) %in% bcalibyear)
bvalid_idx = which(year(temp_merged.ts) %in% bvalidyear)
# bcalib_enssri.idx = which(temp_enssri.stat.df$YEAR %in% bcalibyear)

upens.sd = c()
upens.mean = c()
for (tt in union(bcalib_idx, bvalid_idx)){
  mm = as.vector(temp_merged.ts$MONTH[tt])
  
  as.vector(temp_merged.ts$SRI12[tt])*lm_smi2sri12.df$b1[mm] + lm_smi2sri12.df$b0[mm]
  temp_merged.ts$SMI[tt-1]
  
  upens.est.lm = (as.vector(temp_merged.ts$SMI[tt-1])-lm_smi2sri12.df$b0[mm])/lm_smi2sri12.df$b1[mm]
  
  upens.sd[tt-12] = sqrt(1/(1/temp_enssri.stat.df$SD[tt]^2 + lm_smi2sri12.df$b1[mm]^2/lm_smi2sri12.df$SD[mm]^2))
  upens.mean[tt-12] = upens.sd[tt-12]^2 * (temp_enssri.stat.df$MEAN[tt]/temp_enssri.stat.df$SD[tt]^2 + upens.est.lm*lm_smi2sri12.df$b1[mm]^2/lm_smi2sri12.df$SD[mm]^2)
}
upens.sd.ts = xts(upens.sd, date(temp_merged.ts[union(bcalib_idx, bvalid_idx)]))
upens.mean.ts = xts(upens.mean, date(temp_merged.ts[union(bcalib_idx, bvalid_idx)]))

enssavefilepath = file.path(ensfilepath, kfold) 
if(!file.exists(enssavefilepath)) dir.create(enssavefilepath);


temp_plotts = merge(temp_merged.ts$SRI12, temp_enssri.stat.df$MEAN, upens.mean.ts, all=F)
win.graph(width = 600, height=400)
plotname = paste(toupper(dindexname), "on", bsnname)
# jpeg(file.path(enssavefilepath, paste0(plotname,".jpg")), width=900, height=500)
plot.xts(temp_plotts[97:nrow(temp_plotts)], main=plotname, col=c("black", "blue", "red"))
addLegend("topleft", legend.names=c("Obs.", "EDP", "EDP+S"), lty=1, lwd=2,
          col=c("black", "blue", "red"), cex=2, bty="o")
# dev.off()

# graphics.off()


(RMSE_rawEDP = sqrt(mean(as.vector(temp_plotts[,1] - temp_plotts[,2])^2)))
(RMSE_upEDP = sqrt(mean(as.vector(temp_plotts[,1] - temp_plotts[,3])^2)))

upens.stat.df = data.frame(YEAR=year(upens.mean.ts), MONTH=month(upens.mean.ts), MEAN=upens.mean.ts, SD=upens.sd.ts)

enssavefilepath = file.path(ensfilepath, kfold) 
if(!file.exists(enssavefilepath)) dir.create(enssavefilepath);


# write.csv(date(temp_enssri.stat.df), file.path(enssavefilepath, "rawensdate.csv"))
# write.csv(temp_enssri.stat.df, file.path("./ensSRI", paste0("meansd_rawens_",dindexname,"_",bsnname,".csv")))
# write.csv(upens.stat.df, file.path(enssavefilepath, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")))
# write.csv(rownames(upens.stat.df), file.path(enssavefilepath, "upensdate.csv"))
# }