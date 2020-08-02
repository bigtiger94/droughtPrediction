#### load obs SRI
timescale = 3
indexname = "SRI"
dindexname = 'sri3'
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

kfoldRMSE.mat = matrix(ncol=length(bsnnames), nrow=8)
kfoldParam.mat = matrix(ncol=length(bsnnames), nrow=24)

bsncase = 7
for (bsncase in c(1:8)){
  targetbsn = bsncodes[bsncase];
  bsnname = bsnnames[bsncase];
  
  obsfilepath = file.path("./observations")
  sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
  
  #### SMI from apcc
  apccfilepath = file.path("./observations/apcc_nc")
  smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
    xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))
  
  temp_smi.ts = merge(sri.df, smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%setNames(., c("YEAR", "MONTH", colnames(sri.df), "SMI"))
  
  # Likelihood k-fold 1 -----------------------------------------------------
  calibyear = c(2000:2013)
  ## linear model b/w sri and smi (empirical model)
  
  validyear = setdiff(temp_smi.ts$YEAR, calibyear)
  calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
  valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]
  
  
  mm = 1
  calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
  calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
  temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,])%>% setNames(.,c("SMI", colnames(calib_sri.mm)))
  temp_lm = lm(SMI ~ SRI3, data=temp_df)
  
  lmplottitle = paste("Linear model at", bsnname, "on", mm)
  lmfilepath = file.path("./likelihood", bsnname)
  if(!file.exists(lmfilepath)) dir.create(lmfilepath);
  lmfilepath = file.path("./likelihood", bsnname, "smi2sri3")
  if(!file.exists(lmfilepath)) dir.create(lmfilepath);
  
  jpeg(file=file.path(lmfilepath, paste0(lmplottitle,".jpeg")))
  par(mfrow=c(2,2))
  plot(temp_lm)
  dev.off()
  # print(mm)
  # print(shapiro.test(temp_lm$residuals))
  lm_smi2sri3.df = data.frame(MONTH=mm, as.list(temp_lm$coefficients), SD=sd(temp_lm$residuals)) %>% 
    setNames(., c("MONTH", "b0", "b1", "SD"))
  
  for (mm in c(2:12)){
    calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (mm-1)]
    calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
    temp_df = data.frame(calib_smi.mm, calib_sri.mm)%>% setNames(.,c("SMI", colnames(calib_sri.mm)))  
    temp_lm = lm(SMI ~ SRI3, data=temp_df)
    lmplottitle = paste("Linear model at", bsnname, "on", mm)
    jpeg(file=file.path(lmfilepath, paste0(lmplottitle,".jpeg")))
    par(mfrow=c(2,2))
    plot(temp_lm)
    dev.off()
    # print(mm)
    # print(shapiro.test(temp_lm$residuals))
    lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
  }
  
  # lm.smi.fitted = c()
  # for (tt in c(1:(nrow(calib_smi.ts)-1))){
  #   if (tt%%12 == 0) mm = 12 else mm = tt%%12
  #   lm.smi.fitted[tt] = + lm_smi2sri3.df$b0[mm] + temp_smi.ts$SRI3[tt+1] * lm_smi2sri3.df$b1[mm]
  # }
  
  lm.smi.fitted = temp_smi.ts$SRI3 * lm_smi2sri3.df$b1[temp_smi.ts$MONTH] + lm_smi2sri3.df$b0[temp_smi.ts$MONTH]
  
  lm.smi.fitted = lm.smi.fitted[-1] %>% xts(., seq(as.Date(start(temp_smi.ts)), by = "month", to = end(temp_smi.ts)-1))
  
  valid_lm.smi.fitted = year(lm.smi.fitted) %in% validyear %>% lm.smi.fitted[.]
  calib_lm.smi.fitted = year(lm.smi.fitted) %in% calibyear %>% lm.smi.fitted[.]
  
  k1.valid_lm.err = valid_smi.ts$SMI - valid_lm.smi.fitted
  k1.calib_lm.err = calib_smi.ts$SMI - calib_lm.smi.fitted
  
  k.b0.df = data.frame(k1=lm_smi2sri3.df$b0)
  k.b1.df = data.frame(k1=lm_smi2sri3.df$b1)
  
  
  
  # Likelihood k-fold 2 -----------------------------------------------------
  calibyear = c(2000:2009, 2014:2017)
  
  ## linear model b/w sri and smi (empirical model)
  validyear = setdiff(temp_smi.ts$YEAR, calibyear)
  calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
  valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]
  
  mm = 1
  calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
  calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
  temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,])%>% setNames(.,c("SMI", colnames(calib_sri.mm)))
  temp_lm = lm(SMI ~ SRI3, data=temp_df)
  
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
    calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (mm-1)]
    calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
    temp_df = data.frame(calib_smi.mm, calib_sri.mm)%>% setNames(.,c("SMI", colnames(calib_sri.mm)))  
    temp_lm = lm(SMI ~ SRI3, data=temp_df)
    # lmplottitle = paste("Linear model at", bsnname, "on", mm)
    # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
    # par(mfrow=c(2,2))
    # plot(temp_lm)
    # dev.off()
    # print(mm)
    # print(shapiro.test(temp_lm$residuals))
    lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
  }
  
  # lm.smi.fitted = c()
  # for (tt in c(1:(nrow(calib_smi.ts)-1))){
  #   if (tt%%12 == 0) mm = 12 else mm = tt%%12
  #   lm.smi.fitted[tt] = + lm_smi2sri3.df$b0[mm] + temp_smi.ts$SRI3[tt+1] * lm_smi2sri3.df$b1[mm]
  # }
  
  lm.smi.fitted = temp_smi.ts$SRI3 * lm_smi2sri3.df$b1[temp_smi.ts$MONTH] + lm_smi2sri3.df$b0[temp_smi.ts$MONTH]
  
  lm.smi.fitted = lm.smi.fitted[-1] %>% xts(., seq(as.Date(start(temp_smi.ts)), by = "month", to = end(temp_smi.ts)-1))
  
  valid_lm.smi.fitted = year(lm.smi.fitted) %in% validyear %>% lm.smi.fitted[.]
  calib_lm.smi.fitted = year(lm.smi.fitted) %in% calibyear %>% lm.smi.fitted[.]
  
  k2.valid_lm.err = valid_smi.ts$SMI - valid_lm.smi.fitted
  k2.calib_lm.err = calib_smi.ts$SMI - calib_lm.smi.fitted
  
  k.b0.df = cbind(k.b0.df, lm_smi2sri3.df$b0)
  k.b1.df = cbind(k.b1.df, lm_smi2sri3.df$b1)
  
  
  
  # Likelihood k-fold 3 -----------------------------------------------------
  calibyear = c(2000:2005, 2010:2017)
  
  ## linear model b/w sri and smi (empirical model)
  validyear = setdiff(temp_smi.ts$YEAR, calibyear)
  calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
  valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]
  
  
  mm = 1
  calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
  calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
  temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,])%>% setNames(.,c("SMI", colnames(calib_sri.mm)))
  temp_lm = lm(SMI ~ SRI3, data=temp_df)
  
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
    calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (mm-1)]
    calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
    temp_df = data.frame(calib_smi.mm, calib_sri.mm) %>% setNames(.,c("SMI", colnames(calib_sri.mm)))  
    temp_lm = lm(SMI ~ SRI3, data=temp_df)
    # lmplottitle = paste("Linear model at", bsnname, "on", mm)
    # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
    # par(mfrow=c(2,2))
    # plot(temp_lm)
    # dev.off()
    # print(mm)
    # print(shapiro.test(temp_lm$residuals))
    lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
  }
  
  # lm.smi.fitted = c()
  # for (tt in c(1:(nrow(calib_smi.ts)-1))){
  #   if (tt%%12 == 0) mm = 12 else mm = tt%%12
  #   lm.smi.fitted[tt] = + lm_smi2sri3.df$b0[mm] + temp_smi.ts$SRI3[tt+1] * lm_smi2sri3.df$b1[mm]
  # }
  
  lm.smi.fitted = temp_smi.ts$SRI3 * lm_smi2sri3.df$b1[temp_smi.ts$MONTH] + lm_smi2sri3.df$b0[temp_smi.ts$MONTH]
  
  lm.smi.fitted = lm.smi.fitted[-1] %>% xts(., seq(as.Date(start(temp_smi.ts)), by = "month", to = end(temp_smi.ts)-1))
  
  valid_lm.smi.fitted = year(lm.smi.fitted) %in% validyear %>% lm.smi.fitted[.]
  calib_lm.smi.fitted = year(lm.smi.fitted) %in% calibyear %>% lm.smi.fitted[.]
  
  k3.valid_lm.err = valid_smi.ts$SMI - valid_lm.smi.fitted
  k3.calib_lm.err = calib_smi.ts$SMI - calib_lm.smi.fitted
  
  k.b0.df = cbind(k.b0.df, lm_smi2sri3.df$b0)
  k.b1.df = cbind(k.b1.df, lm_smi2sri3.df$b1)
  
  
  # Likelihood k-fold 4 -----------------------------------------------------
  calibyear = c(2000:2001, 2006:2017)
  
  ## linear model b/w sri and smi (empirical model)
  validyear = setdiff(temp_smi.ts$YEAR, calibyear)
  calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
  valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]
  
  
  mm = 1
  calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
  calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
  temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,])%>% setNames(.,c("SMI", colnames(calib_sri.mm)))
  temp_lm = lm(SMI ~ SRI3, data=temp_df)
  
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
    calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (mm-1)]
    calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:(3+ncol(sri.df)-1)]
    temp_df = data.frame(calib_smi.mm, calib_sri.mm)%>% setNames(.,c("SMI", colnames(calib_sri.mm)))  
    temp_lm = lm(SMI ~ SRI3, data=temp_df)
    # lmplottitle = paste("Linear model at", bsnname, "on", mm)
    # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
    # par(mfrow=c(2,2))
    # plot(temp_lm)
    # dev.off()
    # print(mm)
    # print(shapiro.test(temp_lm$residuals))
    lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
  }
  # lm.smi.fitted = c()
  # for (tt in c(1:(nrow(calib_smi.ts)-1))){
  #   if (tt%%12 == 0) mm = 12 else mm = tt%%12
  #   lm.smi.fitted[tt] = + lm_smi2sri3.df$b0[mm] + temp_smi.ts$SRI3[tt+1] * lm_smi2sri3.df$b1[mm]
  # }
  
  lm.smi.fitted = temp_smi.ts$SRI3 * lm_smi2sri3.df$b1[temp_smi.ts$MONTH] + lm_smi2sri3.df$b0[temp_smi.ts$MONTH]
  
  lm.smi.fitted = lm.smi.fitted[-1] %>% xts(., seq(as.Date(start(temp_smi.ts)), by = "month", to = end(temp_smi.ts)-1))
  
  valid_lm.smi.fitted = year(lm.smi.fitted) %in% validyear %>% lm.smi.fitted[.]
  calib_lm.smi.fitted = year(lm.smi.fitted) %in% calibyear %>% lm.smi.fitted[.]
  
  k4.valid_lm.err = valid_smi.ts$SMI - valid_lm.smi.fitted
  k4.calib_lm.err = calib_smi.ts$SMI - calib_lm.smi.fitted
  
  k.b0.df = cbind(k.b0.df, lm_smi2sri3.df$b0)
  k.b1.df = cbind(k.b1.df, lm_smi2sri3.df$b1)
  
  
  
  # k-fold summary ----------------------------------------------------------
  colnames(k.b0.df) = c("k1", "k2", "k3", "k4")
  colnames(k.b1.df) = c("k1", "k2", "k3", "k4")
  
  for(kk in c(1:4)){
  
    runstring = paste0("temp.calib.err=k",kk,".calib_lm.err")
    eval(parse(text=runstring))
    # monthly_caliberr = month(temp.calib.err) %in% mm %>% temp.calib.err[.]
    kfoldRMSE.mat[kk,bsncase] = sqrt(mean(temp.calib.err^2))
    
    runstring = paste0("temp.valid.err=k",kk,".valid_lm.err")
    eval(parse(text=runstring))
    # monthly_validerr = month(temp.valid.err) %in% mm %>% temp.valid.err[.]
    kfoldRMSE.mat[kk+4,bsncase] = sqrt(mean(temp.valid.err^2))
  }
  kfoldParam.mat[1:12,bsncase] = apply(k.b0.df, 1, mean)
  kfoldParam.mat[13:24,bsncase] = apply(k.b1.df, 1, mean)
}
colnames(kfoldParam.mat) = bsnnames; rownames(kfoldParam.mat) = c(1:12, 1:12)
colnames(kfoldRMSE.mat) = bsnnames; rownames(kfoldRMSE.mat) = rep(c("k1", "k2", "k3", "k4"), 2)
write.csv(kfoldParam.mat, file.path("./likelihood", paste0("parameters_b0b1_kmeans_",dindexname,".csv")))
write.csv(kfoldRMSE.mat, file.path("./likelihood", paste0("RMSE_calib_valid_kfold",dindexname,".csv")))
# mm = 1
# temp_caliberr = month(k1.calib_lm.err) %in% mm %>% k1.calib_lm.err[.]
# sqrt(mean(temp_caliberr^2))
# temp_validerr = month(k1.valid_lm.err) %in% mm %>% k1.valid_lm.err[.]
# sqrt(mean(temp_validerr^2))

# for (mm in c(1:12)){
#   print(mm)
#   # month(k1.calib_lm.err) %in% mm %>% k1.calib_lm.err[.] %>% sd(.) %>% print(.)
#   # month(k1.valid_lm.err) %in% mm %>% k1.valid_lm.err[.] %>% sd(.) %>% print(.)
#   temp_caliberr = month(k1.calib_lm.err) %in% mm %>% k1.calib_lm.err[.]
#   sqrt(mean(temp_caliberr^2))
#   temp_validerr = month(k1.valid_lm.err) %in% mm %>% k1.valid_lm.err[.]
#   sqrt(mean(temp_validerr^2)) %>% print(.)
# }
# for (mm in c(1:12)){
#   print(mm)
#   # month(k2.calib_lm.err) %in% mm %>% k2.calib_lm.err[.] %>% sd(.) %>% print(.)
#   # month(k2.valid_lm.err) %in% mm %>% k2.valid_lm.err[.] %>% sd(.) %>% print(.)
#   temp_caliberr = month(k3.calib_lm.err) %in% mm %>% k3.calib_lm.err[.]
#   sqrt(mean(temp_caliberr^2)) %>% print(.)
#   temp_validerr = month(k3.valid_lm.err) %in% mm %>% k3.valid_lm.err[.]
#   sqrt(mean(temp_validerr^2)) %>% print(.)
#   
# }
# k.b0.df
# k.b1.df



# Leave one out cv ---------------------------------------------------------
# library(caret)
# 
# bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
# bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
# obsfilepath = file.path("./observations")
# apccfilepath = file.path("./observations/apcc_nc")
# 
# for (bsncase in c(7:8)){
# targetbsn = bsncodes[bsncase];
# bsnname = bsnnames[bsncase];
# 
# sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
# 
# #### SMI from apcc
# smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
#      xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))
# 
# temp_smi.ts = merge(sri.df, smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%setNames(., c("YEAR", "MONTH", colnames(sri.df), "SMI"))
# 
# mm = 1
# temp_smi.mm = temp_smi.ts$SMI[month(temp_smi.ts) == (12)]
# temp_sri.mm = temp_smi.ts[month(temp_smi.ts) == mm, 3:5]
# temp_df = data.frame(temp_smi.mm[-length(temp_smi.mm),], temp_sri.mm[-1,]) %>% setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1", "SRI1_lead1"))
# 
# 
# loocv.lm.mm = train(SMI~SRI3_lead1, method="lm", data=temp_df, trControl=trainControl(method="LOOCV"))
# 
# # print(summary(loocv.lm.mm))
# 
# lmvalidity = data.frame(pvalue = summary(loocv.lm.mm)[4]$coefficients[2,4], R2 = loocv.lm.mm$results[3])
# 
# for (mm in c(2:12)){
#   temp_smi.mm = temp_smi.ts$SMI[month(temp_smi.ts) == (mm-1)]
#   temp_sri.mm = temp_smi.ts[month(temp_smi.ts) == mm, 3:5]
#   temp_df = data.frame( temp_smi.mm, temp_sri.mm) %>% setNames(.,c("SMI", colnames(temp_sri.mm)))
#   loocv.lm.mm = train(SMI~SRI3, method="lm", data=temp_df, trControl=trainControl(method="LOOCV"))
#   # print(mm)
#   # print(summary(loocv.lm.mm))
#   
#   # lmplottitle = paste("Linear model at", bsnname, "on", mm)
#   # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
#   # par(mfrow=c(2,2))
#   # plot(temp_lm)
#   # dev.off()
#   # print(mm)
#   # print(shapiro.test(temp_lm$residuals))
#   # lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
#   lmvalidity = rbind(lmvalidity, data.frame(pvalue = summary(loocv.lm.mm)[4]$coefficients[2,4], R2 = loocv.lm.mm$results[3]))
# }
# 
# write.csv(lmvalidity, file.path("./likelihood", paste0("LMvalidity_",dindexname,"_",bsnname,".csv")))
# }
