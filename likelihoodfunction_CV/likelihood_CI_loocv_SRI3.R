# prediction intervals
library(caret)
library(dplyr)
library(xts)
library(lubridate)

var_loocv = function(x_series){
     temp_var = c()
     for(ii in c(1:length(x_series))) temp_var[ii] = var(x_series[-ii])
     return(temp_var)
}
mean_loocv = function(x_series){
     temp_mean = c()
     for(ii in c(1:length(x_series))) temp_mean[ii] = mean(x_series[-ii])
     return(temp_mean)
}
PI_loocv = function(lm, x_series, ALPHA){
     n = length(x_series)-1
     temp_res = lm$pred$obs - lm$pred$pred
     temp_xvar = var_loocv(x_series)
     temp_xmean = mean_loocv(x_series)
     temp_resstd = sqrt(var_loocv(temp_res))
     # print(summary(loocv.lm.mm))
     temp_PIrange = qnorm(1-0.5*ALPHA)*temp_resstd*sqrt((1+ 1/n+ (x_series-temp_xmean)^2/(n*temp_xvar)))
     temp_ub = lm$pred$pred + temp_PIrange
     temp_lb = lm$pred$pred - temp_PIrange
     temp_bound.df = data.frame(OBS=lm$pred$obs, PRED=lm$pred$pred, LB=temp_lb, UB=temp_ub)
     rownames(temp_bound.df) = rownames(lm$pred)
     return(temp_bound.df)
}
CI_loocv = function(lm, x_series, ALPHA){
     n = length(x_series)-1
     temp_res = lm$pred$obs - lm$pred$pred
     temp_xvar = var_loocv(x_series)
     temp_xmean = mean_loocv(x_series)
     temp_resstd = sqrt(var_loocv(temp_res))
     # print(summary(loocv.lm.mm))
     temp_CIrange = qnorm(1-0.5*ALPHA)*temp_resstd*sqrt((1/n+(x_series-temp_xmean)^2/(n*temp_xvar)))
     temp_ub = lm$pred$pred + temp_CIrange
     temp_lb = lm$pred$pred - temp_CIrange
     temp_bound.df = data.frame(OBS=lm$pred$obs, PRED=lm$pred$pred, LB=temp_lb, UB=temp_ub)
     rownames(temp_bound.df) = rownames(lm$pred)
     return(temp_bound.df)
}

ALPHA = 0.1
timescale = 3
indexname = "SRI"
dindexname = 'sri3'

bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
obsfilepath = file.path("./observations")
apccfilepath = file.path("./observations/apcc_nc")

for (bsncase in c(1:length(bsncodes))){
# bsncase = 1
     targetbsn = bsncodes[bsncase];
     bsnname = bsnnames[bsncase];
     
     sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))
     
     #### SMI from apcc
     smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
          xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))
     
     temp_smi.ts = merge(sri.df, smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%setNames(., c("YEAR", "MONTH", colnames(sri.df), "SMI"))
     
     mm = 1
     temp_smi.mm = temp_smi.ts$SMI[month(temp_smi.ts) == (12)]
     temp_sri.mm = temp_smi.ts[month(temp_smi.ts) == mm, 3:5]
     temp_df = data.frame(temp_smi.mm[-length(temp_smi.mm),], temp_sri.mm[-1,]) %>% setNames(.,c("SMI", colnames(temp_sri.mm)))
     
     
     loocv.lm.mm = train(SMI~SRI3, method="lm", data=temp_df, trControl=trainControl(method="LOOCV"))
     
 
     CI.df = CI_loocv(loocv.lm.mm, temp_df$SRI3, ALPHA)
     PI.df = PI_loocv(loocv.lm.mm, temp_df$SRI3, ALPHA)
     lmvalidity = data.frame(pvalue = summary(loocv.lm.mm)[4]$coefficients[2,4], R2 = loocv.lm.mm$results[3])
     
     for (mm in c(2:12)){
          temp_smi.mm = temp_smi.ts$SMI[month(temp_smi.ts) == (mm-1)]
          temp_sri.mm = temp_smi.ts[month(temp_smi.ts) == mm, 3:5]
          temp_df = data.frame( temp_smi.mm, temp_sri.mm) %>% setNames(.,c("SMI", colnames(temp_sri.mm)))
          loocv.lm.mm = train(SMI~SRI3, method="lm", data=temp_df, trControl=trainControl(method="LOOCV"))
          # print(mm)
          # print(summary(loocv.lm.mm))
          
          # lmplottitle = paste("Linear model at", bsnname, "on", mm)
          # jpeg(file=file.path("./likelihood", bsnname, "smi2sri3", paste0(lmplottitle,".jpeg")))
          # par(mfrow=c(2,2))
          # plot(temp_lm)
          # dev.off()
          # print(mm)
          # print(shapiro.test(temp_lm$residuals))
          # lm_smi2sri3.df[mm,] = c(mm, temp_lm$coefficients, sd(temp_lm$residuals))
          CI.df = rbind(CI.df, CI_loocv(loocv.lm.mm, temp_df$SRI3, ALPHA))
          PI.df = rbind(PI.df, PI_loocv(loocv.lm.mm, temp_df$SRI3, ALPHA))
          lmvalidity = rbind(lmvalidity, data.frame(pvalue = summary(loocv.lm.mm)[4]$coefficients[2,4], R2 = loocv.lm.mm$results[3]))
     }
     
     CI.df = CI.df %>% arrange(., rownames(.)) %>% `rownames<-`(sort(rownames(CI.df)))
     PI.df = PI.df %>% arrange(., rownames(.)) %>% `rownames<-`(sort(rownames(PI.df)))
     write.csv(CI.df, file.path("./likelihood", paste0("LMCI_",ALPHA,"_",dindexname,"_",bsnname,".csv")))
     write.csv(PI.df, file.path("./likelihood", paste0("LMPI_",ALPHA,"_",dindexname,"_",bsnname,".csv")))
     # write.csv(lmvalidity, file.path("./likelihood", paste0("LMvalidity_",dindexname,"_",bsnname,".csv")))
}
