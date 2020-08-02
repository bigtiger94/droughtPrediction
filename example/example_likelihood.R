###### Likelihood k-fold 1
calibyear = c(2000:2013)

#### load obs SRI
obsfilepath = file.path("./observations")
sy.sri.df = read.csv(file.path(obsfilepath, "sri_soyang.csv"), row.names=1) %>% xts(., ymd(rownames(.)))
dc.sri.df = read.csv(file.path(obsfilepath, "sri_daecheong.csv"), row.names=1) %>% xts(., ymd(rownames(.)))

#### SMI from apcc
apccfilepath = file.path("C:/Users/Daeho/Desktop/Research/연구자료/EDP/observations/apcc_nc")
sy.smi.ts = read.csv(file.path(apccfilepath, "SMI_daily_soyang.csv"), row.names=1) %>% 
  xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))
dc.smi.ts = read.csv(file.path(apccfilepath, "SMI_daily_daecheong.csv"), row.names=1) %>% 
  xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))


## linear model b/w sri and smi (empirical model)

temp_smi.ts = merge(sy.sri.df, sy.smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%setNames(., c("YEAR", "MONTH", colnames(sy.sri.df), "SMI"))

validyear = setdiff(temp_smi.ts$YEAR, calibyear)
calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]


mm = 1
calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:4]
temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,]) %>% setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
temp_lm = lm(SMI ~ SRI3_lead1, data=temp_df)

mm=2
calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (mm-1)]
calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:4]
temp_df = data.frame( calib_smi.mm, calib_sri.mm) %>%setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
temp_lm = lm(SMI ~ SRI3_lead1, data=temp_df)


temp_lm$fitted.values

calib_lm.smi.fitted[month(calib_lm.smi.fitted) ==12 ]
valid_lm.smi.fitted[month(valid_lm.smi.fitted) == 12]
valid_smi.ts$SMI[valid_smi.ts$MONTH==12]

calib_smi.ts$SMI[calib_smi.ts$MONTH==12] - temp_lm$fitted.values
temp_lm$residuals


month(calib_lm.err) %in% 1 %>% calib_lm.err[.] %>% sd(.)
month(calib_lm.err) %in% 1 %>% calib_lm.err[.]
sd(temp_lm$residuals)
sd(calib_lm.err)