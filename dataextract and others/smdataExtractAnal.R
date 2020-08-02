smfilepath = file.path(getwd(), "observations", "soilmoisture")
flist = list.files(smfilepath)


dcfname = grep("daily_daecheong.csv$", flist)
ii = dcfname
sm = read.csv(file.path(smfilepath, flist[ii]), header=F) %>% setNames(., c("loc1", "loc2", "Time", "sm"))
sm$sm[sm$sm == "-"] = NA
sm.ts = as.numeric(substr(sm$sm, 1, 5)) %>% xts(., as.Date(sm$Time))
dc.sm.ts = apply.monthly(sm.ts, mean, na.rm=T)%>%xts(., ymd(paste(year(.), month(.), 1, sep="-")))
syfname = grep("daily_soyang.csv$", flist)
ii = syfname
sm = read.csv(file.path(smfilepath, flist[ii]), header=F) %>% setNames(., c("loc1", "loc2", "Time", "sm"))
sm$sm[sm$sm == "-"] = NA
sm.ts = as.numeric(substr(sm$sm, 1, 5)) %>% xts(., as.Date(sm$Time))
sy.sm.ts = apply.monthly(sm.ts, mean, na.rm=T)%>%xts(., ymd(paste(year(.), month(.), 1, sep="-")))


temp.sm.ts = merge(sy.sm.ts, smi1012.ts, all = F)
cor(temp.sm.ts, use="complete.obs")
# pairs(as.matrix(temp.sm.ts))

temp.sm.ts = merge(dc.sm.ts, smi3008.ts, all = F)
cor(temp.sm.ts, use="complete.obs")
# pairs(as.matrix(temp.sm.ts))

temp.ts = merge(dc.sm.ts, sri.ts, all=F)
ccf(as.vector(temp.ts$sri.ts), as.vector(temp.ts$x), na.action=na.pass, main="SRI3 vs Soil Moisture observation")

## ssi

temp_dc.sm.df = data.frame(dc.sm.ts, Month = month(dc.sm.ts))
mean_eachMonth = temp_dc.sm.df %>% group_by(Month) %>% summarise(arr = mean(x, na.rm=T))
sd_eachMonth = temp_dc.sm.df %>% group_by(Month) %>% summarise(arr = sd(x, na.rm=T))
dc.sm.df = cbind(temp_dc.sm.df, mean_eachMonth[,2], sd_eachMonth[,2]) %>% setNames(., c("SM", "Month", "Mean", "Sd"))
dc.sm.df$ssi = (dc.sm.df$SM-dc.sm.df$Mean)/dc.sm.df$Sd
write.csv(dc.sm.df, file.path(smfilepath, "sm_daecheong.csv"))


temp_sy.sm.df = data.frame(sy.sm.ts, Month = month(sy.sm.ts))
mean_eachMonth = temp_sy.sm.df %>% group_by(Month) %>% summarise(Mean = mean(x, na.rm=T) )
sd_eachMonth = temp_sy.sm.df %>% group_by(Month) %>% summarise(Sd = sd(x, na.rm=T))
sy.sm.df = cbind(temp_sy.sm.df, mean_eachMonth[,2], sd_eachMonth[,2]) %>% setNames(., c("SM", "Month", "Mean", "Sd"))
sy.sm.df$ssi = (sy.sm.df$SM-sy.sm.df$Mean)/sy.sm.df$Sd
write.csv(sy.sm.df, file.path(smfilepath, "sm_soyang.csv"))

