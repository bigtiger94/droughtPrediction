#### ground water from Kwater
gwfilepath =file.path("C:/Users/Daeho/Desktop/Research/연구자료/EDP/observations", "groundwater")
sy.gw.df = read.csv(file.path(gwfilepath, "gw_obs_soyang.csv"))



temp_gw.ts = xts(sy.gw.df$X82008, ymd(sy.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
sy.sgi.df = data.frame(Time = date(temp_gw.ts))
sy.sgi.df = cbind(sy.sgi.df, as.data.frame(temp_sgi.ts))

temp_gw.ts = xts(sy.gw.df$X95515, ymd(sy.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
sy.sgi.df = cbind(sy.sgi.df, as.data.frame(temp_sgi.ts))

temp_gw.ts = xts(sy.gw.df$X84011, ymd(sy.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
sy.sgi.df = cbind(sy.sgi.df, as.data.frame(temp_sgi.ts))

temp_gw.ts = xts(sy.gw.df$X03546, ymd(sy.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
sy.sgi.df = cbind(sy.sgi.df, as.data.frame(temp_sgi.ts))
colnames(sy.sgi.df) = colnames(sy.gw.df)
write.csv(sy.sgi.df, file.path(gwfilepath, "sgi_soyang.csv"), row.names=F)


## dc
dc.gw.df = read.csv(file.path(gwfilepath, "gw_obs_daecheong.csv"))


temp_gw.ts = xts(dc.gw.df$X65070, ymd(dc.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
dc.sgi.df = data.frame(Time = date(temp_gw.ts))
dc.sgi.df = cbind(dc.sgi.df, as.data.frame(temp_sgi.ts))

temp_gw.ts = xts(dc.gw.df$X09858, ymd(dc.gw.df$Time)) %>% apply.monthly(., mean, na.action=na.pass ) %>% xts(., ymd(paste(year(.), month(.), 1, sep="-")))
# plot(temp_gw.ts)
# hist(temp_gw.ts)
temp_gw.mat = matrix(temp_gw.ts, ncol=12, byrow=T); rownames(temp_gw.mat) = unique(year(date(temp_gw.ts)))
temp_sgi.mat = calSGI(temp_gw.mat) 
temp_sgi.ts = xts(as.vector(t(temp_sgi.mat)), date(temp_gw.ts))
# plot(temp_sgi.ts)
dc.sgi.df = cbind(dc.sgi.df, as.data.frame(temp_sgi.ts))
colnames(dc.sgi.df) = colnames(dc.gw.df)

write.csv(dc.sgi.df, file.path(gwfilepath, "sgi_daechoeng.csv"), row.names=F)
