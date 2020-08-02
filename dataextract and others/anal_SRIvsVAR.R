#### load obs SRI
timescale = 3
indexname = "SRI"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
bsncase = 1
for (bsncase in c(1:8)){
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

obsfilepath = file.path("./observations")
sri.df = read.csv(file.path(obsfilepath, paste0("sri_",bsnname,".csv")), row.names=1) %>% xts(., ymd(rownames(.)))

#### SMI from apcc
apccfilepath = file.path("./observations/apcc_nc")
smi.ts = read.csv(file.path(apccfilepath, paste0("SMI_daily_",bsnname,".csv")), row.names=1) %>% 
              xts(., ymd(rownames(.))) %>% apply.monthly(., mean) %>% xts(., ymd(paste(year(.), month(.), "1", sep="-")))

temp_smi.ts = merge(sri.df, smi.ts, all=F) %>% setNames(., c(colnames(sri.df), "SMI"))
# temp_smi.ts = merge(sri.df, smi4001.ts, all=F) %>% setNames(., c(colnames(sri.df), "SMI"))
library(ggplot2)
library(forecast)
ggAcf(temp_smi.ts)
# graphics.off()
# cc = ccf(as.vector(temp_smi.ts$SRI1), as.vector(temp_smi.ts$SMI), main=paste("SRI1 vs SMI at",targetbsn))
print(bsnname)
cc = ccf(as.vector(temp_smi.ts$SRI3), as.vector(temp_smi.ts$SMI), main=paste("SRI3 vs SMI at",targetbsn))
print(cc[1])
cc = ccf(as.vector(temp_smi.ts$SRI12), as.vector(temp_smi.ts$SMI), main=paste("SRI12 vs SMI at",targetbsn))
print(cc[1])
# win.graph()
# 
# pairs(as.data.frame(temp_smi.ts), main=paste("Scatter plot at", bsnname))
}

# head(temp.ts[-1,1])
# head(temp.ts[-216,2])
# ac = acf(cbind(as.vector(temp.ts$sri.ts), as.vector(temp.ts$smi3008.ts)))
# pc = pacf(cbind(as.vector(temp_sri.ts), as.vector(temp_smi1012.ts)))
# 
# acc.sri_smi = ac$acf[,1,2]
# pcc.sri_smi = pc$acf[,1,2]

#### prob forecast from APCC
apccfilepath = file.path("./observations/apcc_nc")
sy.pr_apcc.ts = read.csv(file.path(apccfilepath, "probprforecast_soyang.csv"), row.names=1) %>% xts(., ymd(rownames(.)))
dc.pr_apcc.ts = read.csv(file.path(apccfilepath, "probprforecast_daecheong.csv"), row.names=1) %>% xts(., ymd(rownames(.)))

sy.pr_forecast.ts = xts(as.numeric(sy.pr_apcc.ts$X)*10, ymd(sy.pr_apcc.ts$Predicttime))
dc.pr_forecast.ts = xts(as.numeric(dc.pr_apcc.ts$X)*10, ymd(dc.pr_apcc.ts$Predicttime))
temp_pr_forecast.ts = merge(sri.df, sy.pr_forecast.ts, all=F)

a = lm(SRI3~SRI12+sy.pr_forecast.ts, as.data.frame(temp_pr_forecast.ts[month(temp_pr_forecast.ts)==12]))
summary(a)

temp_pr_forecast.ts = merge(dc.sri.df, dc.pr_forecast.ts, all=F)
cor.sri3_forecast = c()
cor.sri12_forecast = c()
cor.sri1_forecast = c()

for(mm in c(1:12)){
     cor.sri1_forecast[mm] = cor(temp_pr_forecast.ts[which(month(temp_pr_forecast.ts) == mm)])[3,4]
     cor.sri3_forecast[mm] = cor(temp_pr_forecast.ts[which(month(temp_pr_forecast.ts) == mm)])[1,4]
     cor.sri12_forecast[mm] = cor(temp_pr_forecast.ts[which(month(temp_pr_forecast.ts) == mm)])[2,4]
     dev.new()
     pairs(as.data.frame(temp_pr_forecast.ts[which(month(temp_pr_forecast.ts) == mm)]), main=paste0("scatter plot on ", mm, " at daecheong"))
}
dev.off()
dev.new()
plot(cor.sri3_forecast, type="o", pch=1, ylim=c(-1, 1),
     xlab="month", ylab="correlation", main="correlation b/w SRI3 obs. and APCC pr forecast at soyang")
dev.new()
plot(cor.sri12_forecast, type="o", pch=1, ylim=c(-1, 1),
     xlab="month", ylab="correlation", main="correlation b/w SRI12 obs. and APCC pr forecast at soyang")
dev.off()
dev.new()
plot(cor.sri1_forecast, type="o", pch=1, ylim=c(-1, 1),
     xlab="month", ylab="correlation", main="correlation b/w SRI1 obs. and APCC pr forecast at soyang")
dev.off()


cor(temp_pr_forecast.ts$SRI3, temp_pr_forecast.ts[,4])



pairs(as.data.frame(temp_pr_forecast.ts))
acf(temp_pr_forecast.ts)

cor(merge(diff(dc.sri.df$SRI1), dc.pr_forecast.ts, all=F))

# 

#### soil moisture(ssi) from MAFRA
smfilepath = file.path(getwd(), "observations", "soilmoisture")
sy.sm.df = read.csv(file.path(smfilepath, "sm_soyang.csv"), row.names=1, header=T) %>% xts(., ymd(rownames(.)))
dc.sm.df = read.csv(file.path(smfilepath, "sm_daecheong.csv"), row.names=1, header=T)%>% xts(., ymd(rownames(.)))
# vs SMI from APCC
temp.sm.ts = merge(sy.sm.df$ssi, sy.smi.ts, all = F) %>% setNames(., c("SSI", "SMI"))
cor(temp.sm.ts, use="complete.obs")

temp.sm.ts = merge(dc.sm.df$ssi, dc.smi.ts, all = F) %>% setNames(., c("SSI", "SMI"))
cor(temp.sm.ts, use="complete.obs")


# vs SRI
temp_ssi.ts = merge(sy.sri.df, xts(sy.sm.df$ssi, date(sy.sm.df)), all=F)
cc = ccf(as.vector(temp_ssi.ts$SRI1), as.vector(temp_ssi.ts$ssi), na.action=na.pass, main=paste0("SRI1 vs SSI at 1012"))

temp_ssi.ts =  merge(dc.sri.df, xts(dc.sm.df$ssi, date(dc.sm.df)), all=F)
cc = ccf(as.vector(temp_ssi.ts$SRI1), as.vector(temp_ssi.ts$ssi), na.action=na.pass, main=paste0("SRI1 vs SSI at 3008"))


#### groundwater(SGI) from Kwater
gwfilepath =file.path("./observations", "groundwater")
sy.sgi.df = read.csv(file.path(gwfilepath, "sgi_soyang.csv"), row.names=1, header=T) %>% xts(., ymd(rownames(.)))
dc.sgi.df = read.csv(file.path(gwfilepath, "sgi_daecheong.csv"), row.names=1, header=T) %>% xts(., ymd(rownames(.)))

## º“æÁ∞≠ 82008√·√µ∫œªÍ; 95515¿Œ¡¶≥≤∏È; 84011¿Œ¡¶; 03546√·√µøÏµŒ;
temp_sgi.df = merge(sy.sri.df, sy.sgi.df, all=F)

ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X82008), main="SRI1 vs SGI at 1012(√·√µ∫œªÍ)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X82008), main="SRI3 vs SGI at 1012(√·√µ∫œªÍ)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X82008), main="SRI12 vs SGI at 1012(√·√µ∫œªÍ)")
ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X95515), main="SRI1 vs SGI at 1012(¿Œ¡¶≥≤∏È)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X95515), main="SRI3 vs SGI at 1012(¿Œ¡¶≥≤∏È)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X95515), main="SRI12 vs SGI at 1012(¿Œ¡¶≥≤∏È)")
ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X84011), main="SRI1 vs SGI at 1012(¿Œ¡¶)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X84011), main="SRI3 vs SGI at 1012(¿Œ¡¶)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X84011), main="SRI12 vs SGI at 1012(¿Œ¡¶)")
ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X03546), main="SRI1 vs SGI at 1012(√·√µøÏµŒ)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X03546), main="SRI3 vs SGI at 1012(√·√µøÏµŒ)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X03546), main="SRI12 vs SGI at 1012(√·√µøÏµŒ)")

## ¥Î√ª¥Ô 65070ø¡√µ±∫∫œ; 09858¥Î¿¸πÆ∆Ú;
temp_sgi.df = merge(dc.sri.df, dc.sgi.df, all=F)

ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X65070), main="SRI1 vs SGI at 3008(ø¡√µ±∫∫œ)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X65070), main="SRI3 vs SGI at 3008(ø¡√µ±∫∫œ)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X65070), main="SRI12 vs SGI at 3008(ø¡√µ±∫∫œ)")
ccf(as.vector(temp_sgi.df$SRI1), as.vector(temp_sgi.df$X09858), main="SRI1 vs SGI at 3008(¥Î¿¸πÆ∆Ú)")
ccf(as.vector(temp_sgi.df$SRI3), as.vector(temp_sgi.df$X09858), main="SRI3 vs SGI at 3008(¥Î¿¸πÆ∆Ú)")
ccf(as.vector(temp_sgi.df$SRI12), as.vector(temp_sgi.df$X09858), main="SRI12 vs SGI at 3008(¥Î¿¸πÆ∆Ú)")

