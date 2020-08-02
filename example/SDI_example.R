## standardized index


recperiod = data.frame(bsn=c(1012, 3008), start = c(1974, 1981), end = rep(2017,2))
damlist = readxl::read_excel("dambasin_numbers.xlsx");
bsnarea = c(2703, 3204) %>% setNames(., c("1012", "3008"))
targetbsn = 1012;

y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1
AREA = bsnarea[as.character(targetbsn)]
subsetyear.ts <- function(data, start, end) {
  ks <- which(year(data) >= start & year(data) < end)
  output <- data[ks]
}

inflowdata = read.csv("./observations/Obs_inflow_35dam.csv", header=F) %>% setNames(., c("Year", "Month", "Day", unlist(damlist[,2])))
date.dd = paste(inflowdata$Year, inflowdata$Month, inflowdata$Day, sep = "-") %>% as.Date()
inflowts_target = inflowdata[,as.character(targetbsn)] %>% xts(., date.dd) %>% subsetyear.ts(., y_start, y_end)
# head(inflowts_target)
inflowts_target.mm = cms2mm(inflowts_target, AREA)
monthly_inflowts.mm = apply.monthly(inflowts_target.mm,mean)

timescale = 6
temp_inflow.mm.cum = c()
for(ii in c(13:length(monthly_inflowts.mm))) {temp_inflow.mm.cum[ii-12] = sum(monthly_inflowts.mm[(ii-timescale+1):ii])}
inflow.mm.cum = matrix(temp_inflow.mm.cum, ncol=12, byrow = T)
# empprob = (c(1:length(inflow.mm.cum[,1]))-0.44)/(length(inflow.mm.cum[,1])+0.12)
# plot(empprob, sort(inflow.mm.cum[,1]))

sri = c()
for (ii in c(1:12)){
  temp.gamfit = fitdist(inflow.mm.cum[,ii], 'gamma')
  # lines(seq(0, 1, 0.01), qgamma(seq(0, 1, 0.01), shape=temp.gamfit$estimate[1], rate=temp.gamfit$estimate[2]))
  # inflow.mm.cum[1,1]
  
  temp.prob = pgamma(inflow.mm.cum[,ii], shape=temp.gamfit$estimate[1], rate=temp.gamfit$estimate[2])
  temp.sri = sdi.transform(temp.prob)
  # temp.t = c()
  # temp.t[which(temp.prob<=0.5)] = sqrt(log(1/(temp.prob[which(temp.prob<=0.5)])^2))
  # temp.t[which(temp.prob>0.5)] = sqrt(log(1/(1-temp.prob[which(temp.prob>0.5)])^2))
  # 
  # temp.sri = c()
  # tempidx = which(temp.prob<=0.5)
  # temp.sri[tempidx] = -(temp.t[tempidx]-(c0+c1*temp.t[tempidx]+c2*temp.t[tempidx]^2) / (1+d1*temp.t[tempidx]+d2*temp.t[tempidx]^2+d3*temp.t[tempidx]^3))
  # tempidx = which(temp.prob>0.5)
  # temp.sri[tempidx] = (temp.t[tempidx]-(c0+c1*temp.t[tempidx]+c2*temp.t[tempidx]^2) / (1+d1*temp.t[tempidx]+d2*temp.t[tempidx]^2+d3*temp.t[tempidx]^3))
  sri = cbind(sri, temp.sri)
}
colnames(sri) = as.character(c(1:12))
# plot(as.vector(t(sri)), type="l")

indexname = "SRI"
## fit diagnosis
gamaic = 0
lnormaic = 0
for (ii in 1:12){
  # pathname = file.path(getwd(), "SDIfit", paste0(indexname,"fit"))
  # if(!dir.exists(pathname)) {dir.create(paste0("SDIfit/", indexname,"fit"))}
  # pdf(file.path(pathname,paste0(indexname,"-",timescale,"on",ii,".pdf")), width=5, height=5)
  empprob = (c(1:length(inflow.mm.cum[,ii]))-0.44)/(length(inflow.mm.cum[,12])+0.12)
  plot(empprob, sort(inflow.mm.cum[,ii]), xlab="prob", ylab="runoff[mm]")
  
  temp.gamfit = fitdist(inflow.mm.cum[,ii], 'gamma')
  lines(seq(0, 1, 0.01), qgamma(seq(0, 1, 0.01), shape=temp.gamfit$estimate[1], rate=temp.gamfit$estimate[2]), col="red")
  
  temp.lnormfit = fitdist(inflow.mm.cum[,ii], "lnorm")
  lines(seq(0, 1, 0.01), qlnorm(seq(0, 1, 0.01), meanlog=temp.lnormfit$estimate[1], sdlog=temp.lnormfit$estimate[2]), col="blue")
  legend("topleft", legend = c("obs", "gamma", "lnorm"), pch=c(1,-1,-1), lty=c(-1,1,1), col=c("black", "red", "blue"))
  gamaic = gamaic + temp.gamfit$aic
  lnormaic = lnormaic + temp.lnormfit$aic
  # dev.off()
}
if (gamaic>lnormaic){
  print("lnorm")
} else{
  print("gamma")
}

meanlog.sri6 = rep(0, 12)
sdlog.sri6 = rep(0, 12)

lnormparam.sri6 = data.frame()
for (ii in 1:12){
  temp.lnormfit = fitdist(inflow.mm.cum[,ii], "lnorm")
  if (ii ==1){
    lnormparam.sri6 = temp.lnormfit$estimate
  } else{
    lnormparam.sri6 = rbind(lnormparam.sri6, temp.lnormfit$estimate)
  }
}
lnormparam.sri6 = as.data.frame(lnormparam.sri6, row.names = c(1:12))


# 
# sri.param = fitSCI(monthly_inflowts.mm, first.mon=1, distr="gamma", time.scale=6, p0=TRUE)
# sri.func = transformSCI(monthly_inflowts.mm, first.mon=1, obj=sri.param)
# sri2.param = fitSCI(monthly_inflowts.mm, first.mon=1, distr="pe3", time.scale=6, p0=TRUE)
# sri2 = transformSCI(monthly_inflowts.mm, first.mon=1, obj=sri2.param)
# sri3.param = fitSCI(monthly_inflowts.mm, first.mon=1, distr="lnorm", time.scale=6, p0=TRUE)
# sri3 = transformSCI(monthly_inflowts.mm, first.mon=1, obj=sri3.param) 
# 
# plot(date(monthly_inflowts.mm), sri, type="l")
# lines(date(monthly_inflowts.mm), sri2, col="red")
# lines(date(monthly_inflowts.mm), sri3, col="blue")
# a1 = sri.param$dist.para[,7]
# a2 = sri2.param$dist.para[,7]
# a3 = sri3.param$dist.para[,7]


