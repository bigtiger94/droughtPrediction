## standardized index
######################
timescale = 3
indexname = "SRI"
targetbsn = 2002;
bsnname = "imha"
#####################
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
recperiod = data.frame(bsn=c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002), 
                       start = c(1974, 1981, 1977, 1975, 1986, 1989, 1976, 1992), end = rep(2017,length(bsnnames)))
# damlist = readxl::read_excel("dambasin_numbers.xlsx");
bsnarea = c(2703, 3204, 1584, 763, 6648, 925, 2285, 1361) %>% setNames(., recperiod$bsn)



y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1


AREA = bsnarea[as.character(targetbsn)]
subsetyear.ts <- function(data, start, end) {
     ks <- which(year(data) >= start & year(data) < end)
     output <- data[ks]
}
loadfname = "Obs_inflow_monthly_113basins.csv"

inputdata = read.csv(file.path("observations", loadfname), header=T); colnames(inputdata)[3:ncol(inputdata)] = substr(colnames(inputdata)[3:ncol(inputdata)], 2, 7)
date.dd = paste(inputdata$Y, inputdata$M, "01", sep = "-") %>% as.Date(.)
inputts_target = inputdata[,as.character(targetbsn)] %>% xts(., date.dd) %>% subsetyear.ts(., y_start, y_end)
# head(inputts_target)

if (timescale>12){
     startidx = 24
} else{
     startidx = 12
}


temp_input.cum = c()
for(ii in c((startidx+1):length(inputts_target))) {temp_input.cum[ii-startidx] = sum(inputts_target[(ii-timescale+1):ii])}
input.cum = na.omit(matrix(temp_input.cum, ncol=12, byrow = T))
# empprob = (c(1:length(inflow.mm.cum[,1]))-0.44)/(length(inflow.mm.cum[,1])+0.12)
# plot(empprob, sort(inflow.mm.cum[,1]))

## fit diagnosis
gamaic = 0
lnormaic = 0
for (mm in 1:12){
     # pathname = file.path(getwd(), "SDIfit", paste0(indexname,"fit"))
     # if(!dir.exists(pathname)) {dir.create(paste0("SDIfit/", indexname,"fit"))}
     # pdf(file.path(pathname,paste0(indexname,"-",timescale,"on",ii,".pdf")), width=5, height=5)
     empprob = (c(1:length(input.cum[,mm]))-0.44)/(length(input.cum[,12])+0.12)
     plot(empprob, sort(input.cum[,mm]), xlab="prob", ylab="runoff[mm]")
     
     temp.gamfit = fitdist(input.cum[,mm], 'gamma')
     lines(seq(0, 1, 0.01), qgamma(seq(0, 1, 0.01), shape=temp.gamfit$estimate[1], rate=temp.gamfit$estimate[2]), col="red")
     
     temp.lnormfit = fitdist(input.cum[,mm], "lnorm")
     lines(seq(0, 1, 0.01), qlnorm(seq(0, 1, 0.01), meanlog=temp.lnormfit$estimate[1], sdlog=temp.lnormfit$estimate[2]), col="blue")
     legend("topleft", legend = c("obs", "gamma", "lnorm"), pch=c(1,-1,-1), lty=c(-1,1,1), col=c("black", "red", "blue"))
     gamaic = gamaic + temp.gamfit$aic
     lnormaic = lnormaic + temp.lnormfit$aic
     # dev.off()
}
if (gamaic>lnormaic){
     print("lnorm")
     fitfunc = "lnorm"
     
} else{
     print("gamma")
     fitfunc = "gamma"
     
}
evalname = paste0("p",fitfunc, "(input.cum[,mm], temp.distfit$estimate[1], temp.distfit$estimate[2])")

sri = c()
distparam.sri = data.frame()
for (mm in 1:12){
     temp.distfit = fitdist(input.cum[,mm], fitfunc)
     if (mm ==1){
          distparam.sri = temp.distfit$estimate
     } else{
          distparam.sri = rbind(distparam.sri, temp.distfit$estimate)
     }
     # temp.prob = plnorm(input.cum[,mm], temp.distfit$estimate[1], temp.distfit$estimate[2])
     temp.fitprob = eval(parse(text=evalname))
     print(mm)
     print(chisq.test(input.cum[,mm], temp.fitprob))
     temp.sri = sdi.transform(temp.fitprob)
     sri = cbind(sri, temp.sri)
}

distparam.sri = as.data.frame(distparam.sri, row.names = c(1:12))
colnames(sri) = as.character(c(1:12)); row.names(sri) = unique(year(date(inputts_target[(startidx+1):length(inputts_target)])))

sri.ts = xts(as.vector(t(sri)), date(inputts_target[(startidx+1):length(inputts_target)]))
sri3.ts = sri.ts
# write.csv(distparam.sri, file.path("./ensSRI", paste0("param_sri",timescale,"_",bsnname,".csv")))
# 
# mergedsri = data.frame(SRI3=sri3.ts, SRI12=sri12.ts, SRI1=sri1.ts)
# write.csv(mergedsri, file.path("./observations", paste0("sri_",bsnname,".csv")), row.names=T)
# dc.sri12.ts = xts(as.vector(t(sri)), date(inputts_target[(startidx+1):length(inputts_target)]))
# plot(dc.sri12.ts)
# 
# dc.sri.df = data.frame(dc.sri3.ts, dc.sri12.ts) %>% setNames(., c("SRI3", "SRI12"))
# write.csv(dc.sri.df, file.path("./observations", "daecheong_sri.csv"))
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


