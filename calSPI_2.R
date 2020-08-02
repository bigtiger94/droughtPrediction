## standardized index
######################
timescale = 1
indexname = "SPI"
targetbsn = 320305;
#####################
# setwd("C:\\Users\\Daeho\\Desktop\\Research\\연구자료\\EDP")
recperiod = data.frame(bsn=c(1012, 3008, 320305), start = c(1974, 1981, 1998), end = rep(2017,3))
# damlist = readxl::read_excel("dambasin_numbers.xlsx");
bsnarea = c(2703, 3204, 163.6) %>% setNames(., c("1012", "3008", "320305"))



y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1
AREA = bsnarea[as.character(targetbsn)]
subsetyear.ts <- function(data, start, end) {
  ks <- which(year(data) >= start & year(data) < end)
  output <- data[ks]
}
loadfname = "Obs_PRCP_monthly.csv"

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
  # pdf(file.path(pathname,paste0(indexname,"-",timescale,"on",mm,".pdf")), width=5, height=5)
  empprob = (c(1:length(input.cum[,mm]))-0.44)/(length(input.cum[,12])+0.12)
  plot(empprob, sort(input.cum[,mm]), xlab="prob", ylab="prcp[mm]")
  
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

spi = c()
distparam.spi = data.frame()
for (mm in 1:12){
  temp.distfit = fitdist(input.cum[,mm], fitfunc)
  if (mm ==1){
    distparam.spi = temp.distfit$estimate
  } else{
    distparam.spi = rbind(distparam.spi, temp.distfit$estimate)
  }
  # temp.prob = plnorm(input.cum[,mm], temp.distfit$estimate[1], temp.distfit$estimate[2])
  temp.prob = eval(parse(text=evalname))
  temp.spi = sdi.transform(temp.prob)
  spi = cbind(spi, temp.spi)
}

distparam.spi = as.data.frame(distparam.spi, row.names = c(1:12))
colnames(spi) = as.character(c(1:12))



spi = xts(as.vector(t(spi)), date(inputts_target[(startidx+1):length(inputts_target)]))
plot(spi)

write.csv(spi, paste0("boryungdam_spi",timescale,".csv"))
