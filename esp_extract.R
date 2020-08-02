## standardized index
######################
timescale = 1
indexname = "SRI"
targetbsn = 1012;
bsnname = "sumjin"
#####################
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
recperiod = data.frame(bsn=c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002), 
                       start = c(1974, 1981, 1977, 1975, 1986, 1989, 1976, 1992), end = rep(2017,length(bsnnames)))
# damlist = readxl::read_excel("dambasin_numbers.xlsx");
bsnarea = c(2703, 3204, 1584, 763, 6648, 925, 2285, 1361) %>% setNames(., recperiod$bsn)

for(bsnname in bsnnames[4:8]){

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

inputts_target %>%as.data.frame(.)%>% write.csv(., file.path("./streamflow", bsnname, paste0("obs_",bsnname,".csv")))

###################################################
# SRI ensemble prediction with 1 month lead 
esplist = list.files("./esp1month_csv")

ensbasincode = readxl::read_excel("basin_numbers.xlsx");
# ensbasincode$BSN_number

y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1
AREA = bsnarea[as.character(targetbsn)]

predicttime = data.frame(Y=year(sri.ts), M=month(sri.ts))

esp.mean = c()
# for (pt in which(as.numeric(sri.ts)<(-1))){
for (pt in c(1:(nrow(predicttime)))){
  ensname = esplist[intersect(grep("*_cms.csv$",esplist), which(substr(esplist, 5, 8)==predicttime$Y[pt]&substr(esplist, 9, 10)==sprintf("%02d", predicttime$M[pt])))]
  # ensname = paste0("ESP_", predicttime$Y[pt], sprintf("%02d", predicttime$M[pt]), "_31_cms.csv")
  
  ensdata = read.csv(file.path("esp1month_csv",ensname), header=F)
  colnames(ensdata) = c("Y", "M", "D", ensbasincode$BSN_number)
  ens_target = cbind(ensdata[,1], cms2mm(ensdata[as.character(targetbsn)], AREA)) %>% setNames(., c("Y", "x"))
  monthly_ens_target = ens_target %>% group_by(Y) %>% summarise(mean(x)) %>% setNames(., c("Y", "x"))
  monthly_ens_target = monthly_ens_target$Y %in% c(y_start:y_end) %>% monthly_ens_target[.,]
  nesp = nrow(monthly_ens_target)
  esp.mean[pt] = monthly_ens_target$x %>% mean(.)
}
esp.mean.ts = data.frame(esp.mean)
rownames(esp.mean.ts) = ymd(paste(predicttime[,1],predicttime[,2],1, sep="-"))
esp.mean.ts %>% write.csv(., file.path("./streamflow", bsnname, paste0("espmean_",bsnname,".csv")))
}
