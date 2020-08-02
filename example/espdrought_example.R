library(lubridate)
library(dplyr)
library(xts)
bsn = targetbsn

EDPdur_prob = matrix(0, nrow=length(years), ncol=12)
EDPdef_prob = matrix(0, nrow=length(years), ncol=12)
jj = 5
year = years[jj]
month = 2
recstart = y_start
recend = y_end
threshold = q_mtdl$tdl[month]

basinslist = readxl::read_excel("basin_numbers.xlsx")

esplist = list.files("./esp1month_csv")
espMMlist = esplist[intersect(grep("*_cms.csv$",esplist), which(substr(esplist, 9, 10)==sprintf("%02d", month)))]


# select esp for obs record period
tempespname = espMMlist[grep(paste0("^ESP_",year), espMMlist)]
esp = read.csv(file.path("esp1month_csv", tempespname), header=F) %>% setNames(., c("Year", "Month", "Day", unlist(basinslist[,2])))

espyears = intersect(unique(esp$Year), c(recstart:recend))
espdrought = matrix(0, nrow=length(espyears), ncol=2); rownames(espdrought) = espyears
bsnidx = which(colnames(esp)==bsn); bsnidx = bsnidx[length(bsnidx)]

esp_tb = esp[,c(1:3, bsnidx)]

for (ii in c(1:length(espyears))){
     espy = espyears[ii]
     tempesp = subset(esp_tb, esp_tb$Year == espy)
     espdrought[ii,] = cal_drought_daily(tempesp[,4], threshold)
}


tempDprob = (espdrought>c(dur_clm[MM],def_clm[MM])) %>% apply(., 2, sum)/nrow(tempespD)
EDPdur_prob[jj, month] = tempDprob[1]
EDPdef_prob[jj, month] = tempDprob[2]


MM = 4
YY = 2001
targetbsn = 1003
qrefname = paste0("q_p20_monthly_cms_",targetbsn,".csv")
y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1
q_mtdl = read.csv(file.path("references_csv", qrefname), header=F) %>% setNames(.,"tdl")

espD_result = cal_espdrought_monthly(targetbsn, YY, MM, q_mtdl$tdl[MM], y_start, y_end)

