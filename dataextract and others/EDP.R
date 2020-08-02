# rm(list=ls())

# setwd("C:/Users/Daeho/Desktop/Research/연구자료/EDP")
# setwd("..")


### setup
recperiod = data.frame(bsn=c(1003, 1012, 3008, 320305), start = c(1985, 1974, 1981, 1998), end = rep(2017,4))
damlist = readxl::read_excel("dambasin_numbers.xlsx");
targetbsn = 1012;

y_start = recperiod$start[recperiod$bsn==as.character(targetbsn)]
y_end = recperiod$end[recperiod$bsn==as.character(targetbsn)]+1

subsetyear.ts <- function(data, start, end) {
     ks <- which(year(data) >= start & year(data) < end)
     output <- data[ks]
}

inflowdata = read.csv("./observations/Obs_inflow_35dam.csv", header=F) %>% setNames(., c("Year", "Month", "Day", unlist(damlist[,2])))
date = paste(inflowdata$Year, inflowdata$Month, inflowdata$Day, sep = "-") %>% as.Date()
inflowts_target = inflowdata[,as.character(targetbsn)] %>% xts(., date) %>% subsetyear.ts(., y_start, y_end)
# head(inflowts_target)

### drought calculation
qrefname = paste0("q_p20_monthly_cms_",targetbsn,".csv")
q_mtdl = read.csv(file.path("references_csv", qrefname), header=F) %>% setNames(.,"tdl")
# 3008
# q_mtdl$tdl[c(1:4,10:12)] = 41
# q_mtdl$tdl[5:9] = 41+26
# 1012
q_mtdl$tdl[c(1:4,10:12)] = 38
q_mtdl$tdl[5:9] = 38+1

years = unique(year(inflowts_target))
nY = length(unique(year(inflowts_target)))

def = matrix(0, ncol=12, nrow=nY); rownames(def) = years; colnames(def) = c(1:12)
dur = matrix(0, ncol=12, nrow=nY); rownames(dur) = years; colnames(dur) = c(1:12)

ydx = 0
for (ydx in c(1:length(years))){
     YY = years[ydx]
     for(MM in c(1:12)){
          tempdrought = subset(inflowts_target, year(inflowts_target)==YY&month(inflowts_target)==MM) %>% cal_drought_daily(., q_mtdl$tdl[MM])
          dur[ydx,MM] = tempdrought[1]; def[ydx,MM] = tempdrought[2]
          }
}
dur = as.data.frame(dur); def = as.data.frame(def)
intensity = def/dur; intensity[is.na(intensity)] = 0

### get climatological drought deficit & duration
if(years[nY]>2017) {
     refyears = years
} else if(nY>30) {
     refyears = c(1981:2010)
} else{
     refyears = years
}

dur_ref = apply(dur[as.character(refyears),],2,mean)
def_ref = apply(def[as.character(refyears),],2,mean)
int_ref = apply(intensity[as.character(refyears),],2,mean)


#### ESP drought

EDPdur_prob = matrix(0, nrow=length(years), ncol=12); rownames(EDPdur_prob) = years; colnames(EDPdur_prob) = 1:12;
EDPdef_prob = matrix(0, nrow=length(years), ncol=12); rownames(EDPdef_prob) = years; colnames(EDPdef_prob) = 1:12;
EDPint_prob = matrix(0, nrow=length(years), ncol=12); rownames(EDPdef_prob) = years; colnames(EDPint_prob) = 1:12;

for (ydx in c(1:length(years))){
  YY = years[ydx]
  tempEDPint = c()
  for (MM in c(1:12)){
    tempED = cal_espdrought_monthly(targetbsn, YY, MM, q_mtdl$tdl[MM], y_start, y_end)
    tempINT = tempED[,2]/tempED[,1]; tempINT[is.na(tempINT)] = 0
    tempEDPint = cbind(tempEDPint, tempINT)
    tempprobD = (tempED>c(dur_ref[MM],def_ref[MM])) %>% apply(., 2, sum)/nrow(tempED)
    tempprobINT = (tempINT>int_ref[MM]) %>% sum(.)/length(tempINT)
    EDPdur_prob[ydx, MM] = tempprobD[1]
    EDPdef_prob[ydx, MM] = tempprobD[2]
    EDPint_prob[ydx, MM] = tempprobINT
  }
  colnames(tempEDPint) = c(1:12)
  EDPintfilename = paste0("EDPint",targetbsn,"_",YY,".csv")
  write.csv(tempEDPint, file.path("EDPresult", "intensity", targetbsn, EDPintfilename))
}

EDPdef_prob = as.data.frame(EDPdef_prob); EDPdur_prob = as.data.frame(EDPdur_prob);  EDPint_prob = as.data.frame(EDPint_prob)

EDPdurfilename = paste0("EDPdur",targetbsn,".csv")
EDPdeffilename = paste0("EDPdef",targetbsn,".csv")
EDPintfilename = paste0("EDPint",targetbsn,".csv")
# 
# # 
# write.csv(EDPdur_prob, file.path("EDPresult", EDPdurfilename), row.names=T)
# write.csv(EDPdef_prob, file.path("EDPresult", EDPdeffilename), row.names=T)
# write.csv(EDPint_prob, file.path("EDPresult", EDPintfilename), row.names=T)


# EDPdur_prob = read.csv(file.path("EDPresult", EDPdurfilename), row.names=1)
# EDPdef_prob = read.csv(file.path("EDPresult", EDPdeffilename), row.names=1)
# EDPint_prob = read.csv(file.path("EDPresult", EDPintfilename), row.names=1)

## cal BS
dur_ocr = dur>dur_ref
def_ocr = def>def_ref
int_ocr = intensity>int_ref

durBS_monthly = as.vector((dur_ocr - EDPdur_prob)^2 %>% apply(., 2, sum)/length(years)) %>% setNames(., 1:12)
defBS_monthly = as.vector((def_ocr - EDPdef_prob)^2 %>% apply(., 2, sum)/length(years)) %>% setNames(., 1:12)
intBS_monthly = as.vector((int_ocr - EDPdef_prob)^2 %>% apply(., 2, sum)/length(years)) %>% setNames(., 1:12)
durBS_monthly
defBS_monthly
intBS_monthly


### ROC curve

pr <- prediction(as.vector(t(as.matrix(EDPint_prob))), as.vector(t(int_ocr)))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf@x.values
prf@y.values
prf@alpha.values
win.graph(); plot(prf, main='ROC of Test Data')
auc <- performance(pr, measure="auc")
auc@y.values[1]
