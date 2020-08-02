

AETdata = read.csv("./observations/Obs_AET.csv", header=T) 
colnames(AETdata)[4:ncol(AETdata)] = colnames(AETdata)[4:ncol(AETdata)] %>% substr(., 2, 7)

for (ii in c(4:ncol(AETdata))){
  date.dd = paste(AETdata$Y, AETdata$M, AETdata$D, sep = "-") %>% as.Date(.)
  AETts_target = AETdata[,ii] %>% xts(., date.dd) 
  # head(inflowts_target)
  monthly_AET = apply.monthly(AETts_target, sum)
  if (ii == 4){
    AET.monthly = data.frame(Y = year(monthly_AET), M=month(monthly_AET))
  }
  AET.monthly = cbind(AET.monthly, as.vector(monthly_AET))
  
}
colnames(AET.monthly)[3:ncol(AET.monthly)] = colnames(AETdata)[4:ncol(AETdata)]
head(AET.monthly)
tail(AET.monthly)

write.csv(AET.monthly, file.path("observations", "Obs_AET_monthly.csv"))
