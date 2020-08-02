drought.occr = read.csv(file.path("SRIresult", targetbsn, "droughtoccurrence.csv"))

drought.occr.ts = xts(drought.occr$drought.obs, as.Date(drought.occr$time))

drrecordyear = 2000
temp_drought.occr.ts = drought.occr.ts %>% subset(., year(.)>=drrecordyear)
temp_sri.ts = sri.ts %>% subset(., year(.)>=drrecordyear)
plot(temp_sri.ts, main=paste0("SRI", timescale))
points(temp_drought.occr.ts[temp_drought.occr.ts==1]-1, col="red")
# lines(temp_drought.occr.ts-2, col="red")
