# RPSS
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = ""
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
bsncase = 1
for (bsncase in c(1:length(bsncodes))){
  bsnname = bsnnames[bsncase]
  RPS.clm.ts = read.csv(file.path("./predictResult",paste0("RPS_clm_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
    xts(., ymd(rownames(.)))
  tempRPS.clm = year(RPS.clm.ts) %in% c(2008:2017) %>% RPS.clm.ts[.]
  RPStable = read.csv(file.path("./predictResult", paste0("RPS_", dindexname, "_", bsnname, ".csv")), row.names=1)
  RPSavg.clm =  mean(tempRPS.clm)
  
  RPSirr.clm = month(tempRPS.clm) %in% period.irr %>% tempRPS.clm[.] %>% mean(.)
  RPSnonirr.clm = month(tempRPS.clm) %in% period.nonirr %>% tempRPS.clm[.] %>% mean(.)
  
  RPSStable = data.frame(GROUP=c("EDP DP", "EDP PP", "EDP+S DP", "EDP+S PP"))
  RPSStable$all = 1-RPStable$all/RPSavg.clm
  RPSStable$irrigation = 1-RPStable$irrigation/RPSirr.clm
  RPSStable$non.irrigation = 1-RPStable$non.irrigation/RPSnonirr.clm
  write.csv(RPSStable, file.path("./predictResult", paste0("RPSS_", dindexname,"_",bsnname,".csv")),row.names=F)
}

# RPSS

dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = "apcc"
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
bsncase = 1


for (bsncase in c(1:length(bsncodes))){
     bsnname = bsnnames[bsncase]
     RPS.clm.ts = read.csv(file.path("./predictResult",paste0("RPS_clm_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(rownames(.)))
     tempRPS.clm = year(RPS.clm.ts) %in% c(2008:2017) %>% RPS.clm.ts[.]
     RPStable = read.csv(file.path("./predictResult", paste0("RPS_apcc_", dindexname, "_", bsnname, ".csv")), row.names=1)
     RPSavg.clm =  mean(tempRPS.clm)
     
     RPSirr.clm = month(tempRPS.clm) %in% period.irr %>% tempRPS.clm[.] %>% mean(.)
     RPSnonirr.clm = month(tempRPS.clm) %in% period.nonirr %>% tempRPS.clm[.] %>% mean(.)
     
     RPSStable = data.frame(GROUP=RPStable$GROUP)
     RPSStable$all = 1-RPStable$all/RPSavg.clm
     RPSStable$irrigation = 1-RPStable$irrigation/RPSirr.clm
     RPSStable$non.irrigation = 1-RPStable$non.irrigation/RPSnonirr.clm
     
     write.csv(RPSStable, file.path("./predictResult", paste0("RPSS_apcc_", dindexname,"_",bsnname,".csv")),row.names=F)
}


