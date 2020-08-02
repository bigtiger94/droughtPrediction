dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = "apcc"
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
for (bsncase in c(7:length(bsnnames))){
     targetbsn = bsncodes[bsncase];
     bsnname = bsnnames[bsncase];
     
     kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)

     
     BSfilepath = file.path("./predictResult","BSresult")
     BS.df = read.csv(file.path(BSfilepath, paste0("BS_apcc_allperiod_",dindexname,"_",bsnname,".csv")), row.names=1)
     BS.df$RPS = NA
     enstypes = c("raw", "up", "apccraw", "apccup")
     
     for (kk in c(1:4)){
          
          for (enstype in enstypes){
               
               kfold = paste0("k", kk)
               predictfilepath = file.path("./predictResult", kfold)
               RPSfilepath = file.path(predictfilepath, "RPSresult") 
               
               apccpredictdate = read.csv(file.path(predictfilepath, "apccdate.csv"), row.names=1)
               rps.ts = read.csv(file.path(RPSfilepath,paste0("RPS_prob_",enstype,"_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
                    xts(., ymd(rownames(.)))
               
               rps.ts = date(rps.ts) %in% ymd(apccpredictdate$x) %>% rps.ts[.]
               
               attach(BS.df)
               period = "all"
               BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = mean(rps.ts)
               
               period = "irrigation"
               RPS.irr = month(rps.ts) %in% period.irr %>% rps.ts[.] %>% mean(.)
               BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = RPS.irr
               period = "non-irrigation"
               RPS.nonirr = month(rps.ts) %in% period.nonirr %>% rps.ts[.] %>% mean(.)
               BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = RPS.nonirr
               detach()
          }
     }
     
     accfilepath = file.path("./predictResult")
     acc.df = BS.df
     acc.df %>% write.csv(., file.path(accfilepath, paste0("accuracytable_",apcctype,"_",dindexname,"_",bsnname,".csv")))
}