dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
bsncase = 1
for (bsncase in c(1:length(bsncodes))){
  targetbsn = bsncodes[bsncase];
  bsnname = bsnnames[bsncase];
  
  kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
  kk=1
  
  BSfilepath = file.path("./predictResult","BSresult")
  BS.df = read.csv(file.path(BSfilepath, paste0("BS_allperiod_",dindexname,"_",bsnname,".csv")), row.names=1)
  BS.df$RPS = NA
  
  enstype = "raw"
  for (enstype in c("raw", "up")){
    for (kk in c(1:4)){
      kfold = paste0("k", kk)
      predictfilepath = file.path("./predictResult", kfold)
      RPSfilepath = file.path(predictfilepath, "RPSresult") 
      
      rpsprob.ts = read.csv(file.path(RPSfilepath,paste0("RPS_prob_",enstype,"_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
        xts(., ymd(rownames(.)))
      rpsdet.ts = read.csv(file.path(RPSfilepath,paste0("RPS_det_",enstype,"_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
        xts(., ymd(rownames(.)))
      
      attach(BS.df)
      period = "all"
      BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = mean(rpsprob.ts)
      BS.df$RPS[which(Predicttype=="det"&K==kfold&Enstype==enstype&Period==period)] = mean(rpsdet.ts)
      period = "irrigation"
      RPS.irr = month(rpsprob.ts) %in% period.irr %>% rpsprob.ts[.] %>% mean(.)
      BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = RPS.irr
      RPS.irr = month(rpsdet.ts) %in% period.irr %>% rpsdet.ts[.] %>% mean(.)
      BS.df$RPS[which(Predicttype=="det"&K==kfold&Enstype==enstype&Period==period)] = RPS.irr
      period = "non-irrigation"
      RPS.nonirr = month(rpsprob.ts) %in% period.nonirr %>% rpsprob.ts[.] %>% mean(.)
      BS.df$RPS[which(Predicttype=="prob"&K==kfold&Enstype==enstype&Period==period)] = RPS.nonirr
      RPS.nonirr = month(rpsdet.ts) %in% period.nonirr %>% rpsdet.ts[.] %>% mean(.)
      BS.df$RPS[which(Predicttype=="det"&K==kfold&Enstype==enstype&Period==period)] = RPS.nonirr
      detach()
    }
  }
  
  accfilepath = file.path("./predictResult")
  acc.df = BS.df
  acc.df %>% write.csv(., file.path(accfilepath, paste0("accuracytable_",dindexname,"_",bsnname,".csv")))
}