## modifying accuracy table 
library(writexl)
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)


bsnname = bsnnames[1]

for(bsnname in bsnnames){
     periods = c("all", "irrigation", "non-irrigation")
     for (period in periods){
          accfname = paste0("accuracytable_summarised_",period,"_",dindexname,"_",bsnname,".csv")
          
          acctable = read.csv(file.path("./predictResult", accfname), row.names=1)
          DPidx = acctable$GROUP %>% grep("det$", .)
          PPidx = acctable$GROUP %>% grep("prob$", .)
          acctable$Type[PPidx] = "PP"
          acctable$Type[DPidx] = "DP"
          rawidx = acctable$GROUP %>% grep("^raw", .)
          upidx = acctable$GROUP %>% grep("^up", .)
          acctable$Case[upidx] = "EDP+S"
          acctable$Case[rawidx] = "EDP"
          acctable = acctable %>% arrange(D, Type, Case)
          
          attach(acctable)
          newacctable = data.frame(Phase=D, Type=Type, Case=Case, acctable[,3:7])
          detach(acctable)
          newaccpath = file.path("./predictResult", "summary")
          # write_xlsx(newacctable, file.path(newaccpath, paste0("acctable_",period,"_",dindexname,"_",bsnname,".xlsx")), col_names = T)
          
          
          
          # accfname = paste0("accuracytable_apcc_summarised_",period,"_",dindexname,"_",bsnname,".csv")
          # 
          # acctable = read.csv(file.path("./predictResult", accfname), row.names=1)
          # DPidx = acctable$GROUP %>% grep("det$", .)
          # PPidx = acctable$GROUP %>% grep("prob$", .)
          # acctable$Type[PPidx] = "PP"
          # acctable$Type[DPidx] = "DP"
          # rawidx = acctable$GROUP %>% grep("^raw", .)
          # upidx = acctable$GROUP %>% grep("^up", .)
          # apccrawidx = acctable$GROUP %>% grep("^apccraw", .)
          # apccupidx = acctable$GROUP %>% grep("^apccup", .)
          # acctable$Case[upidx] = "EDP+"
          # acctable$Case[rawidx] = "EDP"
          # acctable$Case[apccupidx] = "EDP+AS"
          # acctable$Case[apccrawidx] = "EDP+A"
          # 
          # acctable = acctable %>% arrange(D, Type, Case) %>% dplyr::filter(Type=="PP")
          # 
          # attach(acctable)
          # newacctable = data.frame(Phase=D, Type=Type, Case=Case, acctable[,3:7])
          # detach(acctable)
          # newaccpath = file.path("./predictResult", "summary")
          # write_xlsx(newacctable, file.path(newaccpath, paste0("acctable_apcc_",period,"_",dindexname,"_",bsnname,".xlsx")), col_names = T)
     }
}
