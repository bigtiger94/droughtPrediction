dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
ensfilepath = file.path("./ensSRI")
rawdate = read.csv(file.path(ensfilepath, "rawensdate.csv"), row.names=1)
apccdate = read.csv(file.path(ensfilepath, "apccupensdate.csv"), row.names=1)

rawdate = read.csv(file.path(ensfilepath, "rawensdate.csv"), row.names=1)
update = read.csv(file.path(ensfilepath, "k1", "upensdate.csv"), row.names=1)
EDPnames = c("EDP", "EDP+S")

bsncase = 1
for (bsncase in c(1:length(bsncodes))){
     targetbsn = bsncodes[bsncase];
     bsnname = bsnnames[bsncase];
     
     MBEtable = c()
     
     obs = read.csv(file.path("observations", paste0("sri_",bsnname,".csv")),row.names=1) %>%
          dplyr::select(toupper(dindexname)) %>% xts(., ymd(rownames(.)))
     
     rawEDP = read.csv(file.path(ensfilepath, paste0("meansd_rawens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(rawdate$x))

     MBE_raw = mean(year(rawEDP) %in% year(update$x) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)
     
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          
          MBE_up[kk] = mean(upEDP$MEAN-obs)
          
     }
     
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_all_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.irr %>% obs[.]
     temp_rawEDP = month(rawEDP) %in% period.irr %>% rawEDP[.]
     MBE_raw = mean(year(rawEDP) %in% year(update$x) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)
     
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          temp_upEDP = month(upEDP) %in% period.irr %>% upEDP[.]
          MBE_up[kk] = mean(upEDP$MEAN-obs)
     }
     MBEtable = c()
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_irrigation_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.nonirr %>% obs[.]
     temp_rawEDP = month(rawEDP) %in% period.nonirr %>% rawEDP[.]
     MBE_raw = mean(year(rawEDP) %in% year(update$x) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)
     
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          temp_upEDP = month(upEDP) %in% period.nonirr %>% upEDP[.]
          MBE_up[kk] = mean(upEDP$MEAN-obs)   
     }
     MBEtable = c()
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_non-irrigation_",dindexname,"_",bsnname,".csv")))
     
     
}