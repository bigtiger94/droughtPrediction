dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
ensfilepath = file.path("./ensSRI")
rawdate = read.csv(file.path(ensfilepath, "rawensdate.csv"), row.names=1)
apccdate = read.csv(file.path(ensfilepath, "apccupensdate.csv"), row.names=1)
EDPnames = c("EDP", "EDP+S", "EDP+A", "EDP+AS")

bsncase = 1
for (bsncase in c(1:length(bsncodes))){
     targetbsn = bsncodes[bsncase];
     bsnname = bsnnames[bsncase];
     
     MBEtable = c()
     
     obs = read.csv(file.path("observations", paste0("sri_",bsnname,".csv")),row.names=1) %>%
          dplyr::select(toupper(dindexname)) %>% xts(., ymd(rownames(.)))
     
     rawEDP = read.csv(file.path(ensfilepath, paste0("meansd_rawens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(rawdate$x))
     rawapccEDP = read.csv(file.path(ensfilepath, paste0("meansd_apccrawens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(apccdate$x))
     
     MBE_rawapcc = mean(obs - rawapccEDP$MEAN) %>% rep(.,4)
     MBE_raw = mean(year(rawEDP) %in% year(rawapccEDP) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)

     MBE_upapcc = c()
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          update = read.csv(file.path(ensfilepath, kfold, "upensdate.csv"), row.names=1)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(apccdate$x))
          MBE_upapcc[kk] = mean(obs-upapccEDP$MEAN)
          MBE_up[kk] = mean(year(upEDP) %in% year(upapccEDP) %>% upEDP$MEAN[.]-obs)

     }
     
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up, MBE_rawapcc, MBE_upapcc) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_apcc_all_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.irr %>% obs[.]
     temp_rawapccEDP = month(rawapccEDP) %in% period.irr %>% rawapccEDP[.]
     MBE_rawapcc = mean(temp_obs - temp_rawapccEDP$MEAN) %>% rep(.,4)
     temp_rawEDP = month(rawEDP) %in% period.irr %>% rawEDP[.]
     MBE_raw = mean(year(rawEDP) %in% year(rawapccEDP) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)
     
     MBE_upapcc = c()
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(apccdate$x))
          temp_upapccEDP = month(upapccEDP) %in% period.irr %>% upapccEDP[.]
          MBE_upapcc[kk] = mean(temp_obs-temp_upapccEDP$MEAN)
          temp_upEDP = month(upEDP) %in% period.irr %>% upEDP[.]
          MBE_up[kk] = mean(year(upEDP) %in% year(upapccEDP) %>% upEDP$MEAN[.]-obs)
     }
     MBEtable = c()
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up, MBE_rawapcc, MBE_upapcc) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_apcc_irrigation_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.nonirr %>% obs[.]
     temp_rawapccEDP = month(rawapccEDP) %in% period.nonirr %>% rawapccEDP[.]
     MBE_rawapcc = mean(temp_obs - temp_rawapccEDP$MEAN) %>% rep(.,4)
     temp_rawEDP = month(rawEDP) %in% period.nonirr %>% rawEDP[.]
     MBE_raw = mean(year(rawEDP) %in% year(rawapccEDP) %>% rawEDP$MEAN[.]-obs) %>% rep(.,4)
     
     MBE_upapcc = c()
     MBE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(apccdate$x))
          temp_upapccEDP = month(upapccEDP) %in% period.nonirr %>% upapccEDP[.]
          MBE_upapcc[kk] = mean(temp_obs-temp_upapccEDP$MEAN)
          temp_upEDP = month(upEDP) %in% period.nonirr %>% upEDP[.]
          MBE_up[kk] = mean(year(upEDP) %in% year(upapccEDP) %>% upEDP$MEAN[.]-obs)   
     }
     MBEtable = c()
     MBEtable = rbind(MBEtable, MBE_raw, MBE_up, MBE_rawapcc, MBE_upapcc) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     MBEtable$AVG = apply(MBEtable, 1, mean)
     rownames(MBEtable) = EDPnames
     write.csv(MBEtable, file.path("./predictResult", paste0("MBE_apcc_non-irrigation_",dindexname,"_",bsnname,".csv")))
     
     
}