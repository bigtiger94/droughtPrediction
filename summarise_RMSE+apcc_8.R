dindexname = "sri3"
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
     
     RMSEtable = c()
     
     obs = read.csv(file.path("observations", paste0("sri_",bsnname,".csv")),row.names=1) %>%
          dplyr::select(toupper(dindexname)) %>% xts(., ymd(rownames(.)))
     
     rawEDP = read.csv(file.path(ensfilepath, paste0("meansd_rawens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(rawdate$x))
     rawapccEDP = read.csv(file.path(ensfilepath, paste0("meansd_apccrawens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
          xts(., ymd(apccdate$x))
     
     RMSE_rawapcc = sqrt(mean((obs - rawapccEDP$MEAN)^2)) %>% rep(.,4)
     temperr = year(rawEDP) %in% year(rawapccEDP) %>% rawEDP$MEAN[.]-obs
     RMSE_raw = sqrt(mean(temperr^2)) %>% rep(.,4)
     
     RMSE_upapcc = c()
     RMSE_up = c()
     for (kk in c(1:4)){
          kfold = paste0("k", kk)
          update = read.csv(file.path(ensfilepath, kfold, "upensdate.csv"), row.names=1)
          
          upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(update$x))
          upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
               xts(., ymd(apccdate$x))
          RMSE_upapcc[kk] = sqrt(mean((obs-upapccEDP$MEAN)^2))
          temperr = year(upEDP) %in% year(upapccEDP) %>% upEDP$MEAN[.]-obs
          RMSE_up[kk] = sqrt(mean(temperr^2))
     }
     
     RMSEtable = rbind(RMSEtable, RMSE_raw, RMSE_up, RMSE_rawapcc, RMSE_upapcc) %>% 
          as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     RMSEtable$AVG = apply(RMSEtable, 1, mean)
     rownames(RMSEtable) = EDPnames
     # write.csv(RMSEtable, file.path("./predictResult", paste0("RMSE_apcc_all_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.irr %>% obs[.]
     temp_rawapccEDP = month(rawapccEDP) %in% period.irr %>% rawapccEDP[.]
     RMSE_rawapcc = sqrt(mean((temp_obs - temp_rawapccEDP$MEAN)^2)) %>% rep(.,4)
     temp_rawEDP = month(rawEDP) %in% period.irr %>% rawEDP[.]
     temperr = year(temp_rawEDP) %in% year(temp_rawapccEDP) %>% temp_rawEDP$MEAN[.]-temp_obs
     RMSE_raw = sqrt(mean(temperr^2)) %>% rep(.,4)
     
     RMSE_upapcc = c()
     RMSE_up = c()
     for (kk in c(1:4)){
       kfold = paste0("k", kk)
       
       upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
         xts(., ymd(update$x))
       upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
         xts(., ymd(apccdate$x))
       temp_upapccEDP = month(upapccEDP) %in% period.irr %>% upapccEDP[.]
       RMSE_upapcc[kk] = sqrt(mean((temp_obs-temp_upapccEDP$MEAN)^2))
       temp_upEDP = month(upEDP) %in% period.irr %>% upEDP[.]
       temperr = year(temp_upEDP) %in% year(temp_upapccEDP) %>% temp_upEDP$MEAN[.]-temp_obs
       RMSE_up[kk] = sqrt(mean(temperr^2))
     }
     RMSEtable = c()
     RMSEtable = rbind(RMSEtable, RMSE_raw, RMSE_up, RMSE_rawapcc, RMSE_upapcc) %>% 
       as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     RMSEtable$AVG = apply(RMSEtable, 1, mean)
     rownames(RMSEtable) = EDPnames
     # write.csv(RMSEtable, file.path("./predictResult", paste0("RMSE_apcc_irrigation_",dindexname,"_",bsnname,".csv")))
     
     
     temp_obs = month(obs)%in% period.nonirr %>% obs[.]
     temp_rawapccEDP = month(rawapccEDP) %in% period.nonirr %>% rawapccEDP[.]
     RMSE_rawapcc = sqrt(mean((temp_obs - temp_rawapccEDP$MEAN)^2)) %>% rep(.,4)
     temp_rawEDP = month(rawEDP) %in% period.nonirr %>% rawEDP[.]
     temperr = year(temp_rawEDP) %in% year(temp_rawapccEDP) %>% temp_rawEDP$MEAN[.]-temp_obs
     RMSE_raw = sqrt(mean(temperr^2)) %>% rep(.,4)
     
     RMSE_upapcc = c()
     RMSE_up = c()
     for (kk in c(1:4)){
       kfold = paste0("k", kk)
       
       upEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_upens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
         xts(., ymd(update$x))
       upapccEDP = read.csv(file.path(ensfilepath, kfold, paste0("meansd_apccupens_",dindexname,"_",bsnname,".csv")), row.names=1) %>%
         xts(., ymd(apccdate$x))
       temp_upapccEDP = month(upapccEDP) %in% period.nonirr %>% upapccEDP[.]
       RMSE_upapcc[kk] = sqrt(mean((temp_obs-temp_upapccEDP$MEAN)^2))
       temp_upEDP = month(upEDP) %in% period.nonirr %>% upEDP[.]
       temperr = year(temp_upEDP) %in% year(temp_upapccEDP) %>% temp_upEDP$MEAN[.]-temp_obs
       RMSE_up[kk] = sqrt(mean(temperr^2))
     }
     RMSEtable = c()
     RMSEtable = rbind(RMSEtable, RMSE_raw, RMSE_up, RMSE_rawapcc, RMSE_upapcc) %>% 
       as.data.frame() %>% setNames(., c("k1", "k2", "k3", "k4"))
     RMSEtable$AVG = apply(RMSEtable, 1, mean)
     rownames(RMSEtable) = EDPnames
     # write.csv(RMSEtable, file.path("./predictResult", paste0("RMSE_apcc_non-irrigation_",dindexname,"_",bsnname,".csv")))
     
     
}