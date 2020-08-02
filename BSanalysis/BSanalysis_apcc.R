library(readxl)
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
bsncase = 1
periods = c("all", "irrigation", "non-irrigation")
acctablepath = file.path("./predictResult", "summary")
BSanalpath = file.path("./predictResult", "BSanal")

BS_all.df = data.frame()
BS_irr.df = data.frame()
BS_nonirr.df = data.frame()
for (bsnname in bsnnames){
  period = periods[1]
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_apcc_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_all.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_all.df, .)
  period = periods[2]
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_apcc_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_irr.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_irr.df, .)
  period = periods[3]
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_apcc_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_nonirr.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_nonirr.df, .)
}


nbasin = length(bsnnames)
nphase = length(unique(BS_all.df$Phase))

EDP_BScompmean_all.df = c()
EDPS_BScompmean_all.df = c()
EDPA_BScompmean_all.df = c()
EDPAS_BScompmean_all.df = c()
for(dd in unique(BS_all.df$Phase)){
  EDP_BScompmean_all.df = BS_all.df %>% dplyr::filter(., Phase==dd, Case=="EDP") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDP_BScompmean_all.df, .) %>% as.data.frame(.)
  EDPS_BScompmean_all.df = BS_all.df %>% dplyr::filter(., Phase==dd, Case=="EDP+") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPS_BScompmean_all.df, .) %>% as.data.frame(.)
  EDPA_BScompmean_all.df = BS_all.df %>% dplyr::filter(., Phase==dd, Case=="EDP+A") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPA_BScompmean_all.df, .) %>% as.data.frame(.)
  EDPAS_BScompmean_all.df = BS_all.df %>% dplyr::filter(., Phase==dd, Case=="EDP+AS") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPAS_BScompmean_all.df, .) %>% as.data.frame(.)
}
rownames(EDP_BScompmean_all.df) = unique(BS_all.df$Phase)
rownames(EDPS_BScompmean_all.df) = unique(BS_all.df$Phase)
rownames(EDPA_BScompmean_all.df) = unique(BS_all.df$Phase)
rownames(EDPAS_BScompmean_all.df) = unique(BS_all.df$Phase)

RESmean_apcc_all.df = cbind(EDP_BScompmean_all.df$RES, EDPS_BScompmean_all.df$RES,
                            EDPA_BScompmean_all.df$RES, EDPAS_BScompmean_all.df$RES) %>% as.data.frame(.) %>% 
                            setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_all.df))
RELmean_apcc_all.df = cbind(EDP_BScompmean_all.df$REL, EDPS_BScompmean_all.df$REL,
                            EDPA_BScompmean_all.df$REL, EDPAS_BScompmean_all.df$REL) %>% as.data.frame(.) %>% 
                            setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_all.df))
BSmean_apcc_all.df = cbind(EDP_BScompmean_all.df$BS, EDPS_BScompmean_all.df$BS,
                           EDPA_BScompmean_all.df$BS, EDPAS_BScompmean_all.df$BS) %>% as.data.frame(.) %>% 
                           setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_all.df))

EDP_BScompmean_irr.df = c()
EDPS_BScompmean_irr.df = c()
EDPA_BScompmean_irr.df = c()
EDPAS_BScompmean_irr.df = c()
for(dd in unique(BS_irr.df$Phase)){
  EDP_BScompmean_irr.df = BS_irr.df %>% dplyr::filter(., Phase==dd, Case=="EDP") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDP_BScompmean_irr.df, .) %>% as.data.frame(.)
  EDPS_BScompmean_irr.df = BS_irr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPS_BScompmean_irr.df, .) %>% as.data.frame(.)
  EDPA_BScompmean_irr.df = BS_irr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+A") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPA_BScompmean_irr.df, .) %>% as.data.frame(.)
  EDPAS_BScompmean_irr.df = BS_irr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+AS") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPAS_BScompmean_irr.df, .) %>% as.data.frame(.)
}
rownames(EDP_BScompmean_irr.df) = unique(BS_irr.df$Phase)
rownames(EDPS_BScompmean_irr.df) = unique(BS_irr.df$Phase)
rownames(EDPA_BScompmean_irr.df) = unique(BS_irr.df$Phase)
rownames(EDPAS_BScompmean_irr.df) = unique(BS_irr.df$Phase)

RESmean_apcc_irr.df = cbind(EDP_BScompmean_irr.df$RES, EDPS_BScompmean_irr.df$RES,
                            EDPA_BScompmean_irr.df$RES, EDPAS_BScompmean_irr.df$RES) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_irr.df))
RELmean_apcc_irr.df = cbind(EDP_BScompmean_irr.df$REL, EDPS_BScompmean_irr.df$REL,
                            EDPA_BScompmean_irr.df$REL, EDPAS_BScompmean_irr.df$REL) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_irr.df))
BSmean_apcc_irr.df = cbind(EDP_BScompmean_irr.df$BS, EDPS_BScompmean_irr.df$BS,
                           EDPA_BScompmean_irr.df$BS, EDPAS_BScompmean_irr.df$BS) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_irr.df))


EDP_BScompmean_nonirr.df = c()
EDPS_BScompmean_nonirr.df = c()
EDPA_BScompmean_nonirr.df = c()
EDPAS_BScompmean_nonirr.df = c()
for(dd in unique(BS_nonirr.df$Phase)){
  EDP_BScompmean_nonirr.df = BS_nonirr.df %>% dplyr::filter(., Phase==dd, Case=="EDP") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDP_BScompmean_nonirr.df, .) %>% as.data.frame(.)
  EDPS_BScompmean_nonirr.df = BS_nonirr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPS_BScompmean_nonirr.df, .) %>% as.data.frame(.)
  EDPA_BScompmean_nonirr.df = BS_nonirr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+A") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPA_BScompmean_nonirr.df, .) %>% as.data.frame(.)
  EDPAS_BScompmean_nonirr.df = BS_nonirr.df %>% dplyr::filter(., Phase==dd, Case=="EDP+AS") %>% dplyr::select(REL, RES, BS) %>% apply(., 2, mean) %>%
    rbind(EDPAS_BScompmean_nonirr.df, .) %>% as.data.frame(.)
}
rownames(EDP_BScompmean_nonirr.df) = unique(BS_nonirr.df$Phase)
rownames(EDPS_BScompmean_nonirr.df) = unique(BS_nonirr.df$Phase)
rownames(EDPA_BScompmean_nonirr.df) = unique(BS_nonirr.df$Phase)
rownames(EDPAS_BScompmean_nonirr.df) = unique(BS_nonirr.df$Phase)

RESmean_apcc_nonirr.df = cbind(EDP_BScompmean_nonirr.df$RES, EDPS_BScompmean_nonirr.df$RES,
                               EDPA_BScompmean_nonirr.df$RES, EDPAS_BScompmean_nonirr.df$RES) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_nonirr.df))
RELmean_apcc_nonirr.df = cbind(EDP_BScompmean_nonirr.df$REL, EDPS_BScompmean_nonirr.df$REL,
                               EDPA_BScompmean_nonirr.df$REL, EDPAS_BScompmean_nonirr.df$REL) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_nonirr.df))
BSmean_apcc_nonirr.df = cbind(EDP_BScompmean_nonirr.df$BS, EDPS_BScompmean_nonirr.df$BS,
                              EDPA_BScompmean_nonirr.df$BS, EDPAS_BScompmean_nonirr.df$BS) %>% as.data.frame(.) %>% 
  setNames(., c("EDP", "EDP+S", "EDP+A", "EDP+AS")) %>% 'rownames<-'(rownames(EDP_BScompmean_nonirr.df))


data.frame(RESmean_apcc_all.df, RELmean_apcc_all.df, BSmean_apcc_all.df) %>%
  setNames(., c("RES_EDP", "RES_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "REL_EDP", "REL_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "BS_EDP", "BS_EDP+S", "BS_EDP+A", "BS_EDP+AS")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_apcc_all_",dindexname,".csv")))


data.frame(RESmean_apcc_irr.df, RELmean_apcc_irr.df, BSmean_apcc_irr.df) %>%
  setNames(., c("RES_EDP", "RES_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "REL_EDP", "REL_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "BS_EDP", "BS_EDP+S", "BS_EDP+A", "BS_EDP+AS")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_apcc_irr_",dindexname,".csv")))

data.frame(RESmean_apcc_nonirr.df, RELmean_apcc_nonirr.df, BSmean_apcc_nonirr.df) %>%
  setNames(., c("RES_EDP", "RES_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "REL_EDP", "REL_EDP+S", "RES_EDP+A", "RES_EDP+AS",
                "BS_EDP", "BS_EDP+S", "BS_EDP+A", "BS_EDP+AS")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_apcc_nonirr_",dindexname,".csv")))
