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
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_all.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_all.df, .)
  period = periods[2]
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_irr.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_irr.df, .)
  period = periods[3]
  tempBS.df = as.data.frame(read_xlsx(file.path(acctablepath, paste0("acctable_",period,"_",dindexname,"_",bsnname,".xlsx"))))
  tempBS.df$Region = bsnname
  BS_nonirr.df = tempBS.df %>% mutate(., Region=bsnname) %>% rbind(BS_nonirr.df, .)
}

tempBS.df = BS_all.df

nbasin = length(bsnnames)
nphase = length(unique(tempBS.df$Phase))
RES_EDP_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
RES_EDPS_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
REL_EDP_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
REL_EDPS_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
BS_EDP_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
BS_EDPS_all.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
for (ii in c(1:nphase)){
  dd = unique(BS_all.df$Phase)[ii]
  for(bsnname in bsnnames){
    RES_EDP_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., RES)
    RES_EDPS_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., RES)
    REL_EDP_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., REL)
    REL_EDPS_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., REL)
    BS_EDP_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., BS)
    BS_EDPS_all.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., BS)
    
  }
}


tempBS.df = BS_irr.df

nbasin = length(bsnnames)
nphase = length(unique(tempBS.df$Phase))
RES_EDP_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
RES_EDPS_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
REL_EDP_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
REL_EDPS_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
BS_EDP_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
BS_EDPS_irr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase)) %>% setNames(., bsnnames)
for (ii in c(1:nphase)){
  dd = unique(BS_irr.df$Phase)[ii]
  for(bsnname in bsnnames){
    RES_EDP_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., RES)
    RES_EDPS_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., RES)
    REL_EDP_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., REL)
    REL_EDPS_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., REL)
    BS_EDP_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., BS)
    BS_EDPS_irr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., BS)
    
  }
}


tempBS.df = BS_nonirr.df

nbasin = length(bsnnames)
nphase = length(unique(tempBS.df$Phase))
RES_EDP_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
RES_EDPS_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
REL_EDP_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
REL_EDPS_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
BS_EDP_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
BS_EDPS_nonirr.df = as.data.frame(matrix(ncol=nbasin, nrow=nphase), row.names=unique(tempBS.df$Phase)) %>% setNames(., bsnnames)
for (ii in c(1:nphase)){
  dd = unique(BS_nonirr.df$Phase)[ii]
  for(bsnname in bsnnames){
    RES_EDP_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., RES)
    RES_EDPS_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., RES)
    REL_EDP_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., REL)
    REL_EDPS_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., REL)
    BS_EDP_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP", Region==bsnname) %>% dplyr::select(., BS)
    BS_EDPS_nonirr.df[ii,as.character(bsnname)] = tempBS.df %>% dplyr::filter(., Phase==dd, Type=="PP", Case=="EDP+S", Region==bsnname) %>% dplyr::select(., BS)
    
  }
}
BSdiff = data.frame(BSN = toupper(rep(bsnnames, each=nphase)), Phase = rep(unique(tempBS.df$Phase), nbasin), 
                  BSdiff = as.vector(as.matrix(BS_EDP_all.df - BS_EDPS_all.df)),
                  RESdiff = as.vector(as.matrix(RES_EDP_all.df - RES_EDPS_all.df)),
                  RELdiff = as.vector(as.matrix(REL_EDP_all.df - REL_EDPS_all.df)))


legendlimit = c(-0.02, 0.02)
if(dindexname=="sri12") legendlimit = c(-0.01, 0.01)

BSdiffheatmap = ggplot(BSdiff, aes(x=BSN, y=Phase)) + geom_tile(aes(fill=BSdiff)) + theme_bw() + xlab("")+ylab("") + ggtitle("BS diff.") +
  scale_x_discrete(position = "top", limits=unique(BSdiff$BSN)) +
  scale_y_discrete(limits = rev(unique(BSdiff$Phase))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"),
                        limits = legendlimit,
                        breaks = c(legendlimit[1], 0, legendlimit[2]),
                        labels = c(legendlimit[1], 0, legendlimit[2])) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title = element_text(hjust=0.5, face="bold", size=20))
ggsave(BSdiffheatmap, filename = paste0("BSdiff_",dindexname,".png"), path=BSanalpath)

RELdiffheatmap = ggplot(BSdiff, aes(x=BSN, y=Phase)) + geom_tile(aes(fill=RELdiff)) + theme_bw() + xlab("")+ ylab("") + ggtitle("REL diff.") +
  scale_x_discrete(position = "top", limits=unique(BSdiff$BSN)) +
  scale_y_discrete(limits = rev(unique(BSdiff$Phase))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"),
                       limits = legendlimit,
                       breaks = c(legendlimit[1], 0, legendlimit[2]),
                       labels = c(legendlimit[1], 0, legendlimit[2])) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title = element_text(hjust=0.5, face="bold", size=20))
ggsave(RELdiffheatmap, filename = paste0("RELdiff_",dindexname,".png"), path=BSanalpath)

RESdiffheatmap = ggplot(BSdiff, aes(x=BSN, y=Phase)) + geom_tile(aes(fill=RESdiff)) + theme_bw() + xlab("")+ ylab("") + ggtitle("RES diff.") +
  scale_x_discrete(position = "top", limits=unique(BSdiff$BSN)) +
  scale_y_discrete(limits = rev(unique(BSdiff$Phase))) +
  scale_fill_gradientn(colours=c( "blue", "white", "red"),
                       limits = legendlimit,
                       breaks = c(legendlimit[1], 0, legendlimit[2]),
                       labels = c(legendlimit[1], 0, legendlimit[2])) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title = element_text(hjust=0.5, face="bold", size=20))
ggsave(RESdiffheatmap, filename = paste0("RESdiff_",dindexname,".png"), path=BSanalpath)

RESmean_EDP_all.df = RES_EDP_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RESmean_EDPS_all.df = RES_EDPS_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

RELmean_EDP_all.df = REL_EDP_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RELmean_EDPS_all.df = REL_EDPS_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

BSmean_EDP_all.df = BS_EDP_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
BSmean_EDPS_all.df = BS_EDPS_all.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))


data.frame(RESmean_EDP_all.df, RESmean_EDPS_all.df, RELmean_EDP_all.df, RELmean_EDPS_all.df, BSmean_EDP_all.df, BSmean_EDPS_all.df) %>% 
  setNames(., c("RES_EDP", "RES_EDP+S", "REL_EDP", "REL_EDP+S", "BS_EDP", "BS_EDP+S")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_all_",dindexname,".csv")))

RESmean_EDP_irr.df = RES_EDP_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RESmean_EDPS_irr.df = RES_EDPS_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

RELmean_EDP_irr.df = REL_EDP_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RELmean_EDPS_irr.df = REL_EDPS_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

BSmean_EDP_irr.df = BS_EDP_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
BSmean_EDPS_irr.df = BS_EDPS_irr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

data.frame(RESmean_EDP_irr.df, RESmean_EDPS_irr.df, RELmean_EDP_irr.df, RELmean_EDPS_irr.df, BSmean_EDP_irr.df, BSmean_EDPS_irr.df) %>% 
  setNames(., c("RES_EDP", "RES_EDP+S", "REL_EDP", "REL_EDP+S", "BS_EDP", "BS_EDP+S")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_irr_",dindexname,".csv")))

RESmean_EDP_nonirr.df = RES_EDP_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RESmean_EDPS_nonirr.df = RES_EDPS_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

RELmean_EDP_nonirr.df = REL_EDP_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
RELmean_EDPS_nonirr.df = REL_EDPS_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))

BSmean_EDP_nonirr.df = BS_EDP_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))
BSmean_EDPS_nonirr.df = BS_EDPS_nonirr.df%>%apply(., 1, mean) %>% setNames(., unique(tempBS.df$Phase))


data.frame(RESmean_EDP_nonirr.df, RESmean_EDPS_nonirr.df, RELmean_EDP_nonirr.df, RELmean_EDPS_nonirr.df, BSmean_EDP_nonirr.df, BSmean_EDPS_nonirr.df) %>% 
  setNames(., c("RES_EDP", "RES_EDP+S", "REL_EDP", "REL_EDP+S", "BS_EDP", "BS_EDP+S")) %>%
  write.csv(., file.path(BSanalpath, paste0("Avg_BScomponents_nonirr_",dindexname,".csv")))





BS_DP_all = BS_all.df %>% dplyr::select(., Case, Phase, Type, BS) %>% dplyr::filter(.,Type=="DP")
BS_DP_irr = BS_irr.df %>% dplyr::select(., Case, Phase, Type, BS) %>% dplyr::filter(.,Type=="DP")
BS_DP_nonirr = BS_nonirr.df %>% dplyr::select(., Case, Phase, Type, BS) %>% dplyr::filter(.,Type=="DP")
for(phase in unique(BS_DP_all$Phase)){
  print(phase)
  tempBS_ = BS_DP_all %>% dplyr::filter(., Case=="EDP", Phase==phase)
  print("all EDP")
  print(mean(tempBS_$BS))
  tempBS_ = BS_DP_all %>% dplyr::filter(., Case=="EDP+S", Phase==phase)
  print("all EDP+S")
  print(mean(tempBS_$BS))
  
  tempBS_ = BS_DP_irr %>% dplyr::filter(., Case=="EDP", Phase==phase)
  print("irr EDP")
  print(mean(tempBS_$BS))
  tempBS_ = BS_DP_irr %>% dplyr::filter(., Case=="EDP+S", Phase==phase)
  print("irr EDP+S")
  print(mean(tempBS_$BS))
  
  tempBS_ = BS_DP_nonirr %>% dplyr::filter(., Case=="EDP", Phase==phase)
  print("nonirr EDP")
  print(mean(tempBS_$BS))
  tempBS_ = BS_DP_nonirr %>% dplyr::filter(., Case=="EDP+S", Phase==phase)
  print("nonirr EDP+S")
  print(mean(tempBS_$BS))
}
