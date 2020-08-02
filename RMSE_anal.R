dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
RMSEpath = file.path("./predictResult")
periods = c("all", "irrigation", "non-irrigation")

EDPnames = c("EDP", "EDP+S", "EDP+A", "EDP+AS")
bsncase = 1

RMSEimgpath = file.path("./predictResult", "RMSEanal")

apcctype = "apcc"
RMSE_apcc_all.df = c()
RMSE_apcc_irr.df = c()
RMSE_apcc_nonirr.df = c()
for(bsncase in c(1:length(bsnnames))){
  bsnname = bsnnames[bsncase]
  
  
  for(period in periods){
    
    RMSEtable = read.csv(file.path(RMSEpath, paste0("RMSE_apcc_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
    tempRMSE.df = data.frame(GROUP=rownames(RMSEtable), RMSE=RMSEtable$AVG)
    

    if (period=="all") {
      RMSE_apcc_all.df = cbind(RMSE_apcc_all.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }else if(period=="irrigation"){
      RMSE_apcc_irr.df = cbind(RMSE_apcc_irr.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }else if(period=="non-irrigation"){
      RMSE_apcc_nonirr.df = cbind(RMSE_apcc_nonirr.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }
  }
  
}
colnames(RMSE_apcc_all.df) = bsnnames
colnames(RMSE_apcc_irr.df) = bsnnames
colnames(RMSE_apcc_nonirr.df) = bsnnames

# write.csv(RMSE_apcc_all.df, file.path(RMSEimgpath, paste0("RMSEtable_apcc_all_",dindexname,".csv")))
# write.csv(RMSE_apcc_irr.df, file.path(RMSEimgpath, paste0("RMSEtable_apcc_irrigation_",dindexname,".csv")))
# write.csv(RMSE_apcc_nonirr.df, file.path(RMSEimgpath, paste0("RMSEtable_apcc_non-irrigation_",dindexname,".csv")))

RMSEmean_apcc_all = RMSE_apcc_all.df %>% apply(., 1, mean)
RMSEmean_apcc_irr = RMSE_apcc_irr.df %>% apply(., 1, mean)
RMSEmean_apcc_nonirr = RMSE_apcc_nonirr.df %>% apply(., 1, mean)

# heatmap
nbasin = length(bsnnames)
RMSEheatmap.df = data.frame(BSN = toupper(rep(bsnnames, each=4)), Case = rep(rownames(RMSE_apcc_all.df), nbasin), 
                    RMSE = as.vector(as.matrix(RMSE_apcc_all.df)),
                    RMSEirr = as.vector(as.matrix(RMSE_apcc_irr.df)),
                    RMSEnonirr = as.vector(as.matrix(RMSE_apcc_nonirr.df)))

min(RMSEheatmap.df$RMSE)
max(RMSEheatmap.df$RMSE)
legendlimit = c(0.4, 0.8)
if(dindexname=="sri12") legendlimit = c(0.2, 0.3)
heatmapname = "RMSE_apcc_all"
heatmapplot = ggplot(RMSEheatmap.df, aes(x=BSN, y=Case)) + geom_tile(aes(fill=RMSE)) + theme_bw() + xlab("")+ylab("") + ggtitle("RMSE") +
  scale_x_discrete(position = "top", limits=unique(RMSEheatmap.df$BSN)) +
  scale_y_discrete(limits = rev(unique(RMSEheatmap.df$Case))) +
  scale_fill_gradientn(colours=c("blue", "white"),
                       limits = legendlimit,
                       breaks = c(legendlimit[1],legendlimit[2]),
                       labels = c(legendlimit[1],legendlimit[2])) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title = element_text(hjust=0.5, face="bold", size=20))

ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname, ".png"), path=RMSEimgpath)

# 
# data.frame(All=RMSEmean_apcc_all, Irrigation=RMSEmean_apcc_irr, Non_irrigation=RMSEmean_apcc_nonirr) %>%
#   write.csv(., file.path(RMSEimgpath, paste0("AvgRMSE_apcc_",dindexname,".csv")))

ylimit = c(0,1)
plotcol = c("blue", "red", "grey", "green")
plotname = paste("RMSE of", toupper(dindexname),"with APCC")

# if (dindexname == "sri12") ylimit = c(0,0.5)
# png(file.path(RMSEimgpath, paste0("Avg_",plotname,".png")))
# data.frame(All=RMSEmean_apcc_all, Irrigation=RMSEmean_apcc_irr, Non.irrigation=RMSEmean_apcc_nonirr) %>%
#   as.matrix(.) %>% barplot(., beside=T, col=plotcol, main=plotname, ylim=ylimit)
# abline(h=seq(ylimit[1], ylimit[2], length.out=6), col="grey", lty=3)
# legend("topright", legend=EDPnames, fill=plotcol, box.lty=0)
# dev.off()
# 



apcctype = ""
RMSE_all.df = c()
RMSE_irr.df = c()
RMSE_nonirr.df = c()

for(bsncase in c(1:length(bsnnames))){
  bsnname = bsnnames[bsncase]
  
  
  for(period in periods){
    
    RMSEtable = read.csv(file.path(RMSEpath, paste0("RMSE_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
    tempRMSE.df = data.frame(GROUP=rownames(RMSEtable), RMSE=RMSEtable$AVG)
    
    
    if (period=="all") {
      RMSE_all.df = cbind(RMSE_all.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }else if(period=="irrigation"){
      RMSE_irr.df = cbind(RMSE_irr.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }else if(period=="non-irrigation"){
      RMSE_nonirr.df = cbind(RMSE_nonirr.df, tempRMSE.df$RMSE) %>% 'rownames<-'(as.vector(tempRMSE.df$GROUP))
    }
  }
  
}

colnames(RMSE_all.df) = bsnnames
colnames(RMSE_irr.df) = bsnnames
colnames(RMSE_nonirr.df) = bsnnames
# 
# write.csv(RMSE_all.df, file.path(RMSEimgpath, paste0("RMSEtable_all_",dindexname,".csv")))
# write.csv(RMSE_irr.df, file.path(RMSEimgpath, paste0("RMSEtable_irrigation_",dindexname,".csv")))
# write.csv(RMSE_nonirr.df, file.path(RMSEimgpath, paste0("RMSEtable_non-irrigation_",dindexname,".csv")))

RMSEmean_all = RMSE_all.df %>% apply(., 1, mean)
RMSEmean_irr = RMSE_irr.df %>% apply(., 1, mean)
RMSEmean_nonirr = RMSE_nonirr.df %>% apply(., 1, mean)

# 
# 
# data.frame(All=RMSEmean_all, Irrigation=RMSEmean_irr, Non_irrigation=RMSEmean_nonirr) %>%
#   write.csv(., file.path(RMSEimgpath, paste0("AvgRMSE_",dindexname,".csv")))

# plotcol2 = plotcol[1:2]
# plotname = paste("RMSE of", toupper(dindexname))
# png(file.path(RMSEimgpath, paste0("Avg_",plotname,".png")))
# data.frame(All=RMSEmean_all, Irrigation=RMSEmean_irr, Non.irrigation=RMSEmean_nonirr) %>%
#   as.matrix(.) %>% barplot(., beside=T, col=plotcol2, main=plotname, ylim=ylimit)
# abline(h=seq(ylimit[1], ylimit[2], length.out=6), col="grey", lty=3)
# legend("topright", legend=EDPnames[1:2], fill=plotcol2, box.lty=0)
# dev.off()
# graphics.off()
