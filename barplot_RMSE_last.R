dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
RMSEpath = file.path("./predictResult")
periods = c("all", "irrigation", "non-irrigation")
plotcol = c("blue", "red", "grey", "green")
EDPnames = c("EDP", "EDP+S", "EDP+A", "EDP+AS")
bsncase = 1

RMSEimgpath = file.path("./predictResult", "RMSE barplot")

apcctype = "apcc"
for(bsncase in c(1:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     
     for(period in periods){
          
          RMSEtable = read.csv(file.path(RMSEpath, paste0("RMSE_apcc_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
          tempRMSE.df = data.frame(GROUP=rownames(RMSEtable), RMSE=RMSEtable$AVG)
          if (period=="all") {RMSE.df = tempRMSE.df
          }else RMSE.df = merge(RMSE.df, tempRMSE.df, by="GROUP")
     }
     colnames(RMSE.df) = c("GROUP", periods)

     RMSE.df = RMSE.df[c(1,4,2,3),]
     plotname = paste("RMSE of", toupper(dindexname),"with APCC at", bsnname)
     jpeg(file.path(RMSEimgpath, paste0(plotname,".jpg")))
     barplot(as.matrix(RMSE.df[,-1]), beside=T, col=plotcol, main=plotname, ylim=c(0,1))
     abline(h=seq(0, 1, length.out=5), col="grey", lty=3)
     legend("topright", legend=EDPnames, fill=plotcol, box.lty=0)
     dev.off()
    
}
# graphics.off()


bsncase = 1
plotcol2 = plotcol[1:2]
EDPnames2 = EDPnames[1:2]

apcctype = ""
for(bsncase in c(1:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     
     for(period in periods){
          
          RMSEtable = read.csv(file.path(RMSEpath, paste0("RMSE_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
          tempRMSE.df = data.frame(GROUP=rownames(RMSEtable), RMSE=RMSEtable$AVG)
          if (period=="all") {RMSE.df = tempRMSE.df
          }else RMSE.df = merge(RMSE.df, tempRMSE.df, by="GROUP")
     }
     colnames(RMSE.df) = c("GROUP", periods)
     
    
     plotname = paste("RMSE of", toupper(dindexname),"at", bsnname)
     jpeg(file.path(RMSEimgpath, paste0(plotname,".jpg")))
     barplot(as.matrix(RMSE.df[,-1]), beside=T, col=plotcol2, main=plotname, ylim=c(0,1))
     abline(h=seq(0, 1, length.out=5), col="grey", lty=3)
     legend("topright", legend=EDPnames2, fill=plotcol, box.lty=0)
     dev.off()
}
# graphics.off()
