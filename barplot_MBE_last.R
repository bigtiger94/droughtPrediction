dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)

period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
MBEpath = file.path("./predictResult")
periods = c("all", "irrigation", "non-irrigation")
plotcol = c("blue", "red", "grey", "green")
EDPnames = c("EDP", "EDP+S", "EDP+A", "EDP+AS")
bsncase = 5

MBEimgpath = file.path("./predictResult", "MBE barplot")

apcctype = "apcc"
for(bsncase in c(1:length(bsnnames))){
     bsnname = bsnnames[bsncase]
     
     
     for(period in periods){
          
          MBEtable = read.csv(file.path(MBEpath, paste0("MBE_apcc_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
          tempMBE.df = data.frame(GROUP=rownames(MBEtable), MBE=MBEtable$AVG)
          if (period=="all") {MBE.df = tempMBE.df
          }else MBE.df = merge(MBE.df, tempMBE.df, by="GROUP")
     }
     colnames(MBE.df) = c("GROUP", periods)
     MBE.df = MBE.df[c(1, 4, 2, 3),]
     yrange = c(-1, 1)
     if (dindexname=="sri12") yrange = c(-0.2, 0.2)
     plotname = paste("MBE of", toupper(dindexname),"with APCC at", bsnname)
     jpeg(file.path(MBEimgpath, paste0(plotname,".jpg")))
     barplot(as.matrix(MBE.df[,-1]), beside=T, col=plotcol, main=plotname, ylim=yrange)
     abline(h=seq(yrange[1], yrange[2], length.out=9), col="grey", lty=3)
     legend("topright", legend=MBE.df[,1], fill=plotcol, box.lty=0)
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
          
          MBEtable = read.csv(file.path(MBEpath, paste0("MBE_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
          tempMBE.df = data.frame(GROUP=rownames(MBEtable), MBE=MBEtable$AVG)
          if (period=="all") {MBE.df = tempMBE.df
          }else MBE.df = merge(MBE.df, tempMBE.df, by="GROUP")
     }
     colnames(MBE.df) = c("GROUP", periods)
     
     
     yrange = c(-1, 1)
     if (dindexname=="sri12") yrange = c(-0.2, 0.2)
     plotname = paste("MBE of", toupper(dindexname),"at", bsnname)
     jpeg(file.path(MBEimgpath, paste0(plotname,".jpg")))
     barplot(as.matrix(MBE.df[,-1]), beside=T, col=plotcol2, main=plotname, ylim=yrange)
     abline(h=seq(yrange[1], yrange[2], length.out=9), col="grey", lty=3)
     legend("topright", legend=EDPnames2, fill=plotcol, box.lty=0)
     dev.off()
}
# graphics.off()
