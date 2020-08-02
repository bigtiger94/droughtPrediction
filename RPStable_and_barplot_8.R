dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = "apcc"
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
acctablepath = file.path("./predictResult")

periods = c("all", "irrigation", "non-irrigation")
plotcol = c("blue", "red", "grey", "green")
EDPnames = c("EDP", "EDP+S", "EDP+A", "EDP+AS")
bsncase = 1
# 
# for(bsncase in c(1:length(bsnnames))){
# bsnname = bsnnames[bsncase]
# 
# for(period in periods){
# 
#   acctable = read.csv(file.path(acctablepath, paste0("accuracytable_apcc_summarised_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
#   
#   
#   tempRPStable = distinct(acctable, GROUP, RPS) %>% dplyr::filter(!is.na(RPS))
#   if (period=="all") RPStable = tempRPStable
#   else RPStable = merge(RPStable, tempRPStable, by="GROUP")
# }
# colnames(RPStable) = c("GROUP", periods)
# 
# 
# RPStable = RPStable[c(3,4,1,2),]
# RPStable$GROUP = EDPnames
# win.graph()
# 
# plotname = paste("RPS of", dindexname,"with APCC at", bsnname)
# barplot(as.matrix(RPStable[,-1]), beside=T, col=plotcol,
#         ylim=c(0, 0.2), main=plotname)
# abline(h=seq(0, 0.2, length.out=5), col="grey", lty=3)
# legend("topright", legend=EDPnames, fill=plotcol, box.lty=0)
# # write.csv(RPStable, file.path(acctablepath, paste0("RPS_apcc_",dindexname,"_",bsnname,".csv")))
# }



bsncase = 1
plotcol2 = plotcol[1:2]
EDPnames2 = c("EDP DP", "EDP PP", "EDP+S DP", "EDP+S PP")
for(bsncase in c(1:length(bsnnames))){
  bsnname = bsnnames[bsncase]
  
  for(period in periods){
    
    acctable = read.csv(file.path(acctablepath, paste0("accuracytable_summarised_",period,"_",dindexname,"_",bsnname,".csv")), row.names=1)
    
    
    tempRPStable = distinct(acctable, GROUP, RPS) %>% dplyr::filter(!is.na(RPS))
    if (period=="all") RPStable = tempRPStable
    else RPStable = merge(RPStable, tempRPStable, by="GROUP")
  }
  colnames(RPStable) = c("GROUP", periods)
  # RPStable = RPStable[c(3,4,1,2),]
  RPStable$GROUP = EDPnames2
  # plotname = paste("RPS of", dindexname,"at", bsnname)
  # win.graph()
  # barplot(as.matrix(RPStable[,-1]), beside=T, col=plotcol2,
  #         ylim=c(0, 0.2), main=plotname)
  # abline(h=seq(0, 0.2, length.out=5), col="grey", lty=3)
  # legend("topright", legend=EDPnames2, fill=plotcol2, box.lty=0)
  write.csv(RPStable, file.path(acctablepath, paste0("RPS_",dindexname,"_",bsnname,".csv")))
}

# graphics.off()
