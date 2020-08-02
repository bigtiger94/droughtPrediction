# install.packages("dynatopmodel")
library(dynatopmodel)

bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
targety = 2008:2017
acc.df = data.frame()

for(bsncase in c(1:length(bsnnames))){
     bsncase = 6
  streamflowpath = file.path("./streamflow", bsnnames[bsncase])
  
  q_obs = read.csv(file.path(streamflowpath, paste0("obs_",bsnnames[bsncase],".csv")),row.names=1) %>%
          xts(., as.Date(rownames(.)))
  q_esp = read.csv(file.path(streamflowpath, paste0("espmean_",bsnnames[bsncase],".csv")),row.names=1) %>%
    xts(., as.Date(rownames(.)))
  q_obs = year(q_obs) %in% c(targety) %>% q_obs[.]
  
  q_esp = year(q_esp) %in% c(targety) %>% q_esp[.]
  nse = NSE(q_esp, q_obs, 3)
  nrmse = RMSE(q_esp, q_obs) / mean(q_obs)
  
  png(filename = file.path("./streamflow", paste0("tsfig_",bsnnames[bsncase],".png")), height = 600, width = 900)
  plot(q_obs, col="black", main="")
  lines(q_esp, col="red")
  addLegend("topleft", legend.names=c("Obs.", "ESP mean"), lty=1, lwd=2,
            col=c("black", "red"), cex=1.2, bty="o")
  dev.off()
  acc.df = rbind(acc.df, data.frame(bsn=bsnnames[bsncase], NSE=nse, NRMSE=nrmse))
}

write.csv(acc.df, file.path("./streamflow", paste0("espaccuracy.csv")))