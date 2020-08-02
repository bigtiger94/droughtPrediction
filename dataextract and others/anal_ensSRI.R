ensfilepath = file.path("./ensSRI")
list.files(ensfilepath)

sy.ens_sri3.df = read.csv(file.path(ensfilepath, "ens_sri3_soyang.csv"), row.names=1) %>% xts(., ymd(rownames(.)))
sy.ens_sri12.df = read.csv(file.path(ensfilepath, "ens_sri12_soyang.csv"), row.names=1) %>% xts(., ymd(rownames(.)))


dc.ens_sri3.df = read.csv(file.path(ensfilepath, "ens_sri3_daecheong.csv"), row.names=1) %>% xts(., ymd(rownames(.)))
dc.ens_sri12.df = read.csv(file.path(ensfilepath, "ens_sri12_daecheong.csv"), row.names=1) %>% xts(., ymd(rownames(.)))


temp_enssri.df = dc.ens_sri3.df
# temp_enssri.df = cbind(year(temp_enssri.df), month(temp_enssri.df), temp_enssri.df) %>% setNames(., c("Year", "Month", colnames(temp_enssri.df)))
# write.csv(dc.ens3stat.df, file.path("./ensSRI", "stat_ens_sri3_daecheong.csv"))

sy.ens3stat.df = as.data.frame(matrix(NA, ncol=3, nrow=12)) %>% setNames(., c("mean", "sd", "skewness"))
bsnname = "soyang"
for (mm in c(1:12)){
     temp_enssri.mm = which(month(temp_enssri.df)==mm) %>% temp_enssri.df[.]
     sy.ens3stat.df$mean[mm] = mean(as.vector(temp_enssri.mm))
     sy.ens3stat.df$sd[mm] = sd(as.vector(temp_enssri.mm))
     sy.ens3stat.df$skewness[mm] = skewness(as.vector(temp_enssri.mm))
     # temp_emp = rank(as.vector(temp_enssri.mm))/(length(as.vector(temp_enssri.mm))+1)

     histtitle = paste("Ensemble SRI3 at" , bsnname, "on", mm)
     # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri3", paste0(histtitle,".jpeg")))
     #
     win.graph()
     hist(as.vector(temp_enssri.mm), probability = T,main=histtitle, xlab="SRI")
     xseq = seq(sy.ens3stat.df$mean[mm]-10, sy.ens3stat.df$mean[mm]+10, 0.01)
     dens = dnorm(xseq, sy.ens3stat.df$mean[mm], sy.ens3stat.df$sd[mm])
     lines(xseq, dens, col="red")
     # dev.off()
     #
     qqplottitle = paste("Ensemble SRI3 Normal Q-Q Plot at", bsnname, "on", mm)
     # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri3", paste0(qqplottitle,".jpeg")))
     win.graph()
     qqnorm(as.vector(temp_enssri.mm), main=qqplottitle)
     qqline(as.vector(temp_enssri.mm), col="red")
     # dev.off()

     # print(chisq.test(as.vector(temp_enssri.mm), pnorm(as.vector(temp_enssri.mm), sy.ens3stat.df$mean[mm]$mean[mm], sy.ens3stat.df$mean[mm]$sd[mm])))
     # print(ks.test(as.vector(temp_enssri.mm), "pnorm", mean=sy.ens3stat.df$mean[mm], sd=sy.ens3stat.df$sd[mm]))
     print(shapiro.test(as.vector(temp_enssri.mm)))
}
graphics.off()
sy.ens12stat.df = as.data.frame(matrix(NA, ncol=3, nrow=12)) %>% setNames(., c("mean", "sd", "skewness"))
bsnname = "soyang"
for (mm in c(1:12)){
  temp_enssri.mm = which(month(temp_enssri.df)==mm) %>% temp_enssri.df[.]
  sy.ens12stat.df$mean[mm] = mean(as.vector(temp_enssri.mm))
  sy.ens12stat.df$sd[mm] = sd(as.vector(temp_enssri.mm))
  sy.ens12stat.df$skewness[mm] = skewness(as.vector(temp_enssri.mm))
  # temp_emp = rank(as.vector(temp_enssri.mm))/(length(as.vector(temp_enssri.mm))+1)

  # histtitle = paste("Ensemble SRI12 at" , bsnname, "on", mm)
  # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri12", paste0(histtitle,".jpeg")))
  #
  # hist(as.vector(temp_enssri.mm), probability = T,main=histtitle, xlab="SRI")
  # xseq = seq(sy.ens12stat.df$mean[mm]-10, sy.ens12stat.df$mean[mm]+10, 0.01)
  # dens = dnorm(xseq, sy.ens12stat.df$mean[mm], sy.ens12stat.df$sd[mm])
  # lines(xseq, dens, col="red")
  # dev.off()
  #
  # qqplottitle = paste("Ensemble SRI12 Normal Q-Q Plot at", bsnname, "on", mm)
  # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri12", paste0(qqplottitle,".jpeg")))
  # qqnorm(as.vector(temp_enssri.mm), main=qqplottitle)
  # qqline(as.vector(temp_enssri.mm), col="red")
  # dev.off()
  # dev.off()
  # print(chisq.test(as.vector(temp_enssri.mm), pnorm(as.vector(temp_enssri.mm), ensstat.df$mean[mm], 1)))
  # print(ks.test(as.vector(temp_enssri.mm), "pnorm", mean=ensstat.df$mean[mm], sd=ensstat.df$sd[mm]))
  print(shapiro.test(as.vector(temp_enssri.mm)))
}
a = shapiro.test(as.vector(temp_enssri.mm))
# dc.ens12stat.df = as.data.frame(matrix(NA, ncol=3, nrow=12)) %>% setNames(., c("mean", "sd", "skewness"))
# bsnname = "daecheong"
# for (mm in c(1:12)){
#   temp_enssri.mm = which(month(temp_enssri.df)==mm) %>% temp_enssri.df[.]
#   dc.ens12stat.df$mean[mm] = mean(as.vector(temp_enssri.mm))
#   dc.ens12stat.df$sd[mm] = sd(as.vector(temp_enssri.mm))
#   dc.ens12stat.df$skewness[mm] = skewness(as.vector(temp_enssri.mm))
#   # temp_emp = rank(as.vector(temp_enssri.mm))/(length(as.vector(temp_enssri.mm))+1)
#   
#   # histtitle = paste("Ensemble SRI12 at" , bsnname, "on", mm)
#   # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri12", paste0(histtitle,".jpeg")))
#   # 
#   # hist(as.vector(temp_enssri.mm), probability = T,main=histtitle, xlab="SRI")
#   # xseq = seq(dc.ens12stat.df$mean[mm]-10, dc.ens12stat.df$mean[mm]+10, 0.01)
#   # dens = dnorm(xseq, dc.ens12stat.df$mean[mm], dc.ens12stat.df$sd[mm])
#   # lines(xseq, dens, col="red")
#   # dev.off()
#   # 
#   # qqplottitle = paste("Ensemble SRI12 Normal Q-Q Plot at", bsnname, "on", mm)
#   # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri12", paste0(qqplottitle,".jpeg")))
#   # qqnorm(as.vector(temp_enssri.mm), main=qqplottitle)
#   # qqline(as.vector(temp_enssri.mm), col="red")
#   # dev.off()
#   # dev.off()
#   # print(chisq.test(as.vector(temp_enssri.mm), pnorm(as.vector(temp_enssri.mm), ensstat.df$mean[mm], 1)))
#   # print(ks.test(as.vector(temp_enssri.mm), "pnorm", mean=ensstat.df$mean[mm], sd=ensstat.df$sd[mm]))
# }

# dc.ens3stat.df = as.data.frame(matrix(NA, ncol=3, nrow=12)) %>% setNames(., c("mean", "sd", "skewness"))
# bsnname = "daecheong"
# for (mm in c(1:12)){
#   temp_enssri.mm = which(month(temp_enssri.df)==mm) %>% temp_enssri.df[.]
#   dc.ens3stat.df$mean[mm] = mean(as.vector(temp_enssri.mm))
#   dc.ens3stat.df$sd[mm] = sd(as.vector(temp_enssri.mm))
#   dc.ens3stat.df$skewness[mm] = skewness(as.vector(temp_enssri.mm))
#   # temp_emp = rank(as.vector(temp_enssri.mm))/(length(as.vector(temp_enssri.mm))+1)
# 
#   # histtitle = paste("Ensemble SRI3 at" , bsnname, "on", mm)
#   # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri3", paste0(histtitle,".jpeg")))
#   # 
#   # hist(as.vector(temp_enssri.mm), probability = T,main=histtitle, xlab="SRI")
#   # xseq = seq(dc.ens3stat.df$mean[mm]-10, dc.ens3stat.df$mean[mm]+10, 0.01)
#   # dens = dnorm(xseq, dc.ens3stat.df$mean[mm], dc.ens3stat.df$sd[mm])
#   # lines(xseq, dens, col="red")
#   # dev.off()
#   # 
#   # qqplottitle = paste("Ensemble SRI3 Normal Q-Q Plot at", bsnname, "on", mm)
#   # jpeg(file=file.path(ensfilepath, "normality", bsnname, "sri3", paste0(qqplottitle,".jpeg")))
#   # qqnorm(as.vector(temp_enssri.mm), main=qqplottitle)
#   # qqline(as.vector(temp_enssri.mm), col="red")
#   # dev.off()
#   # dev.off()
#   # print(chisq.test(as.vector(temp_enssri.mm), pnorm(as.vector(temp_enssri.mm), ensstat.df$mean[mm], 1)))
#   # print(ks.test(as.vector(temp_enssri.mm), "pnorm", mean=ensstat.df$mean[mm], sd=ensstat.df$sd[mm]))
# }
# shapiro.test(as.vector(temp_enssri.mm))
# summary(tresult)

