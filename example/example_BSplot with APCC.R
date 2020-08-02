dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015)
kk=1
kfold = paste0("k", kk)

kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)

bsncase = 5
for (bsncase in c(1:6)){
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];

obs.freq = pnorm(c(0, -1, -1.5, 2))

BS.df = read.csv(file.path("./predictResult", "BSresult", paste0("BS_apcc_allperiod_", dindexname,"_", bsnname,".csv")), row.names=1)
dphase = as.vector(unique(BS.df$D))

for (period in c("all", "irrigation", "non-irrigation")){

obs.freq.df = data.frame(as.list(obs.freq)) %>% setNames(., dphase)
temp.BS.df = BS.df %>% dplyr::filter(K==kfold & Period==period & Predicttype=="prob") %>%
     dplyr::select(D, Enstype, Predicttype, bs, BSS) %>% mutate(GROUP = paste0(Enstype,Predicttype))
plotname = paste("Brier score for", dindexname, "with APCC at", bsnname)
jpeg(file.path("./predictResult", "BSresult", paste0(plotname,"_",period,".jpeg")))
# win.graph()
ylimend = 0.2
if(dindexname == "sri12") ylimend = 0.1

plot(obs.freq*(1-obs.freq), type="l", lty=2, lwd=2,
     main=plotname, xlab="Phase", ylab="BS", ylim=c(0,ylimend), xaxt="n")
temp.bs = temp.BS.df %>% dplyr::filter(Enstype=="up") %>% dplyr::select(bs)
lines(c(1:4), temp.bs$bs, col="red", lwd = 2)
temp.bs = temp.BS.df %>% dplyr::filter(Enstype=="raw") %>% dplyr::select(bs)
lines(c(1:4), temp.bs$bs, col="blue", lwd = 1)

temp.bs = temp.BS.df %>% dplyr::filter(Enstype=="apccup") %>% dplyr::select(bs)
lines(c(1:4), temp.bs$bs, col="green", lwd = 2)
temp.bs = temp.BS.df %>% dplyr::filter(Enstype=="apccraw") %>% dplyr::select(bs)
lines(c(1:4), temp.bs$bs, col="green", lwd = 1, lty=2)
axis(1, at=c(1:4), labels = dphase)
grid()

legend("topright", legend=c("Clm.", "EDP (PP)", "EDP (PP)", "EDP with APCC (PP)", "EDP+ with APCC(PP)"),
       lty=c(2,1,1,2,1), col=c("black", "blue", "red", "green", "green"))

dev.off()
}
}
# 
# obsxtick = as.vector(unique(temp.BS.df$D))
# 
# 
# BSgraph = temp.BS.df %>% 
#      ggplot(aes(x=D, y=bs, group=GROUP))+
#      geom_line(aes(colour = Enstype)) + 
#      geom_point(size=3, aes(shape=Predicttype, colour=Enstype)) +
#      scale_colour_manual(values = c("blue", "red")) +
#      labs(title=bsnname, y="BS", x="Phase")
# BSgraph = BSgraph + 
#      geom_segment(aes(y=obs.freq[1]*(1-obs.freq[1]), x=obsxtick[1], yend=obs.freq[2]*(1-obs.freq[2]), xend=obsxtick[2]), linetype="dashed")+
#      geom_segment(aes(y=obs.freq[2]*(1-obs.freq[2]), x=obsxtick[2], yend=obs.freq[3]*(1-obs.freq[3]), xend=obsxtick[3]), linetype="dashed")+
#      geom_segment(aes(y=obs.freq[3]*(1-obs.freq[3]), x=obsxtick[3], yend=obs.freq[4]*(1-obs.freq[4]), xend=obsxtick[4]), linetype="dashed")
#      
# BSgraph + theme(legend.title=element_blank())
