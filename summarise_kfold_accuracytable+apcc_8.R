# load prediction data ----------------------------------------------------
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
kfoldinfo = read.csv("kfoldinfo.csv", header=T, row.names=1)
kk=4
kfold = paste0("k", kk)

bsncase = 2
for (bsncase in c(1:length(bsnnames))){
     targetbsn = bsncodes[bsncase];
     bsnname = bsnnames[bsncase];
     
     obs.freq = pnorm(c(0, -1, -1.5, 2))
     
     acc.df = read.csv(file.path("./predictResult", paste0("accuracytable_", dindexname,"_", bsnname,".csv")), row.names=1)
     dphase = as.vector(unique(acc.df$D))
     period = "all"
     for (period in c("all", "irrigation", "non-irrigation")){
        sumdf = c()
        for (phase in dphase){
            obs.freq.df = data.frame(as.list(obs.freq)) %>% setNames(., dphase)
            temp.acc.df = acc.df %>% dplyr::filter(D==phase, Period==period) %>% mutate(GROUP = paste0(Enstype, Predicttype))
            temp.sumdf = summarise_each(group_by(temp.acc.df,GROUP), funs(mean), REL, RES, UNC, bs, BSS, RPS) %>% 
                       cbind(data.frame(phase), .) %>% setNames(., c("D", "GROUP", "REL", "RES", "UNC", "BS", "BSS", "RPS"))
            sumdf = rbind(sumdf, temp.sumdf)
     }
     write.csv(sumdf, file.path("./predictResult", paste0("accuracytable_summarised_",period,"_",dindexname,"_",bsnname,".csv")))
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
