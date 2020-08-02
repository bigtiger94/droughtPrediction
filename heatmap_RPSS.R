### draw heatmap
dindexname = "sri3"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
bsncase = 1

imgsavepath = file.path("./predictResult", "RPSSanal")


for (bsncase in c(1:length(bsncodes))){
  bsnname = bsnnames[bsncase]
  RPSS_bsn = read.csv(file.path("./predictResult", paste0("RPSS_",dindexname,"_",bsnname,".csv")))
  
  if (bsncase==1) {
    RPSS_all.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$all)
    RPSS_irr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation)
    RPSS_nonirr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation)
  }  else{
    RPSS_all.df = rbind(RPSS_all.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$all))
    RPSS_irr.df = rbind(RPSS_irr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation))
    RPSS_nonirr.df = rbind(RPSS_nonirr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation))
  }
}

heatmapname = paste0("RPSS_all")
heatmapplot = ggplot(RPSS_all.df, aes(x=BSN, y=Case)) + geom_tile(aes(fill=RPSS)) + theme_bw() + ylab("") +ggtitle("")+xlab("")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(unique(RPSS_all.df$Case))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0))
ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname, ".png"), path=imgsavepath)

heatmapname = paste0("RPSS_irrigation")
heatmapplot = ggplot(RPSS_irr.df, aes(x=BSN, y=Case)) + geom_tile(aes(fill=RPSS)) + theme_bw() + ylab("") +ggtitle("")+xlab("")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(unique(RPSS_irr.df$Case))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0))
ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname, ".png"), path=imgsavepath)

heatmapname = paste0("RPSS_non-irrigation")
heatmapplot = ggplot(RPSS_nonirr.df, aes(x=BSN, y=Case)) + geom_tile(aes(fill=RPSS)) + theme_bw() + ylab("") +ggtitle("")+xlab("")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(unique(RPSS_nonirr.df$Case))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0))
ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname,".png"), path=imgsavepath)
RPSSmean_all = RPSS_all.df %>% group_by(Case) %>% summarise(., mean(RPSS))
RPSSmean_irr = RPSS_irr.df %>% group_by(Case) %>% summarise(., mean(RPSS))
RPSSmean_nonirr = RPSS_nonirr.df %>% group_by(Case) %>% summarise(., mean(RPSS))
# data.frame(All=RPSSmean_all$`mean(RPSS)`, Irrigation=RPSSmean_irr$`mean(RPSS)`, Nonirrigation=RPSSmean_nonirr$`mean(RPSS)`) %>%
  # 'rownames<-'(as.vector(RPSSmean_all$Case)) %>% 
  # write.csv(., file.path(imgsavepath, paste0("Avg_RPSS_",dindexname,".csv")))


# RPSS_all.df

# graphics.off()
# 