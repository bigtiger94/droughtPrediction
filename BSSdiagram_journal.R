### draw heatmap
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = ""
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
periods = c("all", "irrigation", "non-irrigation")

bssfilepath = file.path("./predictResult", "summary")

period ="all"
for (bsncase in c(1:length(bsncodes))){
  bsnname = bsnnames[bsncase]
  
  accfname = paste0("acctable_apcc_",period, "_", dindexname, "_", bsnname, ".xlsx")
  acc.df = read_xlsx(file.path(bssfilepath, accfname))
  if (bsncase==1) {
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% data.frame(BSN=toupper(bsnname), .)
    # RPSS_irr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation)
    # RPSS_nonirr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation)
  }
  else{
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% 
      data.frame(BSN=toupper(bsnname), .) %>%  rbind(BSS_all.df, .)
    # RPSS_irr.df = rbind(RPSS_irr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation))
    # RPSS_nonirr.df = rbind(RPSS_nonirr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation))
  }
}


BSS_all.df$Case[BSS_all.df$Case == "EDP+"] = "EDP+S"
BSS_all.df$BSS[BSS_all.df$BSS == "-Inf"] = NaN
BSS_all.df$BSS = as.numeric(BSS_all.df$BSS)
BSS_all.df$BSS[BSS_all.df$BSS <(-1)] = -1
Dphase = unique(BSS_all.df$Phase)
for(phase in Dphase){
BSSheatmap = BSS_all.df %>% dplyr::filter(Phase==phase) %>%
  ggplot(., aes(x=BSN, y=Case)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") + xlab("")+
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(unique(BSS_all.df$Case))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.text.x = element_text(angle = 45, hjust = 0))

imgsavepath = file.path("./predictResult", "BSanal", "BSS heatmap")
# ggsave(BSSheatmap, filename = paste0("BSSheatmap_",phase,"_",period,"_",dindexname, ".png"), path=imgsavepath,
#        width = 18, height = 12, units = "cm")
}
BSS_all.df %>% aggregate(., list(BSS_all.df$Case), mean, na.rm=T)

period ="irrigation"
for (bsncase in c(1:length(bsncodes))){
  bsnname = bsnnames[bsncase]
  
  accfname = paste0("acctable_apcc_",period, "_", dindexname, "_", bsnname, ".xlsx")
  acc.df = read_xlsx(file.path(bssfilepath, accfname))
  if (bsncase==1) {
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% data.frame(BSN=toupper(bsnname), .)
    # RPSS_irr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation)
    # RPSS_nonirr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation)
  }
  else{
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% 
      data.frame(BSN=toupper(bsnname), .) %>%  rbind(BSS_all.df, .)
    # RPSS_irr.df = rbind(RPSS_irr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation))
    # RPSS_nonirr.df = rbind(RPSS_nonirr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation))
  }
}

BSS_all.df$Case[BSS_all.df$Case == "EDP+"] = "EDP+S"
BSS_all.df$BSS[BSS_all.df$BSS == "-Inf"] = NaN

BSS_all.df$BSS = as.numeric(BSS_all.df$BSS)
BSS_all.df$BSS[BSS_all.df$BSS <(-1)] = -1


BSS_all.df %>% aggregate(., list(BSS_all.df$Case), mean, na.rm=T)

Dphase = unique(BSS_all.df$Phase)
for(phase in Dphase){
  BSSheatmap = BSS_all.df %>% dplyr::filter(Phase==phase) %>%
    ggplot(., aes(x=BSN, y=Case)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") + xlab("")+
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(unique(BSS_all.df$Case))) +
    scale_fill_gradientn(colours=c("red", "white", "blue"), 
                         limits = c(-1, 1),
                         breaks =c(-1, 0, 1),
                         labels=c(-1, 0, 1)) +
    guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
    theme(axis.text = element_text(color="black", face='bold', size=10),
          axis.text.x = element_text(angle = 45, hjust = 0))
  
  imgsavepath = file.path("./predictResult", "BSanal", "BSS heatmap")
  # ggsave(BSSheatmap, filename = paste0("BSSheatmap_",phase,"_",period,"_",dindexname, ".png"), path=imgsavepath,
  #        width = 18, height = 12, units = "cm")
}


period ="non-irrigation"
for (bsncase in c(1:length(bsncodes))){
  bsnname = bsnnames[bsncase]
  
  accfname = paste0("acctable_apcc_",period, "_", dindexname, "_", bsnname, ".xlsx")
  acc.df = read_xlsx(file.path(bssfilepath, accfname))
  if (bsncase==1) {
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% data.frame(BSN=toupper(bsnname), .)
    # RPSS_irr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation)
    # RPSS_nonirr.df = data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation)
  }
  else{
    BSS_all.df = acc.df %>% dplyr::select(Phase, Case, BSS) %>% 
      data.frame(BSN=toupper(bsnname), .) %>%  rbind(BSS_all.df, .)
    # RPSS_irr.df = rbind(RPSS_irr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$irrigation))
    # RPSS_nonirr.df = rbind(RPSS_nonirr.df, data.frame(BSN = toupper(bsnname), Case = RPSS_bsn$GROUP, RPSS = RPSS_bsn$non.irrigation))
  }
}

BSS_all.df$Case[BSS_all.df$Case == "EDP+"] = "EDP+S"
BSS_all.df$BSS[BSS_all.df$BSS == "-Inf"] = NaN

BSS_all.df$BSS = as.numeric(BSS_all.df$BSS)
BSS_all.df$BSS[BSS_all.df$BSS <(-1)] = -1
BSS_all.df %>% aggregate(., list(BSS_all.df$Case), mean, na.rm=T)


Dphase = unique(BSS_all.df$Phase)
for(phase in Dphase){
  BSSheatmap = BSS_all.df %>% dplyr::filter(Phase==phase) %>%
    ggplot(., aes(x=BSN, y=Case)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") + xlab("")+
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(unique(BSS_all.df$Case))) +
    scale_fill_gradientn(colours=c("red", "white", "blue"), 
                         limits = c(-1, 1),
                         breaks =c(-1, 0, 1),
                         labels=c(-1, 0, 1)) +
    guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
    theme(axis.text = element_text(color="black", face='bold', size=10),
          axis.text.x = element_text(angle = 45, hjust = 0))
  
  imgsavepath = file.path("./predictResult", "BSanal", "BSS heatmap")
  ggsave(BSSheatmap, filename = paste0("BSSheatmap_",phase,"_",period,"_",dindexname, ".png"), path=imgsavepath,
         width = 18, height = 12, units = "cm")
}
