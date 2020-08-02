### draw heatmap
dindexname = "sri12"
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon", "namgang", "imha")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015, 2018, 2002)
apcctype = ""
period.irr = c(4:9)
period.nonirr = setdiff(c(1:12), period.irr)
periods = c("all", "irrigation", "non-irrigation")

bssfilepath = file.path("./predictResult", "summary")
imgsavepath = file.path("./predictResult", "BSanal", "BSS heatmap")
for (period in periods){

for (bsnname in bsnnames){
  
  accfname = paste0("acctable_",period, "_", dindexname, "_", bsnname, ".xlsx")
  
  acc.df = read_xlsx(file.path(bssfilepath, accfname))
  temp_acc.df = data.frame(acc.df, CLASS = paste(acc.df$Case,acc.df$Type), BSN = bsnname)
  # temp_acc.df$BSS[temp_acc.df$BSS< -1] = -1
  temp_acc.df$BSS[temp_acc.df$BSS == "-Inf"] = -1
  temp_acc.df$BSS[is.na(temp_acc.df$BSS)] = -1
  temp_acc.df$BSS = as.numeric(temp_acc.df$BSS)
  if (bsnname == "soyang") BSS.df = dplyr::select(temp_acc.df, BSN, CLASS, Phase, BSS)
  else BSS.df = rbind(BSS.df, dplyr::select(temp_acc.df, BSN, CLASS, Phase, BSS))
  
}
heatBSS.df = BSS.df %>% group_by(CLASS, Phase) %>% summarise(., mean(BSS)) %>% setNames(., c("CLASS", "Phase", "BSS"))

heatmapname = paste0("BSSmean ",period)

# jpeg(file.path(imgsavepath, paste0(heatmapname,"_",dindexname,"_", bsnname, ".jpg")))
# image.plot()

heatmapplot = ggplot(heatBSS.df, aes(x=Phase, y=CLASS)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") + xlab("") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(heatBSS.df$CLASS))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.title = element_text(size=10,face="bold"))

# ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname,".jpg"), path=imgsavepath)

}

for (period in periods){
for (bsnname in bsnnames){
  
  accfname = paste0("acctable_apcc_",period, "_", dindexname, "_", bsnname, ".xlsx")
  
  acc.df = read_xlsx(file.path(bssfilepath, accfname))
  temp_acc.df = data.frame(acc.df, Case=acc.df$Case, BSN = bsnname)
  # temp_acc.df$BSS[temp_acc.df$BSS< -1] = -1
  temp_acc.df$BSS[temp_acc.df$BSS == "-Inf"] = -1
  temp_acc.df$BSS[is.na(temp_acc.df$BSS)] = -1
  temp_acc.df$BSS = as.numeric(temp_acc.df$BSS)
  if (bsnname == "soyang") BSS.df = dplyr::select(temp_acc.df, BSN, Case, Phase, BSS)
  else BSS.df = rbind(BSS.df, dplyr::select(temp_acc.df, BSN, Case, Phase, BSS))
  
}
heatBSS.df = BSS.df %>% group_by(Case, Phase) %>% summarise(., mean(BSS)) %>% setNames(., c("Case", "Phase", "BSS"))
heatBSS.df$Case[heatBSS.df$Case == "EDP+"] = "EDP+S"
heatmapname = paste0("BSSmean_apcc ", period)

# jpeg(file.path(imgsavepath, paste0(heatmapname,"_",dindexname,"_", bsnname, ".jpg")))
# image.plot()

heatmapplot = ggplot(heatBSS.df, aes(x=Phase, y=Case)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") + xlab("") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(unique(heatBSS.df$Case))) +
  scale_fill_gradientn(colours=c("red", "white", "blue"), 
                       limits = c(-1, 1),
                       breaks =c(-1, 0, 1),
                       labels=c(-1, 0, 1)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  theme(axis.text = element_text(color="black", face='bold', size=10),
        axis.title = element_text(size=10,face="bold"))

ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname,".jpg"), path=imgsavepath)
}



### at each basin

for (bsnname in bsnnames){

     
     for (period in periods){
          accfname = paste0("acctable_",period, "_", dindexname, "_", bsnname, ".xlsx")
          
          acc.df = read_xlsx(file.path(bssfilepath, accfname))
          temp_acc.df = data.frame(acc.df, CLASS = paste(acc.df$Case,acc.df$Type))
          temp_acc.df$BSS[temp_acc.df$BSS< -1] = -1
          temp_acc.df$BSS[temp_acc.df$BSS == "-Inf"] = NaN
          temp_acc.df$BSS = as.numeric(temp_acc.df$BSS)
          # BSS_all.df = acc_all.df %>% acast(., Case+Type~Phase, value.var="BSS")
          
          heatmapname = paste0("BSS_", period)
          
          # jpeg(file.path(imgsavepath, paste0(heatmapname,"_",dindexname,"_", bsnname, ".jpg")))
          # image.plot()
          
          heatmapplot = ggplot(temp_acc.df, aes(x=Phase, y=CLASS)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") +
               scale_x_discrete(position = "top")+
               scale_y_discrete(limits = rev(levels(temp_acc.df$CLASS))) +
               scale_fill_gradientn(colours=c("red", "white", "blue"), 
                                    limits = c(-1, 1),
                                    breaks =c(-1, 0, 1),
                                    labels=c(-1, 0, 1)) +
               guides(fill = guide_colourbar(barwidth = 1, barheight = 20)) +
               theme(axis.text = element_text(color="black", face='bold', size=15),
                     axis.title = element_text(size=20,face="bold"))
          
          ggsave(heatmapplot, filename = paste0(heatmapname,"_",dindexname,"_", bsnname, ".jpg"), path=imgsavepath)
          
     }
}

for (bsnname in bsnnames){
     
     
     for (period in periods){
          accfname = paste0("acctable_apcc_",period, "_", dindexname, "_", bsnname, ".xlsx")
          
          acc.df = read_xlsx(file.path(bssfilepath, accfname))
          temp_acc.df = acc.df
          temp_acc.df$Case[temp_acc.df$Case=="EDP+"] = "EDP+S"
          temp_acc.df$BSS[temp_acc.df$BSS< -1] = -1
          temp_acc.df$BSS[temp_acc.df$BSS == "-Inf"] = NaN
          temp_acc.df$BSS = as.numeric(temp_acc.df$BSS)
          # BSS_all.df = acc_all.df %>% acast(., Case+Type~Phase, value.var="BSS")
          
          heatmapname = paste0("BSS_", period)
          
          # jpeg(file.path(imgsavepath, paste0(heatmapname,"_",dindexname,"_", bsnname, ".jpg")))
          # image.plot()
          
          heatmapplot = ggplot(temp_acc.df, aes(x=Phase, y=Case)) + geom_tile(aes(fill=BSS)) + theme_bw() + ylab("") +
               scale_x_discrete(position = "top")+
               scale_y_discrete(limits = rev(unique(temp_acc.df$Case))) +
               scale_fill_gradientn(colours=c("red", "white", "blue"), 
                                    limits = c(-1, 1),
                                    breaks =c(-1, 0, 1),
                                    labels=c(-1, 0, 1)) +
               guides(fill = guide_colourbar(barwidth = 1, barheight = 20)) +
               theme(axis.text = element_text(color="black", face='bold', size=15),
                     axis.title = element_text(size=20,face="bold"))
          
          ggsave(heatmapplot, filename = paste0(heatmapname,"_apcc_",dindexname,"_", bsnname, ".jpg"), path=imgsavepath)
          
     }
}


