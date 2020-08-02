setwd("..")
library(plyr)
library(lubridate)
library(dplyr)
library(xts)
library(ggplot2)
library(gridExtra)
library(ROCR)
library(fitdistrplus)
library(SpecsVerification)
library(fields)
library(ncdf4)
library(xml2)
library(fBasics)
library(caret)
library(RColorBrewer)
library(readxl)
# library(SCI)
# library(copula)
cal_drought_daily = function (inflow, threshold){
  # inflow = inflow data [cms]
  # threshold = reference streamflow of drought [cms]
  # duration = number of drought days
  # deficit = deficit amount [ton]
  
  ndays = length(inflow);
  tempdrought = inflow-as.numeric(threshold);
  duration = sum(tempdrought<0);
  deficit = -sum(tempdrought[tempdrought<0])*3600*24/1000; # m^3 = ton 
  
  return(c(duration, deficit))
}

cal_espdrought_monthly = function(bsn, Y, M, threshold, recstart, recend){ # return esp drought [dur, def]
  basinslist = readxl::read_excel("basin_numbers.xlsx")
  
  # read ESP of given bsn, year, month
  esplist = list.files("./esp1month_csv")
  espMMlist = esplist[intersect(grep("*_cms.csv$",esplist), which(substr(esplist, 9, 10)==sprintf("%02d", M)))]
    tempespname = espMMlist[grep(paste0("^ESP_",Y), espMMlist)]
  esp = read.csv(file.path("esp1month_csv", tempespname), header=F) %>% setNames(., c("Year", "Month", "Day", unlist(basinslist[,2])))
  
  # cut ESP to have same period with observation
  espyears = intersect(unique(esp$Year), c(recstart:recend))
  espdrought = matrix(0, nrow=length(espyears), ncol=2); rownames(espdrought) = espyears
  bsnidx = which(colnames(esp)==bsn); bsnidx = bsnidx[1]
  esp_tb = esp[,c(1:3, bsnidx)]
  
  for (eydx in c(1:length(espyears))){
    espy = espyears[eydx]
    tempesp = subset(esp_tb, esp_tb$Year == espy)
    espdrought[eydx,] = cal_drought_daily(tempesp[,4], threshold)
  }
  return(espdrought)
  
}

cal_empprob = function(x, p){
  n = length(x)
  k = (n+1)*p
  x_sorted = sort(as.vector(x))
  
  if(k<1){
    y = x_sorted(1);
  } else{
    k1 = floor(k);
    k2 = ceiling(k)
    y = (x_sorted[k2]-x_sorted[k1])*(k-k1) + x_sorted[k1]
  }
  return(y)
  
}

cal_mmref = function(inflowdata, refyyyy, M, p){
  # inflow = timeseries
  nrefY = length(refyyyy)
  q_p_m_yearly = matrix(0, nrow=nrefY)
  for (ii in c(1:nrefY)){
    tempinflowidx = which(year(inflowdata)==refyyyy[ii] & month(inflowdata)==M)
    tempinflow = inflowdata[tempinflowidx]
    q_p_m_yearly[ii] = cal_empprob(tempinflow, p)
  }
  return(mean(q_p_m_yearly))

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

cms2mm = function(CMS, area){
  # area = km^2
  # cms = m^3/s
  mm = CMS*3600*24*1000/(area*10^6)
  return(mm)
  
}

sdi.transform = function(prob){
  sdi = c()
  temp.t = c()
  c0 = 2.515517; c1 = 0.802583; c2 = 0.010328; d1 = 1.432788; d2 = 0.189369; d3 = 0.001308
  idx_under = which(prob<=0.5)
  idx_up = which(prob>0.5)
  
  temp.t[idx_under] = sqrt(log(1/(prob[idx_under])^2))
  temp.t[idx_up] = sqrt(log(1/(1-prob[idx_up])^2))
  
  sdi[idx_under] = -(temp.t[idx_under]-(c0+c1*temp.t[idx_under]+c2*temp.t[idx_under]^2) / (1+d1*temp.t[idx_under]+d2*temp.t[idx_under]^2+d3*temp.t[idx_under]^3))
  sdi[idx_up] = (temp.t[idx_up]-(c0+c1*temp.t[idx_up]+c2*temp.t[idx_up]^2) / (1+d1*temp.t[idx_up]+d2*temp.t[idx_up]^2+d3*temp.t[idx_up]^3))
  return(sdi)
}

calSGI = function(gw.mat){
  # gw_mat should have Nyear X 12(month) dimension
  gamaic = 0
  normaic = 0
  lnormaic = 0
  for (mm in 1:12){
    input = gw.mat[,mm]
    # pathname = file.path(getwd(), "SDIfit", paste0(indexname,"fit"))
    # if(!dir.exists(pathname)) {dir.create(paste0("SDIfit/", indexname,"fit"))}
    # pdf(file.path(pathname,paste0(indexname,"-",timescale,"on",ii,".pdf")), width=5, height=5)
    empprob = (c(1:length(input))-0.44)/(length(input)+0.12)
    # plot(empprob, sort(input), xlab="prob", ylab="groundwater")
    
    temp.gamfit = fitdist(input, 'gamma')
    # lines(seq(0, 1, 0.01), qgamma(seq(0, 1, 0.01), shape=temp.gamfit$estimate[1], rate=temp.gamfit$estimate[2]), col="red")
    temp.normfit = fitdist(input, "norm")
    # lines(seq(0, 1, 0.01), qnorm(seq(0, 1, 0.01), mean=temp.normfit$estimate[1], sd=temp.normfit$estimate[2]), col="green")
    temp.lnormfit = fitdist(input, "lnorm", method="mme")
    # lines(seq(0, 1, 0.01), qlnorm(seq(0, 1, 0.01), meanlog=temp.lnormfit$estimate[1], sdlog=temp.lnormfit$estimate[2]), col="blue")
    # legend("topleft", legend = c("obs", "gamma", "lnorm"), pch=c(1,-1,-1), lty=c(-1,1,1), col=c("black", "red", "blue"))
    gamaic = gamaic + temp.gamfit$aic
    lnormaic = lnormaic + temp.lnormfit$aic
    normaic = normaic + temp.normfit$aic
    
    # dev.off()
  }
  if (gamaic>lnormaic){
    print("lnorm")
    fitfunc = "lnorm"
    
  } else if (normaic>gamaic){
    print("gamma")
    fitfunc = "gamma"
    
  } else{
    print("norm")
    fitfunc = "norm"
  }
  
  
  # temp.distfit = fitdist(input, fitfunc, method="mme")
  evalname = paste0("p",fitfunc, "(input, temp.distfit$estimate[1], temp.distfit$estimate[2])")
  # temp.fitprob = eval(parse(text=evalname)) 
  # chisq.test(input, temp.fitprob)
  
  
  sgi = c()
  distparam.sgi = data.frame()
  for (mm in 1:12){
    input = gw.mat[,mm]
    temp.distfit = fitdist(input, fitfunc, method="mme")
    if (mm ==1){
      distparam.sgi = temp.distfit$estimate
    } else{
      distparam.sgi = rbind(distparam.sgi, temp.distfit$estimate)
    }
    # temp.prob = plnorm(input.cum[,mm], temp.distfit$estimate[1], temp.distfit$estimate[2])
    temp.fitprob = eval(parse(text=evalname))
    print(mm)
    print(chisq.test(input, temp.fitprob))
    temp.sgi = sdi.transform(temp.fitprob)
    sgi = cbind(sgi, temp.sgi)
  }
  
  distparam.sgi = as.data.frame(distparam.sgi, row.names = c(1:12))
  colnames(sgi) = as.character(c(1:12)); 
  rownames(sgi) = rownames(gw.mat)
  
  return(sgi)
  
}


calBS = function(f_prob, obs){
  BS = as.data.frame(BrierDecomp(f_prob, obs, bins=10))
  BS$bs = BS$REL - BS$RES + BS$UNC
  return(BS)
}

cal_contingency = function(prediction, observation){
     # prediction and observations are vectors which have same length
     temp.TP = sum(prediction*observation)
     temp.FP = sum(prediction*(1-observation))
     temp.FN = sum((1-prediction)*observation)
     
     return(data.frame(TP=temp.TP, FP=temp.FP, FN=temp.FN))
}

cal_PEV = function(CT, observation){
     COST = seq(0, 100, 1)
     LOSS = max(COST)
     n = length(observation)
     s = sum(observation)/n
     E1 = s*COST
     Ef = COST*(CT$TP[1]+CT$FP[1])/n + LOSS*CT$FN[1]/n
     E0 = data.frame(COST, rep(s*LOSS, length(COST))) %>% apply(., 1, min)
     PEV = (E0-Ef)/(E0-E1) 
     PEV[PEV<0] = 0
     PEV.df = data.frame(RATIO = COST/LOSS, PEV)
     return(PEV.df)
}
