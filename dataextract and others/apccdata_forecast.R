
bsnnames = c("soyang", "daecheong", "andong", "sumjin", "chungju", "hapcheon")
bsncodes = c(1012, 3008, 2001, 4001, 1003, 2015)
bsncase = 1
targetbsn = bsncodes[bsncase];
bsnname = bsnnames[bsncase];


# install.packages("fields")
# install.packages("ncdf4")
library(fields)
library(ncdf4)
library(dplyr)

forecastfilepath = file.path("./observations/apcc_nc/forecastdata")

flist = list.files(forecastfilepath);
flist_idx = grep("*.nc$", flist)
for (ii in flist_idx){print(ii)}
lat_rng = c(34, 40)
lon_rng = c(125, 130)

for (ii in flist_idx){
  ii=111
  fname = flist[ii]
  
  f = nc_open(file.path(forecastfilepath, fname))
  
  timestep = ncvar_get(f, "time") %>% as.Date(., origin = "1900-01-01", tz="UTC" )
  lat = ncvar_get(f, "lat"); 
  lon = ncvar_get(f, "lon"); 
  lat_ind = which(lat>=lat_rng[1] & lat<= lat_rng[2])
  lon_ind = which(lon>=lon_rng[1] & lon<= lon_rng[2])
  # lat[lat_ind]
  # lon[lon_ind]
  # lon_ind
  
  # smi = ncvar_get(f, "smi_w")
  # image.plot(smi[,,5])
  
  temp_prec = ncvar_get(f, "prec", start=c(lon_ind[1], lat_ind[1], 1, 1), count=c(-1, -1, -1, -1))
  # temp_prec = ncvar_get(f, "prec", start=c(1, 1, 4, 1), count=c(length(lon), length(lat), 1, -1))
  # [lon, lat, level, time] 
  # level = [above, normal, below, total] --> (above=total>100) (below=total<0)
  # 
  # prec[2,2] --> 강원(소양강); prec[2,1] --> 충청(대청) 
  # 
  # image.plot(temp_prec[,,1,3]) 
  temptime = ymd(paste(year(timestep[1]-16), month(timestep[1]-16), 1, sep = "-"))
  temppredicttime = timestep[1]-14
  
  
  # tempsypr = temp_prec[2,2,1:3,1] %>% setNames(., c("above", "normal", "below"))
  # tempdcpr = as.numeric((temp_prec[2,1,1:3,1]+temp_prec[2,2,1:3,2])/2) %>% setNames(., c("above", "normal", "below"))
  # tempadpr = as.numeric((temp_prec[2,1,1:3,1]+temp_prec[3,1,1:3,2]+temp_prec[2,2,1:3,2]+temp_prec[3,2,1:3,2])/4) %>%
  #              setNames(., c("above", "normal", "below"))
  # tempsjpr = temp_prec[2,1,1:3,1] %>% setNames(., c("above", "normal", "below"))
  # tempcjpr = temp_prec[2,2,1:3,1] %>% setNames(., c("above", "normal", "below"))
  temphcpr = temp_prec[2,1,1:3,1] %>% setNames(., c("above", "normal", "below"))
  # temp_df.prf.sy = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(tempsypr))
  # temp_df.prf.dc = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(tempdcpr))
  # temp_df.prf.ad = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(tempadpr))
  # temp_df.prf.sj = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(tempsjpr))
  # temp_df.prf.cj = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(tempcjpr))
  temp_df.prf.hc = data.frame(Time=temptime, Predicttime=temppredicttime, as.list(temphcpr))
  
  if (ii == 1){
    # df.prf.sy = temp_df.prf.sy
    # df.prf.dc = temp_df.prf.dc
    # df.prf.ad = temp_df.prf.ad
    # df.prf.sj = temp_df.prf.sj
    # df.prf.cj = temp_df.prf.cj
    df.prf.hc = temp_df.prf.hc
  } else{
    # df.prf.sy = rbind(df.prf.sy, temp_df.prf.sy)
    # df.prf.dc = rbind(df.prf.dc, temp_df.prf.dc)
    # df.prf.ad = rbind(df.prf.ad, temp_df.prf.ad)
    # df.prf.sj = rbind(df.prf.sj, temp_df.prf.sj)
    # df.prf.cj = rbind(df.prf.cj, temp_df.prf.cj)
    df.prf.hc = rbind(df.prf.hc, temp_df.prf.hc)
  }

}
# 
#
# write.csv(df.prf.sy, file.path(forecastfilepath,"probprforecast_soyang.csv"), row.names=F)
# write.csv(df.prf.dc, file.path(forecastfilepath,"probprforecast_daecheong.csv"), row.names=F)
# write.csv(df.prf.ad, file.path(forecastfilepath,"probprforecast_andong.csv"), row.names=F)
# write.csv(df.prf.sj, file.path(forecastfilepath,"probprforecast_sumjin.csv"), row.names=F)
# write.csv(df.prf.cj, file.path(forecastfilepath,"probprforecast_chungju.csv"), row.names=F)
write.csv(df.prf.hc, file.path(forecastfilepath,"probprforecast_hapcheon.csv"), row.names=F)
