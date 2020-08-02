gwfilepath =file.path("C:/Users/Daeho/Desktop/Research/연구자료/EDP/observations", "groundwater")
flist = list.files(gwfilepath)
sylist = grep("^soyang",flist)


syfname = flist[sylist]
for (ii in c(1:length(sylist))){
  xmldata = read_xml(file.path(gwfilepath, syfname[ii]))
  timestamp = ymd(xml_text(xml_find_all(xmldata, "//obsrdate")))
  groundwater = as.numeric(xml_text(xml_find_all(xmldata, "//wal")))
  obsnumber = unique(xml_text(xml_find_all(xmldata, "//mngnum")))
  obsregion = unique(xml_text(xml_find_all(xmldata, "//obsrvtnm")))
  temp_sy.groundwater.df = data.frame(Time=timestamp, obsnumber=groundwater) %>% setNames(., c("Time", obsnumber))
  
  if (ii==1){
    sy.groundwater.df = temp_sy.groundwater.df
  } else{
    sy.groundwater.df = merge(sy.groundwater.df, temp_sy.groundwater.df, by="Time", all=T)
  }
}
which(is.na(sy.groundwater.df[,2]))
sy.groundwater.df[34,2] = mean(sy.groundwater.df[c(33,35),2])
n = length(1116:1141)+1
sy.groundwater.df[1116:1141,2] = sy.groundwater.df[1115,2] + (1:(n-1))*(sy.groundwater.df[1142,2]-sy.groundwater.df[1115,2])/n
n = length(1263:1267)+1
sy.groundwater.df[1263:1267,2] = sy.groundwater.df[1262,2] + (1:(n-1))*(sy.groundwater.df[1268,2]-sy.groundwater.df[1262,2])/n

which(is.na(sy.groundwater.df[,3]))
n = length(1116:1127)+1
sy.groundwater.df[1116:1127,3] = sy.groundwater.df[1115,3] + (1:(n-1))*(sy.groundwater.df[1128,3]-sy.groundwater.df[1115,3])/n

which(is.na(sy.groundwater.df[,4]))
n = length(1116:1127)+1
sy.groundwater.df[1116:1127,4] = sy.groundwater.df[1115,4] + (1:(n-1))*(sy.groundwater.df[1128,4]-sy.groundwater.df[1115,4])/n
n = length(618:624)+1
sy.groundwater.df[618:624,4] = sy.groundwater.df[617,4] + (1:(n-1))*(sy.groundwater.df[625,4]-sy.groundwater.df[617,4])/n
sy.groundwater.df[1237,4] = mean(sy.groundwater.df[c(1236,1238),4])
sy.groundwater.df[2729,4] = mean(sy.groundwater.df[c(2728,2730),4])
sy.groundwater.df[2731,4] = mean(sy.groundwater.df[c(2732,2730),4])
sy.groundwater.df[3260,4] = mean(sy.groundwater.df[c(3259,3261),4])
n = length(3314:3315)+1
sy.groundwater.df[3314:3315,4] = sy.groundwater.df[3313,4] + (1:(n-1))*(sy.groundwater.df[3316,4]-sy.groundwater.df[3313,4])/n
n = length(3314:3315)+1
sy.groundwater.df[3314:3315,4] = sy.groundwater.df[3313,4] + (1:(n-1))*(sy.groundwater.df[3316,4]-sy.groundwater.df[3313,4])/n
n = length(3332:3336)+1
sy.groundwater.df[3332:3336,4] = sy.groundwater.df[3331,4] + (1:(n-1))*(sy.groundwater.df[3337,4]-sy.groundwater.df[3331,4])/n
sy.groundwater.df[3350,4] = mean(sy.groundwater.df[c(3349,3351),4])
sy.groundwater.df[3366,4] = mean(sy.groundwater.df[c(3365,3367),4])
sy.groundwater.df[3473,4] = mean(sy.groundwater.df[c(3472,3474),4])

which(is.na(sy.groundwater.df[,5]))
n = length(1260:1266)+1
sy.groundwater.df[1260:1266,5] = sy.groundwater.df[1259,5] + (1:(n-1))*(sy.groundwater.df[1267,5]-sy.groundwater.df[1259,5])/n
n = length(2796:2798)+1
sy.groundwater.df[2796:2798,5] = sy.groundwater.df[2795,5] + (1:(n-1))*(sy.groundwater.df[2799,5]-sy.groundwater.df[2795,5])/n
which(is.na(sy.groundwater.df))

# outlier
plot(sy.groundwater.df[,2], type="l")
which(sy.groundwater.df[,2]<212.5)
sy.groundwater.df[260,2] = mean(sy.groundwater.df[c(259,261),2])
n = length(1173:1184)+1
sy.groundwater.df[1173:1184,2] = sy.groundwater.df[1172,2] + (1:(n-1))*(sy.groundwater.df[1185,2]-sy.groundwater.df[1172,2])/n
sy.groundwater.df[1953,2] = mean(sy.groundwater.df[c(1952,1954),2])
sy.groundwater.df[2840,2] = mean(sy.groundwater.df[c(2839,2841),2])
sy.groundwater.df[2128,2] = mean(sy.groundwater.df[c(2127,2129),2])
sy.groundwater.df[2490,2] = mean(sy.groundwater.df[c(2489,2491),2])

plot(sy.groundwater.df[,3], type="l")
plot(sy.groundwater.df[,1], sy.groundwater.df[,4], type="l")
which(sy.groundwater.df[,4]<190)
sy.groundwater.df[3489,4] = mean(sy.groundwater.df[c(3488, 3491),4])
sy.groundwater.df[3490,4] = mean(sy.groundwater.df[c(3488, 3491),4])
sy.groundwater.df[3511,4] = mean(sy.groundwater.df[c(3510, 3512),4])
which(sy.groundwater.df[,4]>196)
n = length(2729:2731)+1
sy.groundwater.df[2729:2731,4] = sy.groundwater.df[2728,4] + (1:(n-1))*(sy.groundwater.df[2732,4]-sy.groundwater.df[2728,4])/n

plot(sy.groundwater.df[,1], sy.groundwater.df[,5], type="l")
which(sy.groundwater.df[,5]>74)
n = length(1277:1317)+1
sy.groundwater.df[1277:1317,5] = sy.groundwater.df[1276,5] + (1:(n-1))*(sy.groundwater.df[1318,5]-sy.groundwater.df[1276,5])/n
sy.groundwater.df[417,5] = mean(sy.groundwater.df[c(416, 418),5])
write.csv(sy.groundwater.df, file.path(gwfilepath, "gw_obs_soyang.csv"), row.names=F)



## for daechong22 (청주현도)
dclist = grep("^daecheong",flist)
dcfname = flist[dclist]
for (ii in c(1:length(dclist))){
  xmldata = read_xml(file.path(gwfilepath, dcfname[ii]))
  timestamp = ymd(xml_text(xml_find_all(xmldata, "//obsrdate")))
  groundwater = as.numeric(xml_text(xml_find_all(xmldata, "//wal")))
  obsnumber = unique(xml_text(xml_find_all(xmldata, "//mngnum")))
  obsregion = unique(xml_text(xml_find_all(xmldata, "//obsrvtnm")))
  temp_dc.groundwater.df = data.frame(Time=timestamp, obsnumber=groundwater) %>% setNames(., c("Time", obsnumber))
  
  if (ii==1){
    dc.groundwater.df = temp_dc.groundwater.df
  } else{
    dc.groundwater.df = merge(dc.groundwater.df, temp_dc.groundwater.df, by="Time", all=T)
  }
}
which(is.na(dc.groundwater.df[,2]))
dc.groundwater.df[386,2] = mean(dc.groundwater.df[c(385,387),2])
n = length(623:624)+1
dc.groundwater.df[623:624,2] = dc.groundwater.df[622,2] + (1:(n-1))*(dc.groundwater.df[625,2]-dc.groundwater.df[622,2])/n
n = length(1115:1149)+1
dc.groundwater.df[1115:1149,2] = dc.groundwater.df[1114,2] + (1:(n-1))*(dc.groundwater.df[1150,2]-dc.groundwater.df[1114,2])/n
n = length(1229:1239)+1
dc.groundwater.df[1229:1239,2] = dc.groundwater.df[1228,2] + (1:(n-1))*(dc.groundwater.df[1240,2]-dc.groundwater.df[1228,2])/n
n = length(1284:1285)+1
dc.groundwater.df[1284:1285,2] = dc.groundwater.df[1283,2] + (1:(n-1))*(dc.groundwater.df[1286,2]-dc.groundwater.df[1283,2])/n
n = length(1362:1372)+1
dc.groundwater.df[1362:1372,2] = dc.groundwater.df[1361,2] + (1:(n-1))*(dc.groundwater.df[1373,2]-dc.groundwater.df[1361,2])/n
n = length(1677:1680)+1
dc.groundwater.df[1677:1680,2] = dc.groundwater.df[1676,2] + (1:(n-1))*(dc.groundwater.df[1681,2]-dc.groundwater.df[1676,2])/n
dc.groundwater.df[1981,2] = mean(dc.groundwater.df[c(1980,1982),2])

which(is.na(dc.groundwater.df[,3]))
n = length(678:680)+1
dc.groundwater.df[678:680,3] = dc.groundwater.df[677,3] + (1:(n-1))*(dc.groundwater.df[681,3]-dc.groundwater.df[677,3])/n
n = length(971:977)+1
dc.groundwater.df[971:977,3] = dc.groundwater.df[970,3] + (1:(n-1))*(dc.groundwater.df[978,3]-dc.groundwater.df[970,3])/n
n = length(1056:1061)+1
dc.groundwater.df[1056:1061,3] = dc.groundwater.df[1055,3] + (1:(n-1))*(dc.groundwater.df[1062,3]-dc.groundwater.df[1055,3])/n
n = length(1670:1671)+1
dc.groundwater.df[1670:1671,3] = dc.groundwater.df[1669,3] + (1:(n-1))*(dc.groundwater.df[1672,3]-dc.groundwater.df[1669,3])/n

which(is.na(dc.groundwater.df))
# outlier
plot(dc.groundwater.df[,2], type="l")
plot(sy.groundwater.df[,3], type="l")

# write.csv(dc.groundwater.df, file.path(gwfilepath, "gw_obs_daecheong.csv"), row.names=F)
