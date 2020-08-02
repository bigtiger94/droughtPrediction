apccfilepath = file.path("./observations/apcc_nc")

# install.packages("fields")
# install.packages("ncdf4")
# library(fields)
# library(ncdf4)


## smi
list.files(apccfilepath)
fname = "weeklySMI1.nc"

f = nc_open(file.path(apccfilepath, fname))

t.smi = ncvar_get(f, "time") %>% as.Date(., origin = "2000-01-01", tz="UTC" )
lat = ncvar_get(f, "lat"); 
lon = ncvar_get(f, "lon"); 
lat_rng = c(34, 39)
lon_rng = c(125, 130)
lat_ind = which(lat>=lat_rng[1] & lat<= lat_rng[2])
lon_ind = which(lon>=lon_rng[1] & lon<= lon_rng[2])
# lat[lat_ind]
# lon[lon_ind]
# lon_ind

# smi = ncvar_get(f, "smi_w")
# image.plot(smi[,,5])
smi_whole = ncvar_get(f, "smi_w")
smi = ncvar_get(f, "smi_w", start=c(lon_ind[1], lat_ind[1], 1), count=c(length(lon_ind), length(lat_ind), -1))
# smi[4,4] --> 강원(소양강, 충주); smi[3,3] --> 충청(대청) 
# smi[3,2] --> 전라(섬진강); smi[4,3] --> 경북; smi[4,2] --> 경남
image.plot(smi_whole[,,5], col=designer.colors(500, c("brown3", "white", "darkblue"), x=c(0, 0.01, 0.02)))

win.graph()
image.plot(smi[,,5], col=designer.colors(500, c("brown3", "white", "darkblue"), x=c(0, 0.01, 0.02)))
smi1012.ts = xts(smi[4,4,], t.smi)
smi3008.ts = xts(smi[3,3,], t.smi)
smi1003.ts = as.numeric((smi[3,3,]+smi[3,4,]+smi[4,3,]+smi[4,4,])/4) %>% xts(., t.smi)
smi4001.ts = xts(smi[3,2,], t.smi)
smi2001.ts = xts(smi[4,3,], t.smi)
smi2015.ts = as.numeric((smi[3,2,]+smi[4,2,])/2) %>% xts(., t.smi)

# write.csv(as.data.frame(smi1012.ts), file.path(apccfilepath, "SMI_soyang.csv"), row.names=T)
# write.csv(as.data.frame(smi3008.ts), file.path(apccfilepath, "SMI_daecheong.csv"), row.names=T)
# write.csv(as.data.frame(smi2001.ts), file.path(apccfilepath, "SMI_andong.csv"), row.names=T)
# write.csv(as.data.frame(smi4001.ts), file.path(apccfilepath, "SMI_sumjin.csv"), row.names=T)
# write.csv(as.data.frame(smi1003.ts), file.path(apccfilepath, "SMI_chungju.csv"), row.names=T)
write.csv(as.data.frame(smi2015.ts), file.path(apccfilepath, "SMI_hapcheon.csv"), row.names=T)

plot(smi1012.ts, main="smi at 강원(소양강)")
plot(smi3008.ts, main="smi at 충청(대청)")

# smi1012.ts = subset(smi1012.ts, year(smi1012.ts)<2018) %>% to.monthly(, indexAt = "close")

# ## SEDI
# 
# list.files(apccfilepath)
# fname = "SEDI1.nc"
# 
# f = nc_open(file.path(apccfilepath, fname))
# 
# t = ncvar_get(f, "time") %>% as.Date(., origin = "1900-01-01", tz="UTC" )
# lat = ncvar_get(f, "lat"); 
# lon = ncvar_get(f, "lon"); 
# 
# sedi = ncvar_get(f, "SEDI")
# image.plot(sedi[,,5])
# lat_rng = c(34, 39)
# lon_rng = c(125, 130)
# lat_ind = which(lat>=lat_rng[1] & lat<= lat_rng[2])
# lon_ind = which(lon>=lon_rng[1] & lon<= lon_rng[2])
# 
# 
# sedi = ncvar_get(f, "SEDI", start=c(lon_ind[1], lat_ind[1], 1), count=c(length(lon_ind), length(lat_ind), -1))
# # sedi[2,2] --> 남한
# image.plot(sedi[,,5]) 
# sedi.ts = xts(sedi[2,2,], t)
# plot(sedi.ts, main="sedi at 남한")
# 
# 
# 
