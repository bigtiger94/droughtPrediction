# classifying drought phase and computing the probability from ensemble
sdi.thl = c(0, -1, -1.5, -2) # threhold from NOAA and GIDMaPS
thlname = c("No", "D0", "D1", "D2", "D3")
prob.sri.dphase.over = data.frame()
prob.sri.dphase.each = data.frame()
obs.sri.dphase = as.data.frame(matrix(0, nrow=length(ensmean.sri.ts), ncol=1+length(sdi.thl)))
ensmean.sri.dphase = as.data.frame(matrix(0, nrow=length(ensmean.sri.ts), ncol=1+length(sdi.thl)))

for (jj in c(1:nrow(ens.sri.mat))){
  # probability of occurrence over threshold from ensemble
  temp.prob.dphase.over = c()
  temp.prob.dphase.over[1] = sum(ens.sri.mat[jj,]>sdi.thl[1])/nesp
  temp.prob.dphase.over[2] = sum(ens.sri.mat[jj,]<=sdi.thl[1])/nesp
  temp.prob.dphase.over[3] = sum(ens.sri.mat[jj,]<=sdi.thl[2])/nesp
  temp.prob.dphase.over[4] = sum(ens.sri.mat[jj,]<=sdi.thl[3])/nesp
  temp.prob.dphase.over[5] = sum(ens.sri.mat[jj,]<=sdi.thl[4])/nesp
  temp.prob.dphase.over[is.na(temp.prob.dphase.over)] = 0
  prob.sri.dphase.over = rbind(prob.sri.dphase.over, temp.prob.dphase.over)
  
  # probability of occurrence for each phase from ensemble
  temp.prob.dphase.each = c()
  temp.prob.dphase.each[1] = sum(ens.sri.mat[jj,]>sdi.thl[1])/nesp
  temp.prob.dphase.each[2] = sum(ens.sri.mat[jj,]<=sdi.thl[1]&ens.sri.mat[jj,]>sdi.thl[2])/nesp
  temp.prob.dphase.each[3] = sum(ens.sri.mat[jj,]<=sdi.thl[2]&ens.sri.mat[jj,]>sdi.thl[3])/nesp
  temp.prob.dphase.each[4] = sum(ens.sri.mat[jj,]<=sdi.thl[3]&ens.sri.mat[jj,]>sdi.thl[4])/nesp
  temp.prob.dphase.each[5] = sum(ens.sri.mat[jj,]<=sdi.thl[4])/nesp
  temp.prob.dphase.each[is.na(temp.prob.dphase.each)] = 0
  prob.sri.dphase.each = rbind(prob.sri.dphase.each, temp.prob.dphase.each)
  
  # drought phase from ensemble mean
  if(ensmean.sri.ts[jj]>sdi.thl[1]) {
    ensmean.sri.dphase[jj,1] = 1
  }else if(ensmean.sri.ts[jj]<=sdi.thl[1]&ensmean.sri.ts[jj]>sdi.thl[2]){
    ensmean.sri.dphase[jj,2] = 1
  }else if(ensmean.sri.ts[jj]<=sdi.thl[2]&ensmean.sri.ts[jj]>sdi.thl[3]){
    ensmean.sri.dphase[jj,3] = 1
  }else if(ensmean.sri.ts[jj]<=sdi.thl[3]&ensmean.sri.ts[jj]>sdi.thl[4]){
    ensmean.sri.dphase[jj,4] = 1
  }else{
    ensmean.sri.dphase[jj,5] = 1
  }
  # drought phase from observation
  if(sri.ts.clip[jj]>sdi.thl[1]) {
    obs.sri.dphase[jj,1] = 1
  }else if(sri.ts.clip[jj]<=sdi.thl[1]&sri.ts.clip[jj]>sdi.thl[2]){
    obs.sri.dphase[jj,2] = 1
  }else if(sri.ts.clip[jj]<=sdi.thl[2]&sri.ts.clip[jj]>sdi.thl[3]){
    obs.sri.dphase[jj,3] = 1
  }else if(sri.ts.clip[jj]<=sdi.thl[3]&sri.ts.clip[jj]>sdi.thl[4]){
    obs.sri.dphase[jj,4] = 1
  }else{
    obs.sri.dphase[jj,5] = 1
  }
}
colnames(prob.sri.dphase.each) = thlname; prob.sri.dphase.each = xts(prob.sri.dphase.each, date(sri.ts.clip));
colnames(prob.sri.dphase.over) = thlname; prob.sri.dphase.over = xts(prob.sri.dphase.over, date(sri.ts.clip));
colnames(obs.sri.dphase) = thlname; obs.sri.dphase = xts(obs.sri.dphase, date(sri.ts.clip));
colnames(ensmean.sri.dphase) = thlname; ensmean.sri.dphase = xts(ensmean.sri.dphase, date(sri.ts.clip));

tail(prob.sri.dphase.each, 12)



pathname = file.path("SRIresult",targetbsn)
if(!dir.exists(pathname)) {  dir.create(pathname)  }
write.csv(date(prob.sri.dphase.each), file.path(pathname, paste0("timestep.csv")), row.names=T)
pathname = file.path("SRIresult",targetbsn, paste0(indexname, timescale))
if(!dir.exists(pathname)){dir.create(pathname)}

write.csv(prob.sri.dphase.each, file.path(pathname, paste0("prob_each_ensSRI", timescale, ".csv")), row.names=T)
write.csv(prob.sri.dphase.over, file.path(pathname, paste0("prob_over_ensSRI", timescale, ".csv")), row.names=T)
write.csv(obs.sri.dphase, file.path(pathname, paste0("obs_SRI", timescale, ".csv")), row.names=T)
