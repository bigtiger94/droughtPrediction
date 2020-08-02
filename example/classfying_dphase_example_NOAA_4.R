# classifying drought phase and computing the probability from ensemble
sdi.thl = c(-0.5, -0.8, -1.3, -1.6, -2) # threhold from NOAA and GIDMaPS
thlname = c("No", "D0", "D1", "D2", "D3", "D4")
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
  temp.prob.dphase.over[6] = sum(ens.sri.mat[jj,]<=sdi.thl[5])/nesp
  prob.sri.dphase.over = rbind(prob.sri.dphase.over, temp.prob.dphase.over)
  
  # probability of occurrence for each phase from ensemble
  temp.prob.dphase.each = c()
  temp.prob.dphase.each[1] = sum(ens.sri.mat[jj,]>sdi.thl[1])/nesp
  temp.prob.dphase.each[2] = sum(ens.sri.mat[jj,]<=sdi.thl[1]&ens.sri.mat[jj,]>sdi.thl[2])/nesp
  temp.prob.dphase.each[3] = sum(ens.sri.mat[jj,]<=sdi.thl[2]&ens.sri.mat[jj,]>sdi.thl[3])/nesp
  temp.prob.dphase.each[4] = sum(ens.sri.mat[jj,]<=sdi.thl[3]&ens.sri.mat[jj,]>sdi.thl[4])/nesp
  temp.prob.dphase.each[5] = sum(ens.sri.mat[jj,]<=sdi.thl[4]&ens.sri.mat[jj,]>sdi.thl[5])/nesp
  temp.prob.dphase.each[6] = sum(ens.sri.mat[jj,]<=sdi.thl[5])/nesp
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
  }else if(ensmean.sri.ts[jj]<=sdi.thl[4]&ensmean.sri.ts[jj]>sdi.thl[5]){
    ensmean.sri.dphase[jj,5] = 1
  }else{
    ensmean.sri.dphase[jj,6] = 1
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
  }else if(sri.ts.clip[jj]<=sdi.thl[4]&sri.ts.clip[jj]>sdi.thl[5]){
    obs.sri.dphase[jj,5] = 1
  }else{
    obs.sri.dphase[jj,6] = 1
  }
}
colnames(prob.sri.dphase.each) = thlname; rownames(prob.sri.dphase.each) = date(sri.ts.clip)
colnames(prob.sri.dphase.over) = thlname; rownames(prob.sri.dphase.over) = date(sri.ts.clip)
colnames(obs.sri.dphase) = thlname; rownames(obs.sri.dphase) = date(sri.ts.clip)
colnames(ensmean.sri.dphase) = thlname; rownames(ensmean.sri.dphase) = date(sri.ts.clip)
