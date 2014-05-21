###
## Different analysis options for calculating independent based on dependent, contour and fixed values
###

SpecPrevDelta <- function(spec,prev,delta) {
  
  vecdelta<-as.vector(delta)  
  
  vecprev<-as.vector(prev)
  
  vecspec<-as.vector(spec)
  
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdelta)), dimnames = list(vecspec,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]<- 1- pnorm(qnorm(vecspec[i]) - vecdelta[j])
    }
  }
  
  ppv<-array(c(rep(NA,times=length(vecprev)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecprev),length(vecdelta)), dimnames = list(vecspec,vecprev,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        ppv[i,j,k]<- vecprev[j]*(1- pnorm(qnorm(vecspec[i]) - vecdelta[k]))/(vecprev[j]*(1- pnorm(qnorm(vecspec[i]) - vecdelta[k]))+(1-vecspec[i])*(1-vecprev[j]))
      }
    }
  }
  
  cnpv<-array(c(rep(NA,times=length(vecprev)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecprev),length(vecdelta)), dimnames = list(vecspec,vecprev,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        cnpv[i,j,k]<- (vecprev[j]/(1+vecprev[j]))*(pnorm(qnorm(vecspec[i]) - vecdelta[k])/vecspec[i])/(1 + (vecprev[j]/(1+vecprev[j]))*(pnorm(qnorm(vecspec[i]) - vecdelta[k])/vecspec[i]))
      }
    }
  }
  
  T1<-as.table(ppv)
  T2<-as.table(cnpv)
  T3<-as.table(sens)
  data<-list("Positive Predictive Value given specificity, prevalence, and delta"=T1,"Complement of the Negative Predictive Value given specificity, prevalence, and delta"=T2, "Sensitivity given delta and specificity"=T3, "Delta"=delta)
  data
  
}

SpeccNPVDelta <- function(spec,dcnpv,delta) {
  
  ##Delta, cNPV, Specificity as input and Sensitivity as output  
  
  vecspec<-as.vector(spec) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  vecdelta<-as.vector(delta)  ##delta values
  
  vecdnpv<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    vecdnpv[j]<-1-vecdcnpv[j]
  }
  
  dcnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    dcnpvodds[j]<-vecdcnpv[j]/(1-vecdcnpv[j])
  }
  
  dnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdnpv)) {
    dnpvodds[j]<-(vecdnpv[j])/(1-(vecdnpv[j]))
  }
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(vecdelta))),dim = c(length(vecspec),length(vecdcnpv),length(vecdelta)),dimnames = list(vecspec,vecdcnpv,vecdelta))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<-(vecdcnpv[j]/(1-vecdcnpv[j]))*(vecspec[i]/(pnorm(qnorm(vecspec[i])-vecdelta[k])))/(1-(vecdcnpv[j]/(1-vecdcnpv[j]))*(vecspec[i]/(pnorm(qnorm(vecspec[i])-vecdelta[k]))))
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))),dim = c(length(vecspec),length(vecdelta)),dimnames = list(vecspec,vecdelta))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]<-1- pnorm(qnorm(vecspec[i]) - vecdelta[j])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(sens)
  data<-list("Prevalence required to achieve specified cNPV given delta and specificity"=T1, "Sensitivity required to achieve specified cNPV given delta and specificity"=T2, "Delta"=delta,"Desired cNPV as odds"=dcnpvodds)
  data
  
}

SpecPPVDelta <- function(spec,dppv,delta) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  vecdelta<-as.vector(delta)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecspec<-as.vector(spec)
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdelta)), dimnames = list(vecspec,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]<- 1- pnorm(qnorm(vecspec[i]) - vecdelta[j])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdppv),length(vecdelta)), dimnames = list(vecspec,vecdppv,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<- vecdppv[j]*(1-vecspec[i])/((1- pnorm(qnorm(vecspec[i]) - vecdelta[k])) - vecdppv[j]*(1- pnorm(qnorm(vecspec[i]) - vecdelta[k]))+vecdppv[j] - vecdppv[j]*vecspec[i])      
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(sens)
  data<-list("Prevalence required to achieve specified PPV given delta and specificity"=T1, "Sensitivity required to achieve specified PPV given delta and specificity"=T2, "Delta"=delta,"Desired PPV as odds"=dppvodds)
  data
  
}

SpecPPVDelta <- function(spec,dppv,delta) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  vecdelta<-as.vector(delta)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecspec<-as.vector(spec)
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdelta)), dimnames = list(vecspec,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]<- 1- pnorm(qnorm(vecspec[i]) - vecdelta[j])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdppv),length(vecdelta)), dimnames = list(vecspec,vecdppv,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<- vecdppv[j]*(1-vecspec[i])/((1- pnorm(qnorm(vecspec[i]) - vecdelta[k])) - vecdppv[j]*(1- pnorm(qnorm(vecspec[i]) - vecdelta[k]))+vecdppv[j] - vecdppv[j]*vecspec[i])      
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(sens)
  data<-list("Prevalence required to achieve specified PPV given delta and specificity"=T1, "Sensitivity required to achieve specified PPV given delta and specificity"=T2, "Delta"=delta,"Desired PPV as odds"=dppvodds)
  data
  
}

SpecPPVPrev <- function(spec,dppv,prev) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  p<-as.vector(prev)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecspec<-as.vector(spec)
  
  podds<-as.vector(c(rep(NA,times=length(p))))
  for(k in 1:length(p)) {
    podds[k]<-p[k]/(1-p[k])
  }
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  delta<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(prev))), dim = c(length(vecspec),length(vecdppv),length(prev)), dimnames = list(vecspec,vecdppv,prev))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]<- qnorm(vecspec[i]) - qnorm(1-dppvodds[j]/podds[k]*(1-vecspec[i]))
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(prev))), dim = c(length(vecspec),length(vecdppv),length(prev)), dimnames = list(vecspec,vecdppv,prev))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        sens[i,j,k]<- dppvodds[j]/podds[k]*(1-vecspec[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(sens)
  data<-list("Delta required to achieve specified PPV given prevalence and specificity"=T1, "Sensitivity required to achieve specified PPV given prevalence and specificity"=T2, "Prevalence"=prev,"Prevalence Odds"=podds,"Desired PPV as odds"=dppvodds)
  data
  
}

SensPPVDelta <- function(sens,dppv,delta) {
  
  ##Delta, PPV, Sensitivity as input and Specificity as output
  
  vecdelta<-as.vector(delta)  ##delta values
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdelta)), dimnames = list(vecsens,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]<- pnorm(qnorm(1-vecsens[i])+vecdelta[j])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdppv),length(vecdelta)), dimnames = list(vecsens,vecdppv,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<- vecdppv[j]*(1-pnorm(qnorm(1-vecsens[i])+vecdelta[k]))/(vecsens[i] - vecdppv[j]*vecsens[i] + vecdppv[j] - vecdppv[j]*pnorm(qnorm(1-vecsens[i])+vecdelta[k]))      
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(spec)
  data<-list("Prevalence required to achieve specified PPV given delta and sensitivity"=T1, "Specificity required to achieve specified PPV given delta and sensitivity"=T2, "Delta"=delta,"Desired PPV as odds"=dppvodds)
  data
  
}

SensPPVPrev <- function(sens,dppv,prev) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  p<-as.vector(prev)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  
  podds<-as.vector(c(rep(NA,times=length(p))))
  for(k in 1:length(p)) {
    podds[k]<-p[k]/(1-p[k])
  }
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  delta<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(prev))), dim = c(length(vecsens),length(vecdppv),length(prev)), dimnames = list(vecsens,vecdppv,prev))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]<- qnorm(1-vecsens[i]*podds[k]/dppvodds[j])-qnorm(1-vecsens[i])
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(prev))), dim = c(length(vecsens),length(vecdppv),length(prev)), dimnames = list(vecsens,vecdppv,prev))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        spec[i,j,k]<- 1-vecsens[i]*podds[k]/dppvodds[j]
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(spec)
  data<-list("Delta required to achieve specified PPV given prevalence and sensitivity"=T1,"Specificity required to achieve specified PPV given prevalence and sensitivity"=T2,"Prevalence"=prev,"Prevalence Odds"=podds,"Desired PPV as odds"=dppvodds)
  data
  
}

SensPPVSpec <- function(sens,dppv,spec) {
  
  ##Specificity, PPV, Sensitivity as input and Delta and Prevalence as output
  
  vecspec<-as.vector(spec)  ##specificity
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  delta<-array(c(rep(NA,times=length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecspec)), dimnames = list(vecsens,vecspec))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      delta[i,j]<- qnorm(vecspec[j]) - qnorm(1-vecsens[i])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecdppv),length(vecspec)), dimnames = list(vecsens,vecdppv,vecspec))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecspec)) {
        prev[i,j,k]<- vecdppv[j]*(1-vecspec[k])/(vecsens[i] - vecdppv[j]*vecsens[i]+vecdppv[j] - vecdppv[j]*vecspec[k])      
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(delta)
  data<-list("Prevalence required to achieve specified PPV given specificity and sensitivity"=T1, "Delta required to achieve specified PPV given specificity and sensitivity"=T2, "Desired PPV as odds"=dppvodds)
  data
  
}

SensPPVSpec <- function(sens,dppv,spec) {
  
  ##Specificity, PPV, Sensitivity as input and Delta and Prevalence as output
  
  vecspec<-as.vector(spec)  ##specificity
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  
  dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
  for(j in 1:length(vecdppv)) {
    dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
  }
  
  delta<-array(c(rep(NA,times=length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecspec)), dimnames = list(vecsens,vecspec))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      delta[i,j]<- qnorm(vecspec[j]) - qnorm(1-vecsens[i])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecdppv),length(vecspec)), dimnames = list(vecsens,vecdppv,vecspec))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecspec)) {
        prev[i,j,k]<- vecdppv[j]*(1-vecspec[k])/(vecsens[i] - vecdppv[j]*vecsens[i]+vecdppv[j] - vecdppv[j]*vecspec[k])      
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(delta)
  data<-list("Prevalence required to achieve specified PPV given specificity and sensitivity"=T1, "Delta required to achieve specified PPV given specificity and sensitivity"=T2, "Desired PPV as odds"=dppvodds)
  data
  
}

SpeccNPVPrev <- function(spec,dcnpv,prev) {
  
  ##Prevalence, cNPV, Specificity as input and Sensitivity as output  
  
  p<-as.vector(prev)  ##prevalence values
  
  vecspec<-as.vector(spec) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  podds<-as.vector(c(rep(NA,times=length(p))))
  for(k in 1:length(p)) {
    podds[k]<-p[k]/(1+p[k])
  }
  
  vecdnpv<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    vecdnpv[j]<-1-vecdcnpv[j]
  }
  
  dcnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    dcnpvodds[j]<-vecdcnpv[j]/(1-vecdcnpv[j])
  }
  
  dnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdnpv)) {
    dnpvodds[j]<-(vecdnpv[j])/(1-(vecdnpv[j]))
  }
  
  dcnpvprev<-array(c(rep(NA,times=length(dcnpvodds)*length(prev))),dim = c(length(dcnpvodds),length(prev)))
  for(j in 1:length(dcnpvodds)) {
    for(k in 1:length(prev)) {
      dcnpvprev[j,k]<-podds[k]/dcnpvodds[j]
    }
  }
  rownames(dcnpvprev)<-dcnpvodds
  colnames(dcnpvprev)<-prev
  
  delta<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(prev))),dim = c(length(vecspec),length(vecdcnpv),length(prev)),dimnames = list(vecspec,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]<-qnorm(vecspec[i])-qnorm(vecspec[i]/dcnpvprev[j,k])
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(prev))),dim = c(length(vecspec),length(vecdcnpv),length(prev)),dimnames = list(vecspec,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        sens[i,j,k] <- 1-vecspec[i]/dcnpvprev[j,k]
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(sens)
  data<-list("Delta required to achieve specified cNPV given prevalence and specificity"=T1,"Sensitivity required to achieve specified cNPV given prevalence and specificity"=T2,"Prevalence"=prev,"Prevalence Odds"=podds,"Desired NPV"=vecdnpv,"Desired cNPV as odds"=vecdcnpv,"Ratio of desired cNPV and prevalence, odds scale"=dcnpvprev)
  data
}

SenscNPVDelta <- function(sens,dcnpv,delta) {
  
  ##Delta, cNPV, Specificity as input and Sensitivity as output  
  
  vecsens<-as.vector(sens) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  vecdelta<-as.vector(delta)  ##delta values
  
  vecdnpv<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    vecdnpv[j]<-1-vecdcnpv[j]
  }
  
  dcnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    dcnpvodds[j]<-vecdcnpv[j]/(1-vecdcnpv[j])
  }
  
  dnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdnpv)) {
    dnpvodds[j]<-(vecdnpv[j])/(1-(vecdnpv[j]))
  }
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(vecdelta))),dim = c(length(vecsens),length(vecdcnpv),length(vecdelta)),dimnames = list(vecsens,vecdcnpv,vecdelta))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<-(vecdcnpv[j]/(1-vecdcnpv[j]))*((pnorm(vecdelta[k]+qnorm(1-vecsens[i])))/(1-vecsens[i]))/(1-(vecdcnpv[j]/(1-vecdcnpv[j]))*((pnorm(vecdelta[k]+qnorm(1-vecsens[i])))/(1-vecsens[i])))
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))),dim = c(length(vecsens),length(vecdelta)),dimnames = list(vecsens,vecdelta))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]<-pnorm(qnorm(1-vecsens[i])+vecdelta[j])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(spec)
  data<-list("Prevalence required to achieve specified cNPV given delta and sensitivity"=T1, "Specificity required to achieve specified cNPV given delta and sensitivity"=T2, "Delta"=delta,"Desired cNPV as odds"=dcnpvodds)
  data
  
}

SensPrevDelta <- function(sens,prev,delta) {
  
  vecdelta<-as.vector(delta)  ##prevalence values
  
  vecprev<-as.vector(prev)
  
  vecsens<-as.vector(sens)
  
  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdelta)), dimnames = list(vecsens,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]<- pnorm(vecdelta[j]+qnorm(1-vecsens[i]))
    }
  }
  
  ppv<-array(c(rep(NA,times=length(vecprev)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecprev),length(vecdelta)), dimnames = list(vecsens,vecprev,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        ppv[i,j,k]<- vecprev[j]*(vecsens[i])/(vecprev[j]*(vecsens[i])+(1-pnorm(vecdelta[k]+qnorm(1-vecsens[i])))*(1-vecprev[j]))
      }
    }
  }
  
  cnpv<-array(c(rep(NA,times=length(vecprev)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecprev),length(vecdelta)), dimnames = list(vecsens,vecprev,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        cnpv[i,j,k]<- (vecprev[j]/(1+vecprev[j]))*((1-vecsens[i])/(pnorm(vecdelta[k]+qnorm(1-vecsens[i]))))/(1 + (vecprev[j]/(1+vecprev[j]))*((1-vecsens[i])/(pnorm(vecdelta[k]+qnorm(1-vecsens[i])))))
      }
    }
  }
  
  T1<-as.table(ppv)
  T2<-as.table(cnpv)
  T3<-as.table(spec)
  data<-list("Positive Predictive Value given sensitivity, prevalence, and delta"=T1,"Complement of the Negative Predictive Value given sensitivity, prevalence, and delta"=T2, "Specificity given delta and sensitivity"=T3, "Delta"=delta)
  data
  
}

SenscNPVPrev <- function(sens,dcnpv,prev) {
  
  ##Prevalence, cNPV, Sensitivity as input and delta and Specificity as output  
  
  p<-as.vector(prev)  ##prevalence values
  
  vecsens<-as.vector(sens) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  podds<-as.vector(c(rep(NA,times=length(p))))
  for(k in 1:length(p)) {
    podds[k]<-p[k]/(1+p[k])
  }
  
  vecdnpv<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    vecdnpv[j]<-1-vecdcnpv[j]
  }
  
  dcnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    dcnpvodds[j]<-vecdcnpv[j]/(1-vecdcnpv[j])
  }
  
  dnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdnpv)) {
    dnpvodds[j]<-(vecdnpv[j])/(1-(vecdnpv[j]))
  }
  
  dcnpvprev<-array(c(rep(NA,times=length(dcnpvodds)*length(prev))),dim = c(length(dcnpvodds),length(prev)))
  for(j in 1:length(dcnpvodds)) {
    for(k in 1:length(prev)) {
      dcnpvprev[j,k]<-dcnpvodds[j]/podds[k]
    }
  }
  rownames(dcnpvprev)<-dcnpvodds
  colnames(dcnpvprev)<-podds
  
  delta<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(prev))),dim = c(length(vecsens),length(vecdcnpv),length(prev)),dimnames = list(vecsens,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]<-qnorm(dcnpvprev[j,k]*(1-vecsens[i]))-qnorm(1-vecsens[i])
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(prev))),dim = c(length(vecsens),length(vecdcnpv),length(prev)),dimnames = list(vecsens,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        spec[i,j,k]<-dcnpvprev[j,k]*(1-vecsens[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(spec)
  data<-list("Delta required to achieve specified cNPV given prevalence and sensitivity"=T1,"Specificity required to achieve specified cNPV given prevalence and sensitvity"=T2,"Prevalence"=prev,"Prevalence Odds"=podds,"Desired NPV"=vecdnpv,"Desired cNPV as odds"=vecdcnpv,"Ratio of desired cNPV and prevalence, odds scale"=dcnpvprev)
  data
}

SenscNPVSpec <- function(sens,dcnpv,spec) {
  
  ##Sensitivity, cNPV, Specificity as input and Delta and Prevalence as output  
  
  vecsens<-as.vector(sens) ##sensitivity
  
  vecdcnpv<-as.vector(dcnpv)  ##desired cnpv values
  
  vecspec<-as.vector(spec)  ##specificity values
  
  vecdnpv<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    vecdnpv[j]<-1-vecdcnpv[j]
  }
  
  dcnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdcnpv)) {
    dcnpvodds[j]<-vecdcnpv[j]/(1-vecdcnpv[j])
  }
  
  dnpvodds<-as.vector(c(rep(NA,times=length(vecdcnpv))))
  for(j in 1:length(vecdnpv)) {
    dnpvodds[j]<-(vecdnpv[j])/(1-(vecdnpv[j]))
  }
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(vecspec))),dim = c(length(vecsens),length(vecdcnpv),length(vecspec)),dimnames = list(vecsens,vecdcnpv,vecspec))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(vecspec)) {
        prev[i,j,k]<- (vecdcnpv[j]/(1-vecdcnpv[j]))*(vecspec[k]/(1-vecsens[i]))/(1-(vecdcnpv[j]/(1-vecdcnpv[j]))*(vecspec[k]/(1-vecsens[i])))
      }
    }
  }
  
  delta<-array(c(rep(NA,times=length(vecsens)*length(vecspec))),dim = c(length(vecsens),length(vecspec)),dimnames = list(vecsens,vecspec))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      delta[i,j]<-qnorm(vecspec[j]) - qnorm(1-vecsens[i])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(delta)
  data<-list("Prevalence required to achieve specified cNPV given specificity and sensitivity"=T1, "Delta given specificity and sensitivity"=T2, "Desired cNPV as odds"=dcnpvodds)
  data
  
}


###
## Different drawing functions for independent vs dependent with different contours
##

DrawcNPVDeltaSpecPrev <- function(delta, specificity, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- 1-pnorm(qnorm(specificity[i],0,1)-Delta,0,1,1)
    LRMinus <- specificity[i]/(1-Sensitivity)
    cNPV <- (Val/LRMinus)/(1+Val/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Complement of Negative Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawSpecDeltacNPVPrev <- function(spec, cNPV, prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPVPrev <- (prevalence/(1-prevalence))/(cNPV[i]/(1-cNPV[i]))
    Delta<- qnorm(Specificity)-qnorm(Specificity/cNPVPrev[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Delta,type="l",main=c("Specificity vs. Delta","Given Different Values of the","Complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Specificity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltacNPVSpecPrev <- function(cnpv, specificity, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPVPrev <- Val/(cNPV/(1-cNPV))
    Sensitivity <- 1-specificity[i]*cNPVPrev
    Delta <- qnorm(specificity[i]) - qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Delta",ylim=c(min(Delta),5),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawSpeccNPVDeltaPrev <- function(spec, delta, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
    Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-Delta[i],0,1,1)
    LRMinus <- Specificity/(1-Sensitivity)
    cNPV <- (Val/LRMinus)/(1+Val/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~cNPV,type="l",main=c("Specificity vs. Complement of Negative Predictive Value","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Specificity",ylim=range(Specificity),xlim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawDeltaSpeccNPVPrev <- function(spec,cNPV, prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPVPrev <- (prevalence/(1-prevalence))/(cNPV[i]/(1-cNPV[i]))
    Delta<- qnorm(Specificity)-qnorm(Specificity/cNPVPrev[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of the","Complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltaPPVSpecPrev <- function(ppv,  specificity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(specificity[i]) - qnorm(1-(PPV/(1-PPV))/Val*(1-specificity[i]))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~PPV,type="l",main=c("Delta vs. Positive Predictive Value","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Positive Predictive Value",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPPVSpecDeltaPrev <- function(spec, delta, prevalence) {
  
  Val <- prevalence/(1-prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
    Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-Delta[i],0,1,1)
    LRPlus <- Sensitivity/(1-Specificity)
    PPV <- LRPlus*Val/(1+LRPlus*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=range(Specificity),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawSpecDeltaPPVPrev <- function(spec,PPV,prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(Specificity) - qnorm(1-(PPV[i]/(1-PPV[i]))/(prevalence/(1-prevalence))*(1-Specificity))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Delta,type="l",main=c("Specificity vs. Delta","Given Different Values of Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Specificity",xlim=c(min(Delta),5),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawSpecPPVDeltaPrev <- function(spec,  delta, prevalence) {
  
  Val <- prevalence/(1-prevalence)
  Delta<-as.vector(delta)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-Delta[i],0,1,1)
    LRPlus <- Sensitivity/(1-Specificity)
    PPV <- LRPlus*Val/(1+LRPlus*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~PPV,type="l",main=c("Specificity vs. Positive Predictive Value","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Positive Predictive Value",ylab="Specificity",xlim=c(min(PPV),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawPPVDeltaSpecPrev <- function(delta, specificity, prevalence) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- ((1 - pnorm(qnorm(specificity[i])-Delta))/(1-specificity[i]))*Val/(1+((1 - pnorm(qnorm(specificity[i])-Delta))/(1-specificity[i]))*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Specificity",paste("Prevalence =",prevalence)),ylim=c(min(PPV),1),xlab="Delta",ylab="Positive Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPPVPrevSpecDelta <- function(prev, specificity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  Val <- Prevalence/(1-Prevalence)
  Sensitivity <- 1 - pnorm(qnorm(specificity)-delta)
  LRPlus <- Sensitivity/(1-specificity)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus[i]*Val/(1+LRPlus[i]*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Positive Predictive Value",ylim=c(min(PPV),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawPPVSpecPrevDelta <- function(spec, prevalence, delta) {
  
  Val <- prevalence/(1-prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-delta,0,1,1)
  LRPlus <- Sensitivity/(1-Specificity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus*Val[i]/(1+LRPlus*Val[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity","Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSpecPrevPPVDelta <- function(spec, PPV, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV[i]*(1-Specificity)/((1- pnorm(qnorm(Specificity) - delta)) - PPV[i]*(1- pnorm(qnorm(Specificity) - delta))+PPV[i] - PPV[i]*Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence","Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Specificity",xlim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawPrevPPVSpecDelta <- function(ppv, specificity, delta) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-specificity[i])/((1- pnorm(qnorm(specificity[i]) - delta)) - PPV*(1- pnorm(qnorm(specificity[i]) - delta))+PPV - PPV*specificity[i])      
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Positive Predictive Value",ylab="Prevalence",ylim=c(0,max(Prevalence)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawSpecPPVPrevDelta <- function(spec, prevalence, delta) {
  
  Val <- prevalence/(1-prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-delta,0,1,1)
  LRPlus <- Sensitivity/(1-Specificity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus*Val[i]/(1+LRPlus*Val[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~PPV,type="l",main=c("Specificity vs. Positive Predictive Value","Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Positive Predictive Value",ylab="Specificity",xlim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPrevSpecPPVDelta <- function(spec, PPV, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV[i]*(1-Specificity)/((1- pnorm(qnorm(Specificity) - delta)) - PPV[i]*(1- pnorm(qnorm(Specificity) - delta))+PPV[i] - PPV[i]*Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawDeltaPPVSensPrev <- function(ppv,  sensitivity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(1-sensitivity[i]*Val/(PPV/(1-PPV)))-qnorm(1-sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~PPV,type="l",main=c("Delta vs. Positive Predictive Value","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Positive Predictive Value",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawDeltaSensPPVPrev <- function(sens,  PPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(1-Sensitivity*((prevalence/(1-prevalence))/((1-PPV[i])/PPV[i])))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Delta",ylim=c(0,max(Delta)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawPPVSensDeltaPrev <- function(sens, delta, prevalence) {
  
  Val <- prevalence/(1-prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
    Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+Delta[i],0,1,1)
    LRPlus <- Sensitivity/(1-Specificity)
    PPV <- LRPlus*Val/(1+LRPlus*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=range(Sensitivity),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawSensPPVDeltaPrev <- function(sens, delta, prevalence) {
  
  Val <- prevalence/(1-prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
    Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+Delta[i],0,1,1)
    LRPlus <- Sensitivity/(1-Specificity)
    PPV <- LRPlus*Val/(1+LRPlus*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~PPV,type="l",main=c("Sensitivity vs. Positive Predictive Value","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Positive Predictive Value",ylab="Sensitivity",ylim=range(Sensitivity),xlim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawPPVDeltaSensPrev <- function(delta, sensitivity, prevalence) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- (sensitivity[i]*prevalence)/(sensitivity[i]*prevalence + (1-pnorm(qnorm(1-sensitivity[i])+Delta))*prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Positive Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawSensDeltaPPVPrev <- function(sens, PPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(1-Sensitivity*((prevalence/(1-prevalence))/(PPV[i]/(1-PPV[i]))))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Delta,type="l",main=c("Sensitivity vs. Delta","Given Different Values of Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Sensitivity",xlim=c(min(Delta),9),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawPPVPrevSensDelta <- function(prev, sensitivity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  Val <- Prevalence/(1-Prevalence)
  Specificity <- pnorm(qnorm(1-sensitivity,0,1)+delta,0,1,1)
  LRPlus <- sensitivity/(1-Specificity)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus[i]*Val/(1+LRPlus[i]*Val)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Positive Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPrevPPVSensDelta <- function(ppv,  sensitivity, delta) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-pnorm(qnorm(1-sensitivity[i])+delta))/(sensitivity[i] - PPV*sensitivity[i] + PPV - PPV*pnorm(qnorm(1-sensitivity[i])+delta))      
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Prevalence vs. Positive Predictive Value", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Positive Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawSensPrevPPVDelta <- function(sens,  PPV, delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence<- PPV[i]*(1-pnorm(qnorm(1-Sensitivity)+delta))/(Sensitivity - PPV[i]*Sensitivity + PPV[i] - PPV[i]*pnorm(qnorm(1-Sensitivity)+delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence", "Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Sensitivity",xlim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawPrevSensPPVDelta <- function(sens, PPV, delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence<- PPV[i]*(1-pnorm(qnorm(1-Sensitivity)+delta))/(Sensitivity - PPV[i]*Sensitivity + PPV[i] - PPV[i]*pnorm(qnorm(1-Sensitivity)+delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity", "Given Different Values of Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Prevalence",ylim=c(min(Prevalence),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawPPVSensPrevDelta <- function(sens, prevalence, delta) {
  
  Val <- prevalence/(1-prevalence)
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+delta,0,1,1)
  LRPlus <- Sensitivity/(1-Specificity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus*Val[i]/(1+LRPlus*Val[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,max(PPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensPPVPrevDelta <- function(sens, prevalence, delta) {
  
  Val <- prevalence/(1-prevalence)
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+delta,0,1,1)
  LRPlus <- Sensitivity/(1-Specificity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- LRPlus*Val[i]/(1+LRPlus*Val[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~PPV,type="l",main=c("Sensitivity vs. Positive Predictive Value", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Positive Predictive Value",ylab="Sensitivity",xlim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPPVSensSpecPrev <- function(sens,specificity, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- (Sensitivity/(1-specificity[i]))*(prevalence/(1-prevalence))/(1+(Sensitivity/(1-specificity[i]))*(prevalence/(1-prevalence)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity", "Given Different Values of Specificity"),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(min(PPV),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSensPPVSpecPrev <- function(ppv, specificity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- (1-specificity[i])*(PPV/(1-PPV))/Val
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~PPV,type="l",main=c("Sensitivity vs. Positive Predictive Value", "Given Different Values of Specificity"),xlab="Positive Predictive Value",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSpecSensPPVPrev <- function(sens, PPV,prevalence) {
  
  Val <- prevalence/(1-prevalence)
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- 1 - Sensitivity*((1-PPV[i])/PPV[i])*(prevalence/(1-prevalence))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity", "Given Different Values of Prevalence"),xlab="Sensitivity",ylab="Specificity",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
  
}

DrawPPVSpecSensPrev <- function(spec, sensitivity,prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV <- (sensitivity[i]/(1-Specificity))*(prevalence/(1-prevalence))/(1+(sensitivity[i]/(1-Specificity))*(prevalence/(1-prevalence)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity", "Given Different Values of Sensitivity"),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(min(PPV),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawSpecPPVSensPrev <- function(ppv, sensitivity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- 1-sensitivity[i]*Val/(PPV/(1-PPV))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~PPV,type="l",main=c("Specificity vs. Positive Predictive Value", "Given Different Values of Sensitivity"),xlab="Positive Predictive Value",ylab="Specificity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=PPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawcNPVPrevSpecDelta <- function(prev, specificity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  Val <- Prevalence/(1+Prevalence)
  Sensitivity <- 1-pnorm(qnorm(specificity,0,1)-delta,0,1,1)
  LRMinus <- specificity/(1-Sensitivity)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val/LRMinus[i])/(1+Val/LRMinus[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Specificity vs. Complement of Negative Predictive Value","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Complement of Negative Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawcNPVSpecPrevDelta <- function(spec, prevalence, delta) {
  
  Val <- prevalence/(1+prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-delta,0,1,1)
  LRMinus <- Specificity/(1-Sensitivity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val[i]/LRMinus)/(1+Val[i]/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of Negative Predictive Value vs. Specificity","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Complement of Negative Predictive Value",ylim=c(0,max(cNPV)),xlim=rev(range(Specificity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawSpeccNPVPrevDelta <- function(spec, prevalence, delta) {
  
  Val <- prevalence/(1+prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Sensitivity <- 1-pnorm(qnorm(Specificity,0,1)-delta,0,1,1)
  LRMinus <- Specificity/(1-Sensitivity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val[i]/LRMinus)/(1+Val[i]/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~cNPV,type="l",main=c("Specificity vs. Complement of Negative Predictive Value","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Complement of Negative Predictive Value",ylab="Specificity",ylim=range(Specificity),xlim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawSpecPrevcNPVDelta <- function(spec, cNPV, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (Specificity*cNPV[i])/((1-cNPV[i])*(pnorm(qnorm(Specificity) - delta)) + Specificity*cNPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence","Given Different Values of the","Complement of the Negative Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Specificity",xlim=c(0,max(Prevalence)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawPrevcNPVSpecDelta <- function(cnpv, specificity, delta) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (specificity[i]*cNPV)/((1-cNPV)*(pnorm(qnorm(specificity[i]) - delta)) + specificity[i]*cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Specificity vs. Complement of Negative Predictive Value","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Complement of Negative Predictive Value",ylim=c(0,max(Prevalence)),ylab="Prevalence",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawDeltaPrevSpeccNPV <- function(prev, spec, cNPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(spec[i])-qnorm(spec[i]*((1-cNPV)/cNPV)*Prevalence/(1+Prevalence))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Specificity"),xlab="Prevalence",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawPrevDeltaSpeccNPV <- function(delta, spec, cnpv) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (spec[i]*cnpv)/((1-cnpv)*(pnorm(qnorm(spec[i]) - Delta)) + spec[i]*cnpv)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Specificity"),ylim=c(0,1),xlab="Delta",ylab="Prevalence",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawDeltaSpecPrevcNPV <- function(spec, prev, cNPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(Specificity)-qnorm(Specificity*((1-cNPV)/cNPV)*prev[i]/(1+prev[i]))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of Prevalence"),xlab="Specificity",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPrevSpecDeltacNPV <- function(spec, delta,cnpv) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (Specificity*cnpv)/((1-cnpv)*(pnorm(qnorm(Specificity) - delta[i])) + Specificity*cnpv)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Delta"),ylim=c(0,1),xlab="Specificity",ylab="Prevalence",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawPrevSpecDeltaPPV <- function(spec, PPV, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-Specificity)/((1- pnorm(qnorm(Specificity) - delta[i])) - PPV*(1- pnorm(qnorm(Specificity) - delta[i]))+PPV - PPV*Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Delta"),xlab="Specificity",ylab="Prevalence",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawDeltaPrevSpecPPV <- function(prev,  spec,PPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(spec[i])-qnorm(1-((1-Prevalence)/Prevalence)*(PPV/(1-PPV))*(1-spec[i]))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Specificity"),xlab="Prevalence",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawPrevDeltaSpecPPV <- function(delta, spec, PPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-spec[i])/((1- pnorm(qnorm(spec[i]) - Delta)) - PPV*(1- pnorm(qnorm(spec[i]) - Delta))+PPV - PPV*spec[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Specificity"),ylim=c(0,1),xlab="Delta",ylab="Prevalence",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawDeltaSpecPrevPPV <- function(spec, prev, PPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(Specificity)-qnorm(1-((1-prev[i])/prev[i])*(PPV/(1-PPV))*(1-Specificity))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of Prevalence"),xlab="Specificity",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensDeltacNPVPrev <- function(sens, cNPV, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  cNPVOdds <- cNPV/(1-cNPV)
  cNPVPrev <- Val*cNPVOdds
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(((cNPV[i]/(1-cNPV[i]))/Val)*(1-Sensitivity))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Delta,type="l",main=c("Sensitivity vs. Delta","Given Different Values of the","Complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Sensitivity",xlim=c(min(Delta),9),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawcNPVDeltaSensPrev <- function(delta, sensitivity, prevalence) {
  
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Val <- prevalence/(1+prevalence)
    Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
    Specificity <- pnorm(qnorm(1-sensitivity[i],0,1)+Delta,0,1,1)
    LRMinus <- Specificity/(1-sensitivity[i])
    cNPV <- (Val/LRMinus)/(1+Val/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Complement of Negative Predictive Value",ylim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawSenscNPVDeltaPrev <- function(sens, delta, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
    Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+Delta[i],0,1,1)
    LRMinus <- Specificity/(1-Sensitivity)
    cNPV <- (Val/LRMinus)/(1+Val/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~cNPV,type="l",main=c("Sensitivity vs. Complement of Negative Predictive Value","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Sensitivity",xlim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawcNPVSensDeltaPrev <- function(sens, delta, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
    Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+Delta[i],0,1,1)
    LRMinus <- Specificity/(1-Sensitivity)
    cNPV <- (Val/LRMinus)/(1+Val/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of Negative Predictive Value vs. Sensitivity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Complement of Negative Predictive Value",xlim=range(Sensitivity),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawDeltaSenscNPVPrev <- function(sens, cNPV, prevalence) {
  
  Val <- prevalence/(1-prevalence)
  cNPVOdds <- cNPV/(1-cNPV)
  cNPVPrev <- Val*cNPVOdds
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(1-cNPVPrev[i]*(1-Sensitivity))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of the","Complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Delta",ylim=c(0,max(Delta)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltacNPVSensPrev <- function(cnpv, sensitivity, prevalence,) {
  
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Val <- prevalence/(1+prevalence)
    cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
    Delta<- qnorm((1-sensitivity[i])*(cNPV/(1-cNPV))/Val)-qnorm(1-sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawcNPVPrevSensDelta <- function(prev, sensitivity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  Val <- Prevalence/(1+Prevalence)
  Specificity <- pnorm(delta + qnorm(1-sensitivity))
  LRMinus <- Specificity/(1-sensitivity)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val/LRMinus[i])/(1+Val/LRMinus[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of the Negative Predictive Value vs. Prevalence","Given Different Values of Sensitivity",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Complement of the Negative Predictive Value",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence, lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawcNPVSensPrevDelta <- function(sens, prevalence, delta) {
  
  Val <- prevalence/(1+prevalence)
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+delta,0,1,1)
  LRMinus <- Specificity/(1-Sensitivity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val[i]/LRMinus)/(1+Val[i]/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of the Negative Predictive Value vs. Sensitivity","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensPrevcNPVDelta <- function(sens, cNPV,delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (pnorm(qnorm(1-Sensitivity)+delta)*cNPV[i])/((1-cNPV[i])*(1-Sensitivity) + pnorm(qnorm(1-Sensitivity)+delta)*cNPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence","Given Different Values of the", "Complement of the Negative Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Sensitivity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence, lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawPrevcNPVSensDelta <- function(cnpv, sensitivity, delta) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- pnorm(qnorm(1-sensitivity[i])+delta)
    Prevalence <- Specificity*cNPV/((1-cNPV)*(1-sensitivity[i]) + Specificity*cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. Complement of the Negative Predictive Value","Given Different Values of Sensitivity",paste(expression(Delta),"=",delta)),xlab="Complement of the Negative Predictive Value",ylab="Prevalence",ylim=c(min(Prevalence),1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV, lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawSenscNPVPrevDelta <- function(sens, prevalence, delta) {
  
  Val <- prevalence/(1+prevalence)
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Specificity <- pnorm(qnorm(1-Sensitivity,0,1)+delta,0,1,1)
  LRMinus <- Specificity/(1-Sensitivity)
  
  Iterations <- 1:length(Val)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (Val[i]/LRMinus)/(1+Val[i]/LRMinus)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~cNPV,type="l",main=c("Sensitivity vs. Complement of the Negative Predictive Value","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Complement of Negative Predictive Value",ylab="Sensitivity",xlim=c(0,max(cNPV)),ylim=range(Sensitivity),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawDeltaPrevSenscNPV <- function(prev, sens,cNPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(((1+Prevalence)/Prevalence)*(1-sens[i])*(cNPV/(1-cNPV)))-qnorm(1-sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Sensitivity"),xlab="Prevalence",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawDeltaSensPrevcNPV <- function(sens, prev, cNPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(((1+prev[i])/prev[i])*(1-Sensitivity)*(cNPV/(1-cNPV)))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of Prevalence"),xlab="Sensitivity",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPrevDeltaSensPPV <- function(delta, sens, PPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-pnorm(qnorm(1-sens[i])+Delta))/(sens[i] - PPV*sens[i] + PPV - PPV*pnorm(qnorm(1-sens[i])+Delta))      
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Sensitivity"),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPrevSensDeltacNPV <- function(sens, delta, cNPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (pnorm(qnorm(1-Sensitivity)+delta[i])*cNPV)/((1-cNPV)*(1-Sensitivity) + pnorm(qnorm(1-Sensitivity)+delta[i])*cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of Delta"),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawPrevSensDeltaPPV <- function(sens, delta, PPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- PPV*(1-pnorm(qnorm(1-Sensitivity)+delta[i]))/(Sensitivity - PPV*Sensitivity + PPV - PPV*pnorm(qnorm(1-Sensitivity)+delta[i]))      
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of Delta"),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawPrevDeltaSenscNPV <- function(delta, sens, cNPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence <- (pnorm(qnorm(1-sens[i])+Delta)*cNPV)/((1-cNPV)*(1-sens[i]) + pnorm(qnorm(1-sens[i])+Delta)*cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Sensitivity"),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawDeltaPrevSensPPV <- function(prev, sens,  PPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(1-sens[i]*(Prevalence/(1-Prevalence))/(PPV/(1-PPV)))-qnorm(1-sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Sensitivity"),xlab="Prevalence",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawDeltaSensPrevPPV <- function(sens, prev, PPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta<-qnorm(1-Sensitivity*(prev[i]/(1-prev[i]))/(PPV/(1-PPV)))-qnorm(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of Prevalence"),xlab="Sensitivity",ylab="Delta",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSpecSensPrevcNPV <- function(sens, prev, cNPV) {
  
  Sensitivity<- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- (1-Sensitivity)*((1+prev[i])/prev[i])*(cNPV/(1-cNPV))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity","Given Different Values of Prevalence for a Fixed cNPV"),ylim=c(0,1),xlab="Sensitivity",ylab="Specificity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensSpecPrevcNPV <- function(spec, prev, cNPV) {
  
  Specificity<- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- 1 - Specificity*((1+prev[i])/prev[i])*(cNPV/(1-cNPV))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity","Given Different Values of Prevalence for a Fixed cNPV"),ylim=c(0,1),xlab="Specificity",ylab="Sensitivity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawcNPVSensSpecPrev <- function(sens, specificity, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- ((prevalence/(1+prevalence))*(1-Sensitivity)/specificity[i])/(1+((1-Sensitivity)/specificity[i])*(prevalence/(1+prevalence)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of the Negative Predictive Value vs. Sensitivity", "Given Different Values of Specificity"),xlab="Sensitivity",ylab="Complement of the Negative Predictive Value",ylim=c(0.,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSenscNPVSpecPrev <- function(cnpv, specificity,prevalence) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  Val <- prevalence/(1+prevalence)
  cNPVPrev <- Val/(cNPV/(1-cNPV))
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- 1-specificity[i]*cNPVPrev
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~cNPV,type="l",main=c("Sensitivity vs. Complement of the Negative Predictive Value", "Given Different Values of Specificity"),xlab="Complement of the Negative Predictive Value",ylab="Sensitivity",xlim=c(0.,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSpecSenscNPVPrev <- function(sens, cNPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- (cNPV[i]/(1-cNPV[i]))*((1+prevalence)/prevalence)*(1-Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity", "Given Different Values of the","Complement of the Negative Predictive Value"),ylim=c(min(Sensitivity),1),xlab="Sensitivity",ylab="Specificity",lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
  
}

DrawSpeccNPVSensPrev <- function(cnpv, sensitivity, prevalence) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- (cNPV/(1-cNPV))*(prevalence/(1-prevalence))*(1-sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~cNPV,type="l",main=c("Specificity vs. Complement of the Negative Predictive Value", "Given Different Values of Sensitivity"),xlab="Complement of the Negative Predictive Value",ylab="Specificity",xlim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=cNPV,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawcNPVSpecSensPrev <- function(spec, sensitivity, prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV <- (prevalence/(1+prevalence)*(1-sensitivity[i])/Specificity)/(1+((1-sensitivity[i])/Specificity)*(prevalence/(1+prevalence)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of the Negative Predictive Value vs. Specificity", "Given Different Values of Sensitivity"),xlab="Specificity",ylab="Complement of the Negative Predictive Value",ylim=c(0,max(cNPV)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawSensSpecPPVPrev <- function(spec, PPV, prevalence) {
  Val <- prevalence/(1-prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- (PPV[i]/(1 - PPV[i]))/Val*(1 - Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity", "Given Different Values of Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Sensitivity",ylim=c(0,max(Sensitivity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
}
