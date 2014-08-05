###
## Different analysis options for calculating independent based on dependent, contour and fixed values
###
SensSpecPrev <- function(sens,spec,prev) {
  PPV<-array(c(rep(NA,times=length(sens)*length(spec)*length(prev))), dim = c(length(sens),length(spec),length(prev)), dimnames = list(sens,spec,prev))
  for(i in 1:length(sens)) {
    for(j in 1:length(spec)) {
      for(k in 1:length(prev)) {
        PPV[i,j,k] = calculatePPVfrSpecSens(spec[j],sens[i],prev[k])
      }
    }
  }
  cNPV<-array(c(rep(NA,times=length(sens)*length(spec)*length(prev))), dim = c(length(sens),length(spec),length(prev)), dimnames = list(sens,spec,prev))
  for(i in 1:length(sens)) {
    for(j in 1:length(spec)) {
      for(k in 1:length(prev)) {
        cNPV[i,j,k] <- calculatecNPVfrSpecSens(spec[j],sens[i],prev[k])
      }
    }
  }
  Delta<-array(c(rep(NA,times=length(sens)*length(spec))), dim = c(length(sens),length(spec)), dimnames = list(sens,spec))
  for(i in 1:length(sens)) {
    for(j in 1:length(spec)) {
        Delta[i,j] =calculateDeltafrSpecSens(spec[j],sens[i])
    }
  }
  T1 <- as.table(PPV)
  T2 <- as.table(cNPV)
  T3 <- as.table(Delta)
  data <- list("Positive Predictive Value given sensitivity, specificity, and prevalence"=T1,"Complement of the Negative Predictive Value given sensitivity, specificity, and prevalence"=T2,"Delta"=T3)
  data
}



SensPPVDelta <- function(sens,dppv,delta) {
  
  ##Delta, PPV, Sensitivity as input and Specificity as output
  
  vecdelta<-as.vector(delta)  ##delta values
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)

  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdelta)), dimnames = list(vecsens,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]= calculateSensfrSpec(vecsens[i],vecdelta[j])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdppv),length(vecdelta)), dimnames = list(vecsens,vecdppv,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]<- calculatePrevalencefrPPV(vecdppv[j],calculateSpecfrSens(vecsens[i],delta[k]),vecsens[i])
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(spec)
  data<-list("Prevalence given desired PPV, delta, and sensitivity"=T1, "Specificity given desired PPV, delta, and sensitivity"=T2, "Delta"=delta)
  data

}
 
SensPPVPrev <- function(sens,dppv,prev) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  p<-as.vector(prev)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  

  delta<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(prev))), dim = c(length(vecsens),length(vecdppv),length(prev)), dimnames = list(vecsens,vecdppv,prev))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]= calculateDeltafrPPV(vecsens[i],prev[k],vecdppv[j])
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(prev))), dim = c(length(vecsens),length(vecdppv),length(prev)), dimnames = list(vecsens,vecdppv,prev))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        spec[i,j,k]=calculateSpecfrPPV(vecdppv[j],prev[k],vecsens[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(spec)
  data<-list("Delta given desired PPV, prevalence, and sensitivity"=T1,"Specificity given desired PPV, prevalence, and sensitivity"=T2,"Prevalence"=prev)
  data
  
}

SensSpecPPV <- function(sens,spec,dppv) {
  
  ##Specificity, PPV, Sensitivity as input and Delta and Prevalence as output
  
  vecspec<-as.vector(spec)  ##specificity
  
  vecdppv<-as.vector(dppv)
  
  vecsens<-as.vector(sens)
  
  
  delta<-array(c(rep(NA,times=length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecspec)), dimnames = list(vecsens,vecspec))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      delta[i,j]= calculateDeltafrSpecSens(vecspec[j],vecsens[i])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecsens)*length(vecspec))), dim = c(length(vecsens),length(vecspec), length(vecdppv)), dimnames = list(vecsens,vecspec, vecdppv))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      for(k in 1:length(vecdppv)) {
        prev[i,j,k]=calculatePrevalencefrPPV(vecdppv[k],vecspec[j],vecsens[i])
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(delta)
  data<-list("Prevalence given desired PPV, specificity, and sensitivity"=T1, "Delta given desired PPV, specificity, and sensitivity"=T2)
  data
  
}


SensPrevDelta <- function(sens,prev,delta) {
  
  vecdelta<-as.vector(delta)  ##prevalence values
  
  vecprev<-as.vector(prev)
  
  vecsens<-as.vector(sens)
  
  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecdelta)), dimnames = list(vecsens,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]=calculateSpecfrSens(vecsens[i],vecdelta[j])
    }
  }
  
  ppv<-array(c(rep(NA,times=length(vecprev)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecprev),length(vecdelta)), dimnames = list(vecsens,vecprev,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        ppv[i,j,k]=calculatePPVfrSpecSens(calculateSpecfrSens(vecsens[i],vecdelta[k]),vecsens[i],vecprev[j])
      }
    }
  }
  
  cnpv<-array(c(rep(NA,times=length(vecprev)*length(vecsens)*length(vecdelta))), dim = c(length(vecsens),length(vecprev),length(vecdelta)), dimnames = list(vecsens,vecprev,vecdelta))
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        cnpv[i,j,k]=calculatecNPVfrSpecSens(calculateSpecfrSens(vecsens[i],vecdelta[k]),vecsens[i],vecprev[j])
      }
    }
  }
  
  T1<-as.table(ppv)
  T2<-as.table(cnpv)
  T3<-as.table(spec)
  data<-list("Positive Predictive Value given sensitivity, prevalence, and delta"=T1,"Complement of the Negative Predictive Value given sensitivity, prevalence, and delta"=T2, "Specificity given delta and sensitivity"=T3, "Delta"=delta)
  data
  
}


SenscNPVDelta <- function(sens,dcnpv,delta) {
  
  ##Delta, cNPV, Specificity as input and Sensitivity as output  
  
  vecsens<-as.vector(sens) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  vecdelta<-as.vector(delta)  ##delta values
  
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(vecdelta))),dim = c(length(vecsens),length(vecdcnpv),length(vecdelta)),dimnames = list(vecsens,vecdcnpv,vecdelta))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]=calculatePrevalencefrcNPV(vecdcnpv[j],calculateSpecfrSens(vecsens[i],vecdelta[k]),vecsens[i])
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecsens)*length(vecdelta))),dim = c(length(vecsens),length(vecdelta)),dimnames = list(vecsens,vecdelta))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdelta)) {
      spec[i,j]=calculateSpecfrSens(vecsens[i],vecdelta[j])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(spec)
  data<-list("Prevalence given desired cNPV, delta, and sensitivity"=T1, "Specificity given desired cNPV, delta, and sensitivity"=T2, "Delta"=delta)
  data
  
}


SenscNPVPrev <- function(sens,dcnpv,prev) {
  
  ##Prevalence, cNPV, Sensitivity as input and delta and Specificity as output  
  
  p<-as.vector(prev)  ##prevalence values
  
  vecsens<-as.vector(sens) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  
  
  delta<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(prev))),dim = c(length(vecsens),length(vecdcnpv),length(prev)),dimnames = list(vecsens,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]=calculateDeltafrcNPVSens(vecsens[i],prev[k],vecdcnpv[j])
      }
    }
  }
  
  spec<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(prev))),dim = c(length(vecsens),length(vecdcnpv),length(prev)),dimnames = list(vecsens,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        spec[i,j,k]=calculateSpecfrcNPV(vecdcnpv[j],prev[k],vecsens[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(spec)
  data<-list("Delta given desired cNPV, prevalence, and sensitivity"=T1,"Specificity given desired cNPV, prevalence, and sensitivity"=T2,"Prevalence"=prev)
  data
}


SensSpeccNPV <- function(sens,spec,dcnpv) {
  
  ##Sensitivity, cNPV, Specificity as input and Delta and Prevalence as output  
  
  vecsens<-as.vector(sens) ##sensitivity
  
  vecdcnpv<-as.vector(dcnpv)  ##desired cnpv values
  
  vecspec<-as.vector(spec)  ##specificity values
  
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecsens)*length(vecspec))),dim = c(length(vecsens),length(vecspec), length(vecdcnpv)),dimnames = list(vecsens,vecspec, vecdcnpv))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      for(k in 1:length(vecdcnpv)) {
        prev[i,j,k]=calculatePrevalencefrcNPV(vecdcnpv[k],vecspec[j],vecsens[i])
      }
    }
  }
  
  delta<-array(c(rep(NA,times=length(vecsens)*length(vecspec))),dim = c(length(vecsens),length(vecspec)),dimnames = list(vecsens,vecspec))  ##full table
  for(i in 1:length(vecsens)) {
    for(j in 1:length(vecspec)) {
      delta[i,j]=calculateDeltafrSpecSens(vecspec[j],vecsens[i])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(delta)
  data<-list("Prevalence given desired cNPV, specificity, and sensitivity"=T1, "Delta given specificity and sensitivity"=T2)
  data
  
}


SpecPPVDelta <- function(spec,dppv,delta) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  vecdelta<-as.vector(delta)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecspec<-as.vector(spec)
  
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdelta)), dimnames = list(vecspec,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]=calculateSensfrSpec(vecspec[i],vecdelta[j])
    }
  }
  
  prev<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdppv),length(vecdelta)), dimnames = list(vecspec,vecdppv,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k]=calculatePrevalencefrPPV(vecdppv[j],vecspec[i],calculateSensfrSpec(vecspec[i],vecdelta[k]))
      }
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(sens)
  data<-list("Prevalence given desired PPV, delta, and specificity"=T1, "Sensitivity given desired PPV, delta, and specificity"=T2, "Delta"=delta)
  data
  
}


SpecPPVPrev <- function(spec,dppv,prev) {
  
  ##Prevalence, PPV, Sensitivity as input and Specificity as output
  
  p<-as.vector(prev)  ##prevalence values
  
  vecdppv<-as.vector(dppv)
  
  vecspec<-as.vector(spec)
  

  
  delta<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(prev))), dim = c(length(vecspec),length(vecdppv),length(prev)), dimnames = list(vecspec,vecdppv,prev))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]=calculateDeltafrPPVSpec(vecspec[i],prev[k],vecdppv[j])
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(prev))), dim = c(length(vecspec),length(vecdppv),length(prev)), dimnames = list(vecspec,vecdppv,prev))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdppv)) {
      for(k in 1:length(prev)) {
        sens[i,j,k]= calculateSensfrPPV(vecdppv[j],prev[k],vecspec[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(sens)
  data<-list("Delta given desired PPV, prevalence, and specificity"=T1, "Sensitivity given desired PPV, prevalence, and specificity"=T2, "Prevalence"=prev)
  data
  
}


SpecPrevDelta <- function(spec,prev,delta) {
  
  vecdelta<-as.vector(delta)  
  
  vecprev<-as.vector(prev)
  
  vecspec<-as.vector(spec)
  
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecdelta)), dimnames = list(vecspec,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]=calculateSensfrSpec(vecspec[i],vecdelta[j])
    }
  }
  
  ppv<-array(c(rep(NA,times=length(vecprev)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecprev),length(vecdelta)), dimnames = list(vecspec,vecprev,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        ppv[i,j,k] = calculatePPVfrSpecSens(vecspec[i],calculateSensfrSpec(vecspec[i],vecdelta[k]),vecprev[j])
      }
    }
  }
  
  cnpv<-array(c(rep(NA,times=length(vecprev)*length(vecspec)*length(vecdelta))), dim = c(length(vecspec),length(vecprev),length(vecdelta)), dimnames = list(vecspec,vecprev,vecdelta))
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecprev)) {
      for(k in 1:length(vecdelta)) {
        cnpv[i,j,k]=calculatecNPVfrSpecSens(vecspec[i],calculateSensfrSpec(vecspec[i],vecdelta[k]),vecprev[j])
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
  
  
  prev<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(vecdelta))),dim = c(length(vecspec),length(vecdcnpv),length(vecdelta)),dimnames = list(vecspec,vecdcnpv,vecdelta))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(vecdelta)) {
        prev[i,j,k] = calculatePrevalencefrcNPV(vecdcnpv[j],vecspec[i],calculateSensfrSpec(vecspec[i],vecdelta[k]))
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecspec)*length(vecdelta))),dim = c(length(vecspec),length(vecdelta)),dimnames = list(vecspec,vecdelta))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdelta)) {
      sens[i,j]=calculateSensfrSpec(vecspec[i],vecdelta[j])
    }
  }
  
  T1<-as.table(prev)
  T2<-as.table(sens)
  data<-list("Prevalence given desired cNPV, delta, and specificity"=T1, "Sensitivity given desired cNPV, delta, and specificity"=T2, "Delta"=delta)
  data
  
}


SpeccNPVPrev <- function(spec,dcnpv,prev) {
  
  ##Prevalence, cNPV, Specificity as input and Sensitivity as output  
  
  p<-as.vector(prev)  ##prevalence values
  
  vecspec<-as.vector(spec) ##desired cnpv
  
  vecdcnpv<-as.vector(dcnpv)  ##specificity values
  
  
  delta<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(prev))),dim = c(length(vecspec),length(vecdcnpv),length(prev)),dimnames = list(vecspec,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        delta[i,j,k]=calculateDeltafrSpecSens(vecspec[i],calculateSensfrcNPV(vecdcnpv[j],prev[k],vecspec[i]))
      }
    }
  }
  
  sens<-array(c(rep(NA,times=length(vecdcnpv)*length(vecspec)*length(prev))),dim = c(length(vecspec),length(vecdcnpv),length(prev)),dimnames = list(vecspec,vecdcnpv,prev))  ##full table
  for(i in 1:length(vecspec)) {
    for(j in 1:length(vecdcnpv)) {
      for(k in 1:length(prev)) {
        sens[i,j,k] = calculateSensfrcNPV(vecdcnpv[j],prev[k],vecspec[i])
      }
    }
  }
  
  T1<-as.table(delta)
  T2<-as.table(sens)
  data<-list("Delta given desired cNPV, prevalence, and specificity"=T1,"Sensitivity given desired cNPV, prevalence, and specificity"=T2,"Prevalence"=prev)
  data
}



###
## Different drawing functions for independent vs dependent with different contours
##


###
## Different drawing functions for independent vs dependent with different contours
##

DrawcNPVDeltaSpecPrev <- function(delta, specificity, prevalence) {
  

  Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV = calculatecNPVfrSpecSens(specificity[i],calculateSensfrSpec(specificity[i],Delta),prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawcNPVDeltaPrevSpec<- function(delta, prevalence,specificity) {
  
  
  Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV = calculatecNPVfrSpecSens(specificity,calculateSensfrSpec(specificity,Delta),prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Prevalence",paste("Specificity =",specificity)),xlab="Delta",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawDeltacNPVSpecPrev <- function(cnpv, specificity, prevalence) {
  
  Val <- prevalence/(1+prevalence)
  cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta =calculateDeltafrcNPVSpec(specificity[i],prevalence,cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawDeltacNPVPrevSpec <- function(cnpv, prevalence,specificity) {
  
  
  cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    Delta = calculateDeltafrcNPVSpec(specificity,prevalence[i],cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Prevalence",paste("Specificity =",specificity)),xlab="Complement of Negative Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}


DrawDeltaSpeccNPVPrev <- function(spec,cNPV, prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta= calculateDeltafrcNPVSpec(Specificity,prevalence,cNPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of","complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltaPPVSpecPrev <- function(ppv,  specificity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  Val <- prevalence/(1-prevalence)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta = calculateDeltafrPPVSpec(specificity[i],prevalence,PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~PPV,type="l",main=c("Delta vs. Positive Predictive Value","Given Different Values of Specificity",paste("Prevalence =",prevalence)),xlab="Positive Predictive Value",ylab="Delta",lty=LTY[i], ylim=c(0,6),col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawDeltaPPVPrevSpec <- function(ppv,  prevalence, specificity) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
 
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    Delta = calculateDeltafrPPVSpec(specificity,prevalence[i],PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~PPV,type="l",main=c("Delta vs. Positive Predictive Value","Given Different Values of Prevalence",paste("Specificity =",specificity)),xlab="Positive Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawPPVSpecDeltaPrev <- function(spec, delta, prevalence) {
  

  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)


    PPV =calculatePPVfrSpecSens(Specificity,calculateSensfrSpec(Specificity,Delta[i]),prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=range(Specificity),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawPPVDeltaSpecPrev <- function(delta, specificity, prevalence) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)

  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(specificity[i],calculateSensfrSpec(specificity[i],Delta),prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Specificity",paste("Prevalence =",prevalence)),ylim=c(0,1),xlab="Delta",ylab="Positive Predictive Value",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPPVDeltaPrevSpec <- function(delta, prevalence, specificity) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)

  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(specificity,calculateSensfrSpec(specificity,Delta),prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Prevalence",paste("Specificity =",specificity)),ylim=c(0,1),xlab="Delta",ylab="Positive Predictive Value",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DeltaPrevSpec <- function(delta, prevalence,specificity) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    PPV=calculatePPV(specificity, Delta, Val)
    #PPV <- ((1 - pnorm(qnorm(specificity)-Delta))/(1-specificity))*Val[i]/(1+((1 - pnorm(qnorm(specificity)-Delta))/(1-specificity))*Val[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Prevalence",paste("Specificity =",specificity)),ylim=c(min(PPV),1),xlab="Delta",ylab="Positive Predictive Value",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawPPVPrevSpecDelta <- function(prev, specificity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)

  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(specificity[i],calculateSensfrSpec(specificity[i],delta),Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of","Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawPPVPrevDeltaSpec <- function(prev, delta, specificity) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){

    PPV = calculatePPVfrSpecSens(specificity,calculateSensfrSpec(specificity,delta[i]),Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of Delta",paste("Specificity =",specificity)),xlab="Prevalence",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}

DrawPPVSpecPrevDelta <- function(spec, prevalence, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)

  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(Specificity,calculateSensfrSpec(Specificity,delta),prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity","Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}


DrawPrevPPVSpecDelta <- function(ppv, specificity, delta) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- calculateSensfrSpec(specificity[i],delta)
    Prevalence =calculatePrevalencefrPPV(PPV,specificity[i],sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of","Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Positive Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawPrevPPVDeltaSpec <- function(ppv, delta, specificity) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- calculateSensfrSpec(specificity,delta[i])
    Prevalence =calculatePrevalencefrPPV(PPV,specificity,sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Positive Predictive Value vs. Prevalence","Given Different Values of Delta",paste("Specificity =",specificity)),xlab="Positive Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}

DrawPrevDeltaPPVSens <- function(delta,PPV, sensitivity) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity = calculateSpecfrSens(sensitivity,Delta)
    Prevalence =calculatePrevalencefrPPV(PPV[i],specificity,sensitivity)   
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of","Positive Predictive Value",paste("Sensitivity=",sensitivity)),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawPrevPPVDeltaSens <- function(ppv, delta, sensitivity) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity = calculateSpecfrSens(sensitivity,delta[i])
    Prevalence =calculatePrevalencefrPPV(PPV,specificity[i],sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Prevalence vs. Positive Predictive Value","Given Different Values of Delta", paste("Sensitivity=",sensitivity)),xlab="Positive Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}


DrawPrevSpecPPVDelta <- function(spec, PPV, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity = calculateSensfrSpec(Specificity,delta)
    Prevalence = calculatePrevalencefrPPV(PPV[i], Specificity,sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of","Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}


DrawDeltaSensPPVPrev <- function(sens,  PPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta = calculateDeltafrPPV(Sensitivity,prevalence,PPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of","Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawPPVSensDeltaPrev <- function(sens, delta, prevalence) {

  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(Sensitivity,Delta[i]),Sensitivity,prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,1),xlim=range(Sensitivity),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}



DrawPPVDeltaSensPrev <- function(delta, sensitivity, prevalence) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(sensitivity[i],Delta),sensitivity[i],prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawPPVDeltaPrevSens <- function(delta, prevalence, sensitivity) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(sensitivity,Delta),sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Delta,type="l",main=c("Positive Predictive Value vs. Delta","Given Different Values of Prevalence",paste("Sensitivity =",sensitivity)),xlab="Delta",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}


DrawPPVPrevSensDelta <- function(prev, sensitivity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(sensitivity[i],delta),sensitivity[i],Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPPVPrevDeltaSens <- function(prev, delta,sensitivity) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(sensitivity,delta[i]),sensitivity,Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Prevalence,type="l",main=c("Positive Predictive Value vs. Prevalence", "Given Different Values of Delta",paste("Sensitivity =",sensitivity)),xlab="Prevalence",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}



DrawPrevSensPPVDelta <- function(sens, PPV, delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity <- calculateSpecfrSens(Sensitivity,delta)
    Prevalence=calculatePrevalencefrPPV(PPV[i],specificity,Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity", "Given Different Values of","Positive Predictive Value",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawPPVSensPrevDelta <- function(sens, prevalence, delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)

  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(calculateSpecfrSens(Sensitivity,delta),Sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity", "Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}


DrawPPVSensSpecPrev <- function(sens,specificity, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(specificity[i],Sensitivity,prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity", "Given Different Values of Specificity",paste("Prevalence = ",prevalence)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}



DrawPPVSensPrevSpec <- function(sens,prevalence, specificity) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(specificity,Sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Sensitivity,type="l",main=c("Positive Predictive Value vs. Sensitivity", "Given Different Values of Prevalence",paste("Specificity = ",specificity)),xlab="Sensitivity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawSensPPVSpecPrev <- function(ppv, specificity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity=calculateSensfrPPV(PPV,prevalence,specificity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~PPV,type="l",main=c("Sensitivity vs. Positive Predictive Value", "Given Different Values of Specificity",paste("Prevalence = ",prevalence)),xlab="Positive Predictive Value",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSensPPVPrevSpec <- function(ppv, prevalence,specificity) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity =calculateSensfrPPV(PPV,prevalence[i],specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~PPV,type="l",main=c("Sensitivity vs. Positive Predictive Value", "Given Different Values of Prevalence",paste("Specificity = ",specificity)),xlab="Positive Predictive Value",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSpecSensPPVPrev <- function(sens, PPV,prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity =calculateSpecfrPPV(PPV[i],prevalence, Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity", "Given Different Values of","Positive Predictive Value",paste("Prevalence = ",prevalence)),xlab="Sensitivity",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
  
}

DrawPPVSpecSensPrev <- function(spec, sensitivity,prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(Specificity,sensitivity[i],prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity", "Given Different Values of Sensitivity",paste("Prevalence = ",prevalence)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawPPVSpecPrevSens <- function(spec, prevalence, sensitivity) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(Specificity,sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity", "Given Different Values of Prevalence",paste("Sensitivity = ",sensitivity)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawPPVPrevSensSpec <- function(prevalence, sensitivity,spec) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(spec,sensitivity[i],Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity", "Given Different Values of Sensitivity", paste("Specificity = ",spec)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawPPVPrevSpecSens <- function(prevalence, spec,sensitivity) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    PPV = calculatePPVfrSpecSens(spec[i],sensitivity,Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(PPV~Specificity,type="l",main=c("Positive Predictive Value vs. Specificity", "Given Different Values of Specificity",paste("Sensitivity = ",sensitivity)),xlab="Specificity",ylab="Positive Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=PPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSpecPPVSensPrev <- function(ppv, sensitivity, prevalence) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrPPV(PPV,prevalence,sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~PPV,type="l",main=c("Specificity vs. Positive Predictive Value", "Given Different Values of Sensitivity", paste("Prevalence = ",prevalence)),xlab="Positive Predictive Value",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawSpecPPVPrevSens <- function(ppv, prevalence,sensitivity) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrPPV(PPV,prevalence[i],sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~PPV,type="l",main=c("Specificity vs. Positive Predictive Value", "Given Different Values of Prevalence", paste("Sensitivity = ",sensitivity)),xlab="Positive Predictive Value",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawcNPVPrevSpecDelta <- function(prev, specificity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)

  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(specificity[i],calculateSensfrSpec(specificity[i],delta),Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of Negative Predictive Value vs. Prevalence","Given Different Values of Specificity",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawcNPVPrevDeltaSpec <- function(prev, delta, specificity) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(specificity,calculateSensfrSpec(specificity,delta[i]),Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of Negative Predictive Value vs. Prevalence","Given Different Values of Delta",paste("Specificity =",specificity)),xlab="Prevalence",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawcNPVSpecPrevDelta <- function(spec, prevalence, delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)

  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(Specificity,calculateSensfrSpec(Specificity,delta),prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of Negative Predictive Value vs. Specificity","Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Specificity",ylab="Complement of Negative Predictive Value",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawcNPVSpecDeltaPrev <- function(spec,delta, prevalence) {
  
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV = calculatecNPVfrSpecSens(Specificity,calculateSensfrSpec(Specificity,delta[i]),prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of Negative Predictive Value vs. Specificity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Complement of Negative Predictive Value",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta") 
  
}




DrawPrevcNPVSpecDelta <- function(cnpv, specificity, delta) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence =calculatePrevalencefrcNPV(cNPV,specificity[i],calculateSensfrSpec(specificity[i],delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Specificity vs. Complement of Negative Predictive Value","Given Different Values of Prevalence",paste(expression(Delta),"=",delta)),xlab="Complement of Negative Predictive Value",ylim=c(0,1),ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPrevcNPVDeltaSpec <- function(cnpv, delta,specificity) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations) {
    Prevalence =calculatePrevalencefrcNPV(cNPV,specificity,calculateSensfrSpec(specificity,delta[i]))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. Complement of Negative Predictive Value","Given Different Values of Delta",paste("Specificity =",specificity)),xlab="Complement of Negative Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta") 
  
}

DrawDeltaPrevSpeccNPV <- function(prev, spec, cNPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSpec(spec[i],Prevalence,cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Specificity", paste("cNPV = ",cNPV)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawDeltaPrevcNPVSpec <- function(prev, cNPV, spec) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSpec(spec,Prevalence,cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of cNPV", paste("Specificity = ",spec)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawPrevDeltaSpeccNPV <- function(delta, spec, cnpv) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence =calculatePrevalencefrcNPV(cnpv,spec[i],calculateSensfrSpec(spec[i],Delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Specificity",paste("cNPV = ",cnpv)),ylim=c(0,1),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPrevDeltacNPVSpec <- function(delta, cnpv, spec) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(cnpv)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- 1- pnorm(qnorm(spec)-Delta)
    Prevalence = calculatePrevalencefrcNPV(cnpv[i],spec,calculateSensfrSpec(spec,Delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of cNPV",paste("Specificity = ",spec)),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cnpv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltaSpecPrevcNPV <- function(spec, prev, cNPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSpec(Specificity,prev[i],cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of Prevalence", paste("cNPV = ",cNPV)),xlab="Specificity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPrevSpecDeltacNPV <- function(spec, delta,cnpv) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrcNPV(cnpv,Specificity,calculateSensfrSpec(Specificity,delta[i]))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Delta", paste("cNPV = ",cnpv)),ylim=c(0,1),xlab="Specificity",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawPrevSpeccNPVDelta <- function(spec,cnpv,delta) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cnpv)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrcNPV(cnpv[i],Specificity,calculateSensfrSpec(Specificity,delta))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of cNPV",paste(expression(Delta)," = ",delta)),ylim=c(0,1),xlab="Specificity",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cnpv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawPrevSpecDeltaPPV <- function(spec, delta, PPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- calculateSensfrSpec(Specificity,delta[i])
    Prevalence =calculatePrevalencefrPPV(PPV,Specificity,sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Delta", paste("PPV = ",PPV)),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawDeltaPrevSpecPPV <- function(prev,  spec,PPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPVSpec(spec[i],Prevalence,PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Specificity", paste("PPV = ",PPV)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
}

DrawDeltaPrevPPVSpec <- function(prev, PPV, spec) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPVSpec(spec,Prevalence,PPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of","Positive Predictive Value", paste("Specificity = ",spec)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}


DrawPrevDeltaSpecPPV <- function(delta, spec, PPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- calculateSpecfrSens(spec[i],Delta)
    Prevalence=calculatePrevalencefrPPV(PPV,spec[i],sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Specificity",paste("PPV = ",PPV)),ylim=c(0,1),xlab="Delta",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPrevDeltaPPVSpec <- function(delta, PPV, spec) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    sensitivity <- calculateSensfrSpec(spec,Delta)
    Prevalence =calculatePrevalencefrPPV(PPV[i],spec,sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of PPV", paste("Specificity = ",spec)),ylim=c(0,1),xlab="Delta",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawDeltaSpecPrevPPV <- function(spec, prev, PPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPVSpec(Specificity,prev[i],PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of Prevalence", paste("PPV = ",PPV)),xlab="Specificity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawDeltaSpecPPVPrev <- function(spec, PPV, prev) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPVSpec(Specificity,prev,PPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of","Positive Predictive Value", paste("Prevalence = ",prevalence)),xlab="Specificity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawcNPVDeltaSensPrev <- function(delta, sensitivity, prevalence) {
  Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){


    cNPV = calculatecNPVfrSpecSens(calculateSpecfrSens(sensitivity[i],Delta),sensitivity[i],prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Sensitivity",paste("Prevalence =",prevalence)),xlab="Delta",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawcNPVDeltaPrevSens <- function(delta, prevalence, sensitivity) {
  Delta<-seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV <= calculatecNPVfrSpecSens(calculateSpecfrSens(sensitivity,Delta),sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Delta,type="l",main=c("Complement of Negative Predictive Value vs. Delta","Given Different Values of Prevalence",paste("Sensitivity =",sensitivity)),xlab="Delta",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}


DrawcNPVSensDeltaPrev <- function(sens, delta, prevalence) {
  
  Delta<-as.vector(delta)
  
  Iterations <- 1:length(Delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)

    cNPV = calculatecNPVfrSpecSens(calculateSpecfrSens(Sensitivity,Delta[i]),Sensitivity,prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of Negative Predictive Value vs. Sensitivity","Given Different Values of Delta",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Complement of Negative Predictive Value",xlim=range(Sensitivity),ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title=expression(Delta)) 
  
}

DrawDeltaSenscNPVPrev <- function(sens, cNPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta =calculateDeltafrcNPVSens(Sensitivity,prevalence,cNPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of","complement of the Negative Predictive Value",paste("Prevalence =",prevalence)),xlab="Sensitivity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV") 
  
}

DrawDeltacNPVSensPrev <- function(cnpv, sensitivity, prevalence) {
  cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Val <- prevalence/(1+prevalence)
    
    Delta= calculateDeltafrcNPVSens(sensitivity[i],prevalence,cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Sensitivity", paste("Prevalence = ",prevalence)),xlab="Complement of Negative Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawDeltacNPVPrevSens <- function(cnpv, prevalence, sensitivity) {
  cNPV<-seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    
    Delta=calculateDeltafrcNPVSens(sensitivity,prevalence[i],cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~cNPV,type="l",main=c("Delta vs. Complement of Negative Predictive Value","Given Different Values of Prevalence",paste("Sensitivity =",sensitivity)),xlab="Complement of Negative Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence") 
  
}

DrawcNPVPrevSensDelta <- function(prev, sensitivity, delta) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV = calculatecNPVfrSpecSens(calculateSpecfrSens(sensitivity[i],delta),sensitivity[i],Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of the Negative Predictive Value vs. Prevalence","Given Different Values of Sensitivity",paste(expression(Delta),"=",delta)),xlab="Prevalence",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence, lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawcNPVPrevDeltaSens <- function(prev, delta, sensitivity) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){

    cNPV <- calculatecNPVfrSpecSens(calculateSpecfrSens(sensitivity,delta[i]),sensitivity,Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of the Negative Predictive Value vs. Prevalence","Given Different Values of Delta",paste("Sensitivity =",sensitivity)),xlab="Prevalence",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence, lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}

DrawcNPVSensPrevDelta <- function(sens, prevalence, delta) {

  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)

  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(calculateSpecfrSens(Sensitivity,delta),Sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of the Negative Predictive Value vs. Sensitivity","Given Different Values of Disease Prevalence",paste(expression(Delta),"=",delta)),xlab="Sensitivity",ylab="Complement of Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}


DrawPrevcNPVSensDelta <- function(cnpv, sensitivity, delta) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrcNPV(cNPV,calculateSpecfrSens(sensitivity[i],delta),sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. Complement of the Negative Predictive Value","Given Different Values of Sensitivity",paste(expression(Delta),"=",delta)),xlab="Complement of the Negative Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV, lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPrevcNPVDeltaSens <- function(cnpv, delta, sensitivity) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){

    Prevalence =calculatePrevalencefrcNPV(cNPV,calculateSpecfrSens(sensitivity,delta[i]),sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. Complement of the Negative Predictive Value","Given Different Values of Delta",paste("Sensitivity =",sensitivity)),xlab="Complement of the Negative Predictive Value",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=cNPV, lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}



DrawDeltaPrevSenscNPV <- function(prev, sens,cNPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSens(sens[i],Prevalence,cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Sensitivity",paste("cNPV = ",cNPV)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawDeltaPrevcNPVSens <- function(prev, cNPV, sens) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSens(sens,Prevalence,cNPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of cNPV",paste("Sensitivity = ",sensitivity)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawDeltaSensPrevcNPV <- function(sens, prev, cNPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrcNPVSens(Sensitivity,prev[i],cNPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of Prevalence", paste("cNPV = ",cNPV)),xlab="Sensitivity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawPrevDeltaSensPPV <- function(delta, sens, PPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity <- calculateSpecfrSens(sens[i],Delta)
    Prevalence =calculatePrevalencefrPPV(PPV,specificity,sens[i])   
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Sensitivity", paste("PPV = ",PPV)),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}


DrawPrevSensDeltacNPV <- function(sens, delta, cNPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cNPV,calculateSpecfrSens(Sensitivity,delta[i]),Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of Delta",paste("cNPV = ",cNPV)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}

DrawPrevSenscNPVDelta <- function(sens, cNPV, delta) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){

    Prevalence = calculatePrevalencefrcNPV(cNPV[i],calculateSpecfrSens(Sensitivity,delta),Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of cNPV",paste(expression(Delta)," = ",delta)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawPrevSensDeltaPPV <- function(sens, delta, PPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity <- calculateSpecfrSens(Sensitivity,delta[i])
    Prevalence = calculatePrevalencefrPPV(PPV,specificity[i],Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of Delta",paste("PPV = ",PPV)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Delta")
  
}




DrawPrevPPVSensDelta <- function(ppv, sens, delta) {
  
  PPV <- seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    specificity = calculateSpecfrSens(sens[i],delta)
    Prevalence = calculatePrevalencefrPPV(PPV,specificity[i],sens[i])  
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Prevalence vs. PPV","Given Different Values of Sensitivity",paste(expression(Delta)," = ",delta)),xlab="PPV",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPrevDeltaSenscNPV <- function(delta, sens, cNPV) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrcNPV(cNPV,calculateSpecfrSens(sens[i],Delta),sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of Sensitivity",paste("cNPV = ",cNPV)),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawPrevDeltacNPVSens <- function(delta, cNPV, sens) {
  
  Delta <- seq(from=min(delta),to=max(delta),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrcNPV(cNPV[i],calculateSpecfrSens(sens,Delta),sens)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Delta,type="l",main=c("Prevalence vs. Delta","Given Different Values of cNPV",paste("Sensitivity = ",sensitivity)),xlab="Delta",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Delta,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawDeltaPrevSensPPV <- function(prev, sens,  PPV) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPV(sens[i],Prevalence,PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of Sensitivity",paste("PPV = ",PPV)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

DrawDeltaPrevPPVSens <- function(prev, PPV, sens) {
  
  Prevalence <- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPV(sens,Prevalence,PPV[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Prevalence,type="l",main=c("Delta vs. Prevalence","Given Different Values of","Positive Predictive Value",paste("Sensitivity = ",sens)),xlab="Prevalence",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawDeltaPPVPrevSens <- function(PPV, prev, sens) {
  
  PPV <- seq(from=min(PPV),to=max(PPV),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPV(sens,prev[i],PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~PPV,type="l",main=c("Delta vs. PPV","Given Different Values of Prevalence",paste("Sensitivity = ",sens)),xlab="PPV",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}



DrawDeltaSensPrevPPV <- function(sens, prev, PPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPV(Sensitivity,prev[i],PPV)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Sensitivity,type="l",main=c("Delta vs. Sensitivity","Given Different Values of Prevalence",paste("PPV = ",PPV)),xlab="Sensitivity",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}


DrawSpecSensPrevcNPV <- function(sens, prev, cNPV) {
  
  Sensitivity<- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV,prev[i],Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity","Given Different Values of Prevalence", paste("cNPV = ",cNPV)),ylim=c(0,1),xlab="Sensitivity",ylab="Specificity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensSpecPrevcNPV <- function(spec, prev, cNPV) {
  
  Specificity<- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prev)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity=calculateSensfrcNPV(cNPV,prev[i],Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity","Given Different Values of Prevalence",paste("cNPV = ",cNPV)),ylim=c(0,1),xlab="Specificity",ylab="Sensitivity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prev,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
}

DrawSensSpeccNPVPrev <- function(spec, cNPV,prev) {
  
  Specificity<- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity = calculateSensfrcNPV(cNPV[i],prev,Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity","Given Different Values of cNPV",paste("Prevalence = ",prev)),ylim=c(0,1),xlab="Specificity",ylab="Sensitivity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
}

DrawcNPVSensSpecPrev <- function(sens, specificity, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(specificity[i],Sensitivity,prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of the Negative Predictive Value vs. Sensitivity", "Given Different Values of Specificity",paste("Prevalence = ",prevalence)),xlab="Sensitivity",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawcNPVSensPrevSpec <- function(sens, prevalence, specificity) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV =calculatecNPVfrSpecSens(specificity,Sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Sensitivity,type="l",main=c("Complement of the Negative Predictive Value vs. Sensitivity", "Given Different Values of Prevalence", paste("Specificity = ",specificity)),xlab="Sensitivity",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawcNPVPrevSensSpec <- function(prevalence,sens,specificity) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(specificity,sens[i],Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of the Negative Predictive Value vs. Prevalence", "Given Different Values of Sensitivity",paste("Specificity = ",specificity)),xlab="Prevalence",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawcNPVPrevSpecSens <- function(prevalence,specificity,sens) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV=calculatecNPVfrSpecSens(specificity[i],sens,Prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Prevalence,type="l",main=c("Complement of the Negative Predictive Value vs. Prevalence", "Given Different Values of Specificity",paste("Sensitivity = ",sens)),xlab="Prevalence",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSenscNPVSpecPrev <- function(cnpv, prevalence, specificity) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity =calculateSensfrcNPV(cNPV,prevalence[i],specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~cNPV,type="l",main=c("Sensitivity vs. Complement of the Negative Predictive Value", "Given Different Values of Prevalence", paste("Specificity = ",specificity)),xlab="Complement of the Negative Predictive Value",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawSenscNPVPrevSpec <- function(cnpv, specificity,prevalence) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity = calculateSensfrcNPV(cNPV, prevalence,specificity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~cNPV,type="l",main=c("Sensitivity vs. Complement of the Negative Predictive Value", "Given Different Values of Specificity", paste("Prevalence = ",prevalence)),xlab="Complement of the Negative Predictive Value",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSensPrevcNPVSpec <- function(prevalence, cNPV,specificity) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity = calculateSensfrcNPV(cNPV[i],Prevalence,specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence", "Given Different Values of cNPV",paste("Specificity = ",specificity)),xlab="Prevalence",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
  
}

DrawSensPrevSpeccNPV <- function(prevalence, specificity,cNPV) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  
  Iterations <- 1:length(specificity)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity = calculateSensfrcNPV(cNPV,Prevalence,specificity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence", "Given Different Values of Specificity",paste("cNPV = ",cNPV)),xlab="Prevalence",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=specificity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
  
  
}

DrawSpecSenscNPVPrev <- function(sens, cNPV, prevalence) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV[i],prevalence,Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity", "Given Different Values of","complement of the Negative Predictive Value", paste("Prevalence = ",prevalence)),ylim=c(0,1),xlab="Sensitivity",ylab="Specificity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
  
}

DrawSpeccNPVSensPrev <- function(cnpv, sensitivity, prevalence) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV,prevalence,sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~cNPV,type="l",main=c("Specificity vs. Complement of the Negative Predictive Value", "Given Different Values of Sensitivity", paste("Prevalence = ",prevalence)),xlab="Complement of the Negative Predictive Value",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawSpeccNPVPrevSens <- function(cnpv, prevalence,sensitivity) {
  
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV,prevalence[i],sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~cNPV,type="l",main=c("Specificity vs. Complement of the Negative Predictive Value", "Given Different Values of Prevalence", paste("Sensitivity = ",sensitivity)),xlab="Complement of the Negative Predictive Value",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=cNPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}

DrawSpecPrevSenscNPV <- function(prevalence, sensitivity, cNPV) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV, Prevalence, sensitivity[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence", "Given Different Values of Sensitivity", paste("cNPV = ",cNPV)),xlab="Prevalence",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawSpecPrevcNPVSens <- function(prevalence, cNPV, sensitivity) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(cNPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrcNPV(cNPV[i],Prevalence,sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence", "Given Different Values of cNPV", paste("Sensitivity = ",sensitivity)),xlab="Prevalence",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=cNPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")
  
  
}

DrawcNPVSpecSensPrev <- function(spec, sensitivity, prevalence) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(sensitivity)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(Specificity,sensitivity[i],prevalence)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of the Negative Predictive Value vs. Specificity", "Given Different Values of Sensitivity",paste("Prevalence = ",prevalence)),xlab="Specificity",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
   }
    else 
      lines(y=cNPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sensitivity,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}


DrawcNPVSpecPrevSens <- function(spec, prevalence,sensitivity) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){
    cNPV = calculatecNPVfrSpecSens(Specificity,sensitivity,prevalence[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(cNPV~Specificity,type="l",main=c("Complement of the Negative Predictive Value vs. Specificity", "Given Different Values of Prevalence",paste("Sensitivity = ",sensitivity)),xlab="Specificity",ylab="Complement of the Negative Predictive Value",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=cNPV, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
  
  
}


DrawPrevSpecSenscNPV <- function(spec,sens,cnpv) {
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cnpv, Specificity, sens[i])
    #Prevalence <- cnpv/(1-cnpv)*(Specificity/(1-sens[i]))/(1-(cnpv/(1-cnpv))*(Specificity/(1-sens[i])))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity", "Given Different Values of Sensitivity",paste("cNPV = ",cnpv)),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
  
}

DrawPrevSpeccNPVSens <- function(spec,cnpv,sens) {
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  Iterations <- 1:length(cnpv)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cnpv[i], Specificity, sens)
    #Prevalence <- cnpv[i]/(1-cnpv[i])*(Specificity/(1-sens))/(1-(cnpv[i]/(1-cnpv[i]))*(Specificity/(1-sens)))
if(i==1) {
  par(mar=c(5.1, 4.1, 4.1, 9.5))
  plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity", "Given Different Values of cNPV",paste("Sensitivity = ",sens)),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}
else
  lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }

legend(lty=LTY,legend=cnpv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")


}

DrawPrevSensSpeccNPV <- function(sens,spec,cnpv) {
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cnpv, spec[i], Sensitivity)
    #Prevalence <- cnpv/(1-cnpv)*(spec[i]/(1-Sensitivity))/(1-(cnpv/(1-cnpv))*(spec[i]/(1-Sensitivity)))
if(i==1) {
  par(mar=c(5.1, 4.1, 4.1, 9.5))
  plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity", "Given Different Values of Specificity",paste("cNPV = ",cnpv)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}
else
  lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }

legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")


}

DrawPrevSenscNPVSpec <- function(sens,cnpv,spec) {
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  Iterations <- 1:length(cnpv)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cnpv[i], spec, Sensitivity)
    #Prevalence <- cnpv[i]/(1-cnpv[i])*(spec/(1-Sensitivity))/(1-(cnpv[i]/(1-cnpv[i]))*(spec/(1-Sensitivity)))
if(i==1) {
  par(mar=c(5.1, 4.1, 4.1, 9.5))
  plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity", "Given Different Values of", "complement of the Negative Predictive Value",paste("Specificity = ",spec)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}
else
  lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }

legend(lty=LTY,legend=cnpv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="cNPV")


}

DrawPrevcNPVSensSpec <- function(cnpv,sens,spec) {
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cNPV, spec, sens[i])
    #Prevalence <- cNPV/(1-cNPV)*(spec/(1-sens[i]))/(1-(cNPV/(1-cNPV))*(spec/(1-sens[i])))
if(i==1) {
  par(mar=c(5.1, 4.1, 4.1, 9.5))
  plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. the complement of the Negative Predictive Value", "Given Different Values of Sensitivity",paste("Specificity = ",spec)),xlab="cNPV",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}
else
  lines(y=Prevalence, x=cNPV,lty=LTY[i], col = LTY[i])
  }

legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")


}

DrawPrevcNPVSpecSens <- function(cnpv,spec,sens) {
  cNPV <- seq(from=min(cnpv),to=max(cnpv),by=0.00001)
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence=calculatePrevalencefrcNPV(cNPV, spec[i], sens)
    #Prevalence <- cNPV/(1-cNPV)*(spec[i]/(1-sens))/(1-(cNPV/(1-cNPV))*(spec[i]/(1-sens)))
if(i==1) {
  par(mar=c(5.1, 4.1, 4.1, 9.5))
  plot(Prevalence~cNPV,type="l",main=c("Prevalence vs. the complement of the Negative Predictive Value", "Given Different Values of Specificity",paste("Sensitivity = ",sens)),xlab="cNPV",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}
else
  lines(y=Prevalence, x=cNPV,lty=LTY[i], col = LTY[i])
  }

legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")


}

DrawSensPrevPPVSpec <- function(prevalence,PPV,spec) {

  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity = calculateSensfrPPV(PPV[i],Prevalence,spec)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence", "Given Different Values of", "Positive Predictive Value",paste("Specificity =",specificity)),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
}

DrawSensPrevSpecPPV <- function(prevalence,spec,PPV) {
  
  Prevalence <- seq(from=min(prevalence),to=max(prevalence),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Val <- Prevalence/(1-Prevalence)
    Sensitivity =calculateSensfrPPV(PPV,Prevalence,spec[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Prevalence,type="l",main=c("Sensitivity vs. Prevalence", "Given Different Values of Specificity",paste("PPV =",PPV)),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity")
}

DrawSensSpecPPVPrev <- function(spec, PPV, prevalence) {
  Val <- prevalence/(1-prevalence)
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity =calculateSensfrPPV(PPV[i],prevalence,Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity", "Given Different Values of","Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
}

DrawSensSpecPrevPPV <- function(spec, prevalence,PPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(prevalence)
  LTY <- Iterations
  
  for(i in Iterations){

    Sensitivity = calculateSensfrPPV(PPV,prevalence[i],Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity", "Given Different Values of Prevalence",paste("PPV =",PPV)),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=prevalence,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Prevalence")
}

DrawPrevSensSpecPPV <- function(sens,spec,PPV) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence =calculatePrevalencefrPPV(PPV,spec[i],Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of Specificity", paste("PPV = ",PPV)),xlab="Sensitivity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}

DrawPrevSpecSensPPV <- function(spec,sens,PPV) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrPPV(PPV,Specificity,sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of Sensitivity", paste("PPV = ",PPV)),ylim=c(0,1),xlab="Specificity",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawPrevSpecPPVSens <- function(spec,PPV,sens) {
  
  Specificity <- seq(from=min(spec),to=max(spec),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){ 
    Prevalence = calculatePrevalencefrPPV(PPV[i],Specificity,sens)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Specificity,type="l",main=c("Prevalence vs. Specificity","Given Different Values of PPV", paste("Sensitivity = ",sens)),ylim=c(0,1),xlab="Sensitivity",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Specificity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawPrevSensPPVSpec <- function(sens,PPV,spec) {
  
  Sensitivity <- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence =calculatePrevalencefrPPV(PPV[i],spec,Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~Sensitivity,type="l",main=c("Prevalence vs. Sensitivity","Given Different Values of","Positive Predictive Value",paste("Specificity = ",spec)),ylim=c(0,1),xlab="Sensitivity",ylab="Prevalence",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}

DrawPrevPPVSensSpec <- function(PPV,sens,spec) {
  
  PPV <- seq(from=min(PPV),to=max(PPV),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence =calculatePrevalencefrPPV(PPV,spec,sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Prevalence vs. Positive Predictive Value","Given Different Values of Sensitivity",paste("Specificity = ",spec)),xlab="PPV",ylab="Prevalence",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity") 
  
}

DrawPrevPPVSpecSens <- function(PPV,spec,sens) {
  
  PPV <- seq(from=min(PPV),to=max(PPV),by=0.00001)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    Prevalence = calculatePrevalencefrPPV(PPV,spec[i],sens)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Prevalence~PPV,type="l",main=c("Prevalence vs. PPV","Given Different Values of Specificity",paste("Sensitivity = ",sens)),ylim=c(0,1),xlab="Sensitivity",ylab="PPV",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Prevalence, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Specificity") 
  
}


DrawDeltaPPVSensPrev <- function(ppv,sens,prev) {
  
  PPV<-seq(from=min(ppv),to=max(ppv),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta=calculateDeltafrPPV(sens[i], prev, PPV)
    #Delta <- qnorm(1-sens[i]*(prev/(1-prev))*((1-ppv)/ppv) - qnorm(1-sens[i])
                   if(i==1) {
                     par(mar=c(5.1, 4.1, 4.1, 9.5))
                     plot(Delta~PPV,type="l",main=c("Delta vs. Positive Predictive Value","Given Different Values of Sensitvity",paste("Prevalence =",prev)),xlab="Positive Predictive Value",ylab="Delta",ylim=c(0,6),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
                   }
                   else
                     lines(y=Delta, x=PPV,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=ppv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  
}


DrawSpecSensPrevPPV <- function(sens,ppv,prev) {
  
  Sensitivity<- seq(from=min(sens),to=max(sens),by=0.00001)
  
  Iterations <- 1:length(ppv)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrPPV(ppv[i],prev,Sensitivity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Sensitivity,type="l",main=c("Specificity vs. Sensitivity","Given Different Values of","Positive Predictive Value",paste("Prevalence = ",prev)),ylim=c(0,1),xlab="Sensitivity",ylab="Specificity",ylim=c(0,1),lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Sensitivity,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=ppv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}


DrawSpecPrevPPVSens <- function(prev,ppv,sens) {
  
  Prevalence<- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(ppv)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrPPV(ppv[i],Prevalence,sens)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence","Given Different Values of","Positive Predictive Value",paste("Sensitivity = ",sens)),ylim=c(0,1),xlab="Prevalence",ylab="Specificity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=ppv,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  
}

DrawSpecPrevSensPPV <- function(prev,sens,ppv) {
  
  Prevalence<- seq(from=min(prev),to=max(prev),by=0.00001)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    Specificity = calculateSpecfrPPV(ppv,Prevalence,sens[i])
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Specificity~Prevalence,type="l",main=c("Specificity vs. Prevalence","Given Different Values of Sensitivity",paste("PPV = ",ppv)),ylim=c(0,1),xlab="Prevalence",ylab="Specificity",lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Specificity, x=Prevalence,lty=LTY[i], col = LTY[i])
  }
  
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="Sensitivity")
  
}

calculatePrevalencefrcNPV<-function(cnpv, specificity, sensitivity) {
  prevalence <- specificity*(cnpv/(1-cnpv))/(1-sensitivity+specificity*(cnpv/(1-cnpv)))
}

calculatePrevalencefrPPV<-function(ppv,specificity,sensitivity) {
  prevalence <- ppv*(1-specificity)/(sensitivity - ppv*(sensitivity)+ppv - ppv*specificity)      
}

calculateDeltafrPPV<-function(sensitivity, prevalence, ppv) {
  delta <-  qnorm(1-sensitivity*(prevalence/(1-prevalence))*((1-ppv)/ppv)) - qnorm(1-sensitivity) 
}

calculateDeltafrPPVSpec<-function(specificity,prevalence,ppv) {
  delta <- qnorm(specificity) - qnorm(1-(1-specificity)*((1-prevalence)/prevalence)*(ppv/(1-ppv)))
}

calculateDeltafrcNPVSpec<- function(specificity,prevalence,cnpv) {
  sensitivity <- 1-specificity*((1-prevalence)/prevalence)*(cnpv/(1-cnpv))
  delta <- qnorm(specificity) - qnorm(1-sensitivity)
}

calculateDeltafrcNPVSens<- function(sensitivity,prevalence,cnpv) {
  specificity <- (1-sensitivity)/(((1-prevalence)/prevalence)*(cnpv/(1-cnpv)))
  delta <- qnorm(specificity) - qnorm(1-sensitivity)
}


calculateSensfrSpec<- function(specificity,delta) {
  sensitivity <- 1-pnorm(qnorm(specificity)-delta)
}

calculateSpecfrSens<- function(sensitivity,delta) {
  specificity <- pnorm(delta+qnorm(1-sensitivity))
}

calculateSensfrPPV<- function(ppv,prevalence,specificity) {
  sensitivity <- (1-specificity)*(((1-prevalence)/prevalence)*(ppv/(1-ppv)))
}

calculateSensfrcNPV <- function(cnpv,prevalence,specificity) {
  sensitivity <- 1-specificity*((1-prevalence)/prevalence)*(cnpv/(1-cnpv))
}

calculateSpecfrcNPV <- function(cnpv,prevalence,sensitivity) {
  specificity <- (1-sensitivity)/(((1-prevalence)/prevalence)*(cnpv/(1-cnpv)))
}

calculateSpecfrPPV <- function(ppv,prevalence,sensitivity) {
  specificity <- 1-sensitivity*(prevalence/(1-prevalence))*((1-ppv)/ppv)
}

calculatePPVfrSpecSens <- function(specificity,sensitivity,prevalence) {
  PPV <- ((sensitivity/(1-specificity))*(prevalence/(1-prevalence)))/(1+(sensitivity/(1-specificity))*(prevalence/(1-prevalence)))
}

calculatecNPVfrSpecSens <- function(specificity,sensitivity,prevalence) {
  cNPV <- 1 - specificity*(1-prevalence)/(specificity*(1-prevalence) + (1-sensitivity)*prevalence)
}

calculateDeltafrSpecSens <- function(specificity,sensitivity) {
  delta <- qnorm(specificity) - qnorm(1-sensitivity)
}
