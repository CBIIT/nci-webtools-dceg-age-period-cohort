RiskFromROC <- function(data, specificity, prevalence) {
  x <- data[,1][data[,2] == 0]
  y <- data[,1][data[,2] == 1]
  V <- data[,1]
  Specificity <- as.vector(c(rep(NA, times=length(V))))
  Sensitivity <- as.vector(c(rep(NA, times=length(V))))
  for(i in 1:length(V)){
    Specificity[i] <- sum(x < V[i])/length(x)
    Sensitivity[i] <- 1 - sum(y < V[i])/length(y)
  }
  
  contmean <- mean(x)
  casmean <- mean(y)
  stdev <- c(NA,NA)
  stdev[1] <- sqrt(mean((x - contmean)^2))
  stdev[2] <- sqrt(mean((y - casmean)^2))
  contstderr <- sqrt((stdev[1])^2/length(x))
  casstderr <- sqrt((stdev[2])^2/length(y))
  contn <- length(x)
  casn <- length(y)
  
  a <- c(casmean,casstderr,casn)
  b <- c(contmean,contstderr,contn)
  
  var<-as.vector(c(stdev[1]^2,stdev[2]^2))
  
  coefvar<-as.vector(c(stdev[1]/a[1],stdev[2]/b[1]))
  
  n<-a[3]+b[3]
  
  mean<-(a[1]*a[3]+b[1]*b[3])/n
  
  overallvar<-(var[1]*a[3]+var[2]*b[3])/n
  
  overallcoefvar<-sqrt(overallvar)/mean
  
  diff<-a[1]-b[1]
  
  cdata <- matrix(c(Specificity,Sensitivity),ncol=2,nrow=length(Specificity))
  c1 <- cdata[,1][cdata[,1] >= 0.95]
  c2 <- cdata[,2][cdata[,1] >= 0.95]
  d <- as.vector(c(rep(NA, times=length(c1))))
  for(i in 1:length(c1)) {
    d[i] <- qnorm(c1[i]) - qnorm(1-c2[i])
  }
  d <- d[!is.na(d)]
  d <- d[!is.infinite(d)]
  delta <- mean(d)
  delta
  
  library(pROC)
  pred <- roc(data[,2],data[,1])
  auc <- pred$auc
  auc
  
  data1<-matrix(sigfigures(c(a[1],b[1],mean,a[2],b[2],NA,a[3],b[3],n,stdev[1],stdev[2],NA,var[1],var[2],overallvar,coefvar[1],coefvar[2],overallcoefvar,NA,NA,diff,NA,NA,delta,NA,NA,auc)),ncol=3,nrow=9,byrow=T)
  rownames(data1)<-c("Mean","Standard Error","N","Standard Deviation","Variance","Coefficient of Variation (CV)","Difference in means","Delta","AUC")
  colnames(data1)<-c("Cases","Controls","Overall")
  T1<-as.table(data1)
  
  ##sensitivity given specificity table
  vecspec <- as.vector(specificity)
  prev <- as.vector(prevalence)
  sens<-as.vector(c(rep(NA,times=length(vecspec))))
  for(i in 1:length(vecspec)) {
    sens[i]<-1-pnorm(qnorm(vecspec[i],0,1)-delta,0,1,1)
  }
  
  lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
  lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
  for(i in 1:length(vecspec)) {
    lrplus[i]<-sens[i]/(1-vecspec[i])
    lrminus[i]<-vecspec[i]/(1-sens[i])
  }
  
  data2<-matrix(sigfigures(c(vecspec,sens,lrplus,lrminus)),nrow=length(vecspec),ncol=4) ##Sensitivity Specificity table
  colnames(data2)<-c("Specificity", "Sensitivity","LR+","LR-")
  T2<-as.table(data2)
  
  val<-as.vector(c(rep(NA,times=length(prev))))
  for(i in 1:length(prev)) {
    val[i]<-prev[i]/(1-prev[i])
  }
  
  ab<-matrix(c(rep(NA,times=length(val)*length(lrplus))),nrow=length(lrplus),ncol=length(val)) ##PPV table
  ca<-matrix(c(rep(NA,times=length(val)*length(lrminus))),nrow=length(lrminus),ncol=length(val)) ##cNPV table
  for(i in 1:length(val)) {
    for(j in 1:length(lrplus)) {
      ab[j,i]<-sigfigures(lrplus[j]*val[i]/(1+lrplus[j]*val[i]))
      ca[j,i]<-sigfigures((val[i]/lrminus[j])/(1+val[i]/lrminus[j]))
    }
  }
  
  data3 <- ab
  colnames(data3)<-prevalence
  T3<-as.table(data3)
  
  data4 <- ca
  colnames(data4)<-prevalence
  T4<-as.table(data4)
  
  ##PPV-cNPV table
  pc<-matrix(c(rep(NA,times=length(val)*length(lrminus))),nrow=length(lrminus),ncol=length(val))
  for(i in 1:length(val)) {
    for(j in 1:length(lrminus)) {
      pc[j,i]<-sigfigures(ab[j,i]-ca[j,i])
    }
  }
  
  data5<- pc
  colnames(data5)<-prevalence
  T5<-as.table(data5)
  
  progba<-matrix(c(rep(NA,times=length(prev)*length(sens))),nrow=length(sens),ncol=length(prev)) ##Program-based table
  pba<-matrix(c(rep(NA,times=length(prev)*length(lrplus))),nrow=length(lrplus),ncol=length(prev)) ##PPV-based table
  for(i in 1:length(prev)) {
    for(j in 1:length(sens)) {
      progba[j,i]<-sigfigures(1000*prev[i]*sens[j])
      pba[j,i]<-sigfigures(1000*ab[j,i])
    }
  }
  
  data6<-progba
  colnames(data6)<-prevalence
  T6<-as.table(data6)
  
  data7<-pba
  colnames(data7)<-prevalence
  T7<-as.table(data7)
  
  sb<-as.vector(c(rep(NA,times=length(sens)))) ##Sensitivity-Based table
  for(i in 1:length(sens)) {
    sb[i]<-sigfigures(1000*sens[i])
  }
  
  data8<-matrix(rep(sb,times=length(prev)),nrow=length(sens),ncol=length(prev),byrow=F)
  colnames(data8)<-prevalence
  T8<-as.table(data8)
  
  ##Dominated by specificity for rare disease table
  dsa<-matrix(c(rep(NA,times=length(prev)*length(vecspec))),nrow=length(vecspec),ncol=length(prev))
  for(i in 1:length(prev)) {
    for(j in 1:length(vecspec)) {
      dsa[j,i]<-sigfigures(1000*(prev[i]*sens[j]+(1-prev[i])*(1-vecspec[j])))
    }
  }
  
  data9 <- dsa
  colnames(data9)<-prevalence
  T9<-as.table(data9)
  
  compdata<-list("Delta"=T1,"Sensitivity Given Specificity"=T2,"PPV"=T3,"cNPV"=T4,"PPV-cNPV"=T5,"Program-Based"=T6,"PPV-Based"=T7,"Sensitivity-Based"=T8,"Dominated by Specificity for a Rare Disease"=T9)
  compdata
}

DrawRawROC <- function(data) {
  library(pROC)
  pred <- roc(data[,2],data[,1])
  
  plot(pred,main="ROC curve, Sensitivity vs. Specificity",xlab="Specificity",ylab="Sensitivity",pch=19,font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
}

deltaspecppv <- function (cases,controls,specificity,prevalence) {
  
  ##delta table
  a<-as.vector(cases)
  b<-as.vector(controls)
  vecspec<-as.vector(specificity)
  prev<-as.vector(prevalence)
  
  stdev<-as.vector(c(a[2]*sqrt(a[3]),b[2]*sqrt(b[3])))
  
  var<-as.vector(c(stdev[1]^2,stdev[2]^2))
  
  coefvar<-as.vector(c(stdev[1]/a[1],stdev[2]/b[1]))
  
  n<-a[3]+b[3]
  
  mean<-(a[1]*a[3]+b[1]*b[3])/n
  
  overallvar<-(var[1]*a[3]+var[2]*b[3])/n
  
  overallcoefvar<-sqrt(overallvar)/mean
  
  diff<-a[1]-b[1]
  
  delta<-abs(diff/sqrt(overallvar))
  
  auc<-AUCFromDelta(delta)
  
  data1<-matrix(sigfigures(c(a[1],b[1],mean,a[2],b[2],NA,a[3],b[3],n,stdev[1],stdev[2],NA,var[1],var[2],overallvar,coefvar[1],coefvar[2],overallcoefvar,NA,NA,diff,NA,NA,delta, NA, NA, auc)),ncol=3,nrow=9,byrow=T)
  rownames(data1)<-c("Mean","Standard Error","N","Standard Deviation","Variance","Coefficient of Variation (CV)","Difference in means","Delta", "AUC")
  colnames(data1)<-c("Cases","Controls","Overall")
  T1<-as.table(data1)
  
  ##sensitivity given specificity table
  sens<-as.vector(c(rep(NA,times=length(vecspec))))
  for(i in 1:length(vecspec)) {
    sens[i]<-1-pnorm(qnorm(vecspec[i],0,1)-delta,0,1,1)
  }
  
  lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
  lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
  for(i in 1:length(vecspec)) {
    lrplus[i]<-sens[i]/(1-vecspec[i])
    lrminus[i]<-vecspec[i]/(1-sens[i])
  }
  
  data2<-matrix(sigfigures(c(vecspec,sens,lrplus,lrminus)),nrow=length(vecspec),ncol=4) ##Sensitivity Specificity table
  colnames(data2)<-c("Specificity", "Sensitivity","LR+","LR-")
  T2<-as.table(data2)
  
  val<-as.vector(c(rep(NA,times=length(prev))))
  for(i in 1:length(prev)) {
    val[i]<-prev[i]/(1-prev[i])
  }
  
  ab<-matrix(c(rep(NA,times=length(val)*length(lrplus))),nrow=length(lrplus),ncol=length(val)) ##PPV table
  ca<-matrix(c(rep(NA,times=length(val)*length(lrminus))),nrow=length(lrminus),ncol=length(val)) ##cNPV table
  for(i in 1:length(val)) {
    for(j in 1:length(lrplus)) {
      ab[j,i]<-sigfigures(lrplus[j]*val[i]/(1+lrplus[j]*val[i]))
      ca[j,i]<-sigfigures((val[i]/lrminus[j])/(1+val[i]/lrminus[j]))
    }
  }
  
  data3 <- ab
  colnames(data3)<-prevalence
  T3<-as.table(data3)
  
  data4 <- ca
  colnames(data4)<-prevalence
  T4<-as.table(data4)
  
  ##PPV-cNPV table
  pc<-matrix(c(rep(NA,times=length(val)*length(lrminus))),nrow=length(lrminus),ncol=length(val))
  for(i in 1:length(val)) {
    for(j in 1:length(lrminus)) {
      pc[j,i]<-sigfigures(ab[j,i]-ca[j,i])
    }
  }
  
  data5<-pc
  colnames(data5)<-prevalence
  T5<-as.table(data5)
  
  progba<-matrix(c(rep(NA,times=length(prev)*length(sens))),nrow=length(sens),ncol=length(prev)) ##Program-based table
  pba<-matrix(c(rep(NA,times=length(prev)*length(lrplus))),nrow=length(lrplus),ncol=length(prev)) ##PPV-based table
  for(i in 1:length(prev)) {
    for(j in 1:length(sens)) {  
      progba[j,i]<-sigfigures(1000*prev[i]*sens[j])
      pba[j,i]<-sigfigures(1000*ab[j,i])
    }
  }
  
  data6<-progba
  colnames(data6)<-prevalence
  T6<-as.table(data6)
  
  data7<-pba
  colnames(data7)<-prevalence
  T7<-as.table(data7)
  
  sb<-as.vector(c(rep(NA,times=length(sens)))) ##Sensitivity-Based table
  for(i in 1:length(sens)) {
    sb[i]<-sigfigures(1000*sens[i])
  }
  
  data8<-matrix(rep(sb,times=length(prev)),nrow=length(sens),ncol=length(prev),byrow=F)
  colnames(data8)<-prevalence
  T8<-as.table(data8)
  
  ##Dominated by specificity for rare disease table
  dsa<-matrix(c(rep(NA,times=length(prev)*length(vecspec))),nrow=length(vecspec),ncol=length(prev))
  for(i in 1:length(prev)) {
    for(j in 1:length(vecspec)) {
      dsa[j,i]<-sigfigures(1000*(prev[i]*sens[j]+(1-prev[i])*(1-vecspec[j])))
    }
  }
  
  data9<-dsa
  colnames(data9)<-prevalence
  T9<-as.table(data9)
  
  compdata<-list("Delta"=T1,"Sensitivity Given Specificity"=T2,"PPV"=T3,"cNPV"=T4,"PPV-cNPV"=T5,"Program-Based"=T6,"PPV-Based"=T7,"Sensitivity-Based"=T8,"Dominated by Specificity for a Rare Disease"=T9)
  compdata
}

DrawROC <- function(specmin, specmax, delta) {
  Specificity<-seq(from=specmin,to=specmax,by=0.0001)
  
  Iterations <- 1:length(delta)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity<-1-pnorm(qnorm(Specificity,0,1)-delta[i],0,1,1)
    if(i==1)
      plot(Sensitivity~Specificity,type="l",main=c("ROC curves, Sensitivity vs. Specificity", "Given Different Values of Delta"),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=delta,"bottomright",cex=1.15,text.font=2,bty="o",title=expression(Delta))
}

AUCFromDelta <- function(delta) {
  Specificity <- seq(from=0,to=1,by=0.0000001)
  Sensitivity <- function(Specificity) {
    1 - pnorm(qnorm(Specificity) - delta)
  }
  AUC <- integrate(Sensitivity,0,1)
  sigfigures(AUC$value)
}

sigfigures <- function(number) {
  data <- as.numeric(formatC(signif(number,digits=3), digits=3,format="fg", flag="#"))
}