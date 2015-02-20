CompRecVarkSensSpec <- function(sens,spec,prev,N) {
  
  list <- 1:length(sens)
  vals <-  1:length(sens)
  dppvdsens <-  1:length(sens)
  dppvdspec <-  1:length(sens)
  varsens <-  1:length(sens)
  varspec <-  1:length(sens)
  varppv <-  1:length(sens)
  varppvk <-  1:length(sens)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    
    if((sens[i]+spec-1) == 0) {
      vals[i] <- 0.5
      dppvdsens[i] <- prev*(1-spec)*(1-prev)/(sens[i]*prev+(1-spec)*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens[i]/(sens[i]*prev+(1-spec)*(1-prev))^2
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dppvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
    
    else {    
      vals[i] <- (1-sens[i])*(1-spec)*(-1+sqrt(1+(sens[i]+spec-1)/((1-sens[i])*(1-spec))))/(sens[i]+spec-1)
      vals[i] <- round(vals[i],5)
      dppvdsens[i] <- prev*(1-spec)*(1-prev)/(sens[i]*prev+(1-spec)*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens[i]/(sens[i]*prev+(1-spec)*(1-prev))^2
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dppvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
  }
  data<-matrix(c(sens,vals,list),ncol=3,nrow=length(sens),byrow=F)
  colnames(data)<-c("Sensitivity","Optimal k","Relative efficiency gain or loss compared to k = 0.5")
  T <-as.table(data)
  T
  
}

CompRecVarkSpecSens <- function(spec,sens,prev,N) {
  
  list <- 1:length(spec)
  vals <-  1:length(spec)
  dppvdsens <-  1:length(spec)
  dppvdspec <-  1:length(spec)
  varsens <-  1:length(spec)
  varspec <-  1:length(spec)
  varppv <-  1:length(spec)
  varppvk <-  1:length(spec)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    
    if((sens+spec[i]-1) == 0) {
      vals[i] <- 0.5
      dppvdsens[i] <- prev*(1-spec[i])*(1-prev)/(sens*prev+(1-spec[i])*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens/(sens*prev+(1-spec[i])*(1-prev))^2
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dppvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
    
    else {    
      vals[i] <- (1-sens)*(1-spec[i])*(-1+sqrt(1+(sens+spec[i]-1)/((1-sens)*(1-spec[i]))))/(sens+spec[i]-1)
      vals[i] <- round(vals[i],5)
      dppvdsens[i] <- prev*(1-spec[i])*(1-prev)/(sens*prev+(1-spec[i])*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens/(sens*prev+(1-spec[i])*(1-prev))^2
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dppvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
  }
  data<-matrix(c(spec,vals,list),ncol=3,nrow=length(spec),byrow=F)
  colnames(data)<-c("Specificity","Optimal k","Relative efficiency gain or loss compared to k = 0.5")
  T <-as.table(data)
  T
  
}

CompRecVarcNPVkSpecSens <- function(spec,sens,prev,N) {
  
  list <- 1:length(spec)
  vals <-  1:length(spec)
  dcnpvdsens <-  1:length(spec)
  dcnpvdspec <-  1:length(spec)
  varsens <-  1:length(spec)
  varspec <-  1:length(spec)
  varcnpv <-  1:length(spec)
  varcnpvk <-  1:length(spec)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    
    if((sens+spec[i]-1) == 0) {
      vals[i] <- 0.5
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec[i])/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens)/spec[i]^2)/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dcnpvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    else {    
      vals[i] <- sens*spec[i]*(1-sqrt(1-(sens+spec[i]-1)/(sens*spec[i])))/(sens+spec[i]-1)
      vals[i] <- round(vals[i],5)
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec[i])/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens)/spec[i]^2)/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dcnpvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
  }
  data<-matrix(c(spec,vals,list),ncol=3,nrow=length(spec),byrow=F)
  colnames(data)<-c("Specificity","Optimal k","Relative efficiency gain or loss compared to k = 0.5")
  T <-as.table(data)
  T
  
}

CompRecVarcNPVkSensSpec <- function(sens,spec,prev,N) {
  
  list <- 1:length(sens)
  vals <-  1:length(sens)
  dcnpvdsens <-  1:length(sens)
  dcnpvdspec <-  1:length(sens)
  varsens <-  1:length(sens)
  varspec <-  1:length(sens)
  varcnpv <-  1:length(sens)
  varcnpvk <-  1:length(sens)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    
    if((sens[i]+spec-1) == 0) {
      vals[i] <- 0.5
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens[i])/spec^2)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dcnpvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    else {
      vals[i] <- sens[i]*spec*(1-sqrt(1-(sens[i]+spec-1)/(sens[i]*spec)))/(sens[i]+spec-1)
      vals[i] <- round(vals[i],5)
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens[i])/spec^2)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dcnpvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
  }
  data<-matrix(c(sens,vals,list),ncol=3,nrow=length(sens),byrow=F)
  colnames(data)<-c("Sensitivity","Optimal k","Relative efficiency gain or loss compared to k = 0.5")
  T <-as.table(data)
  T
  
}

DrawCompRecVarkSpecSens <- function(k,spec,sens,prev,N) {
  
  K <- seq(from=min(k),to=max(k),by=0.00001)
  Y <- seq(from=-1,to=3,by=0.00001)
  list <- 1:length(spec)
  vals <-  1:length(spec)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    
    dppvdsens <- prev*(1-spec[i])*(1-prev)/(sens*prev+(1-spec[i])*(1-prev))^2
    dppvdspec <- prev*(1-prev)*sens/(sens*prev+(1-spec[i])*(1-prev))^2
    varsens <- sens*(1-sens)/(K*N)
    varspec <- spec[i]*(1-spec[i])/((1-K)*N)
    varppv <- dppvdsens^2*varsens+dppvdspec^2*varspec
    varppvk <- dppvdsens^2*(sens*(1-sens)/(0.5*N))+dppvdspec^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
    compare <- ((1/varppv)/(1/varppvk))^2-1
    
    if((sens+spec[i]-1) == 0) {
      vals[i] <- 0.5
      dppvdsens[i] <- prev*(1-spec[i])*(1-prev)/(sens*prev+(1-spec[i])*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens/(sens*prev+(1-spec[i])*(1-prev))^2
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dppvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
    
    else {    
      vals[i] <- (1-sens)*(1-spec[i])*(-1+sqrt(1+(sens+spec[i]-1)/((1-sens)*(1-spec[i]))))/(sens+spec[i]-1)
      vals[i] <- round(vals[i],5)
      dppvdsens[i] <- prev*(1-spec[i])*(1-prev)/(sens*prev+(1-spec[i])*(1-prev))^2
      dppvdspec[i] <- prev*(1-prev)*sens/(sens*prev+(1-spec[i])*(1-prev))^2
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varppv[i] <- dppvdsens[i]^2*varsens[i]+dppvdspec[i]^2*varspec[i]
      varppvk[i] <- dppvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dppvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varppv[i])/(1/varppvk[i]))^2-1
    }
    
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of PPV by", "proportion of total assays in cases",paste("Sensitivity =",sens,", Prevalence =",prev)),xlab="k: the proportion of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-0.5,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
    points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,lty=LTY,legend=spec,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.3,0),xpd = TRUE,title="Specificity")
}

DrawCompRecVarcNPVkSpecSens <- function(k,spec,sens,prev,N) {
  
  K <- seq(from=min(k),to=max(k),by=0.00001)
  Y <- seq(from=-1,to=3,by=0.00001)
  list <- 1:length(spec)
  vals <-  1:length(spec)
  
  Iterations <- 1:length(spec)
  LTY <- Iterations
  
  for(i in Iterations){
    
    dcnpvdsens <-(prev/(1-prev))*(-1/spec[i])/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
    dcnpvdspec <-(prev/(1-prev))*(-(1-sens)/spec[i]^2)/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
    varsens <- sens*(1-sens)/(K*N)
    varspec <- spec[i]*(1-spec[i])/((1-K)*N)
    varcnpv <- dcnpvdsens^2*varsens+dcnpvdspec^2*varspec
    varcnpvk <- dcnpvdsens^2*(sens*(1-sens)/(0.5*N))+dcnpvdspec^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
    compare <- ((1/varcnpv)/(1/varcnpvk))^2-1
    
    if((sens+spec[i]-1) == 0) {
      vals[i] <- 0.5
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec[i])/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens)/spec[i]^2)/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dcnpvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    else {    
      vals[i] <- sens*spec[i]*(1-sqrt(1-(sens+spec[i]-1)/(sens*spec[i])))/(sens+spec[i]-1)
      vals[i] <- round(vals[i],5)
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec[i])/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens)/spec[i]^2)/(1+(prev/(1-prev))*(1-sens)/spec[i])^2 
      varsens[i] <- sens*(1-sens)/(vals[i]*N)
      varspec[i] <- spec[i]*(1-spec[i])/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens*(1-sens)/(0.5*N))+dcnpvdspec[i]^2*(spec[i]*(1-spec[i])/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of cNPV by", "proportion of total assays in cases",paste("Sensitivity =",sens,", Prevalence =",prev)),xlab="k: the proportion of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-0.5,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
    points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,lty=LTY,legend=spec,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.3,0),xpd = TRUE,title="Specificity")
}

DrawCompRecVarcNPVkSensSpec <- function(k,sens,spec,prev,N) {
  
  K <- seq(from=min(k),to=max(k),by=0.00001)
  Y <- seq(from=-1,to=3,by=0.00001)
  list <- 1:length(sens)
  vals <-  1:length(sens)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    
    dcnpvdsens <-(prev/(1-prev))*(-1/spec)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
    dcnpvdspec <-(prev/(1-prev))*(-(1-sens[i])/spec^2)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
    varsens <- sens[i]*(1-sens[i])/(K*N)
    varspec <- spec*(1-spec)/((1-K)*N)
    varcnpv <- dcnpvdsens^2*varsens+dcnpvdspec^2*varspec
    varcnpvk <- dcnpvdsens^2*(sens[i]*(1-sens[i])/(0.5*N))+dcnpvdspec^2*(spec*(1-spec)/((1-0.5)*N))
    compare <- ((1/varcnpv)/(1/varcnpvk))^2-1
    
    if((sens[i]+spec-1) == 0) {
      vals[i] <- 0.5
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens[i])/spec^2)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dcnpvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    else {
      vals[i] <- sens[i]*spec*(1-sqrt(1-(sens[i]+spec-1)/(sens[i]*spec)))/(sens[i]+spec-1)
      vals[i] <- round(vals[i],5)
      dcnpvdsens[i] <-(prev/(1-prev))*(-1/spec)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      dcnpvdspec[i] <-(prev/(1-prev))*(-(1-sens[i])/spec^2)/(1+(prev/(1-prev))*(1-sens[i])/spec)^2 
      varsens[i] <- sens[i]*(1-sens[i])/(vals[i]*N)
      varspec[i] <- spec*(1-spec)/((1-vals[i])*N)
      varcnpv[i] <- dcnpvdsens[i]^2*varsens[i]+dcnpvdspec[i]^2*varspec[i]
      varcnpvk[i] <- dcnpvdsens[i]^2*(sens[i]*(1-sens[i])/(0.5*N))+dcnpvdspec[i]^2*(spec*(1-spec)/((1-0.5)*N))
      list[i] <- ((1/varcnpv[i])/(1/varcnpvk[i]))^2-1
    }
    
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of cNPV by", "proportion of total assays in cases",paste("Specificity =",spec,", Prevalence =",prev)),xlab="k: the proportion of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-0.5,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
    points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,lty=LTY,legend=sens,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.3,0),xpd = TRUE,title="Sensitivity")
}