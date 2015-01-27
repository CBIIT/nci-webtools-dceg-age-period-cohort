DrawCompRecVarkSensSpec <- function(k,sens,spec,prev,N) {
  
  K <- seq(from=min(k),to=max(k),by=0.00001)
  Y <- seq(from=-1,to=3,by=0.00001)
  list <- 1:length(sens)
  vals <-  1:length(sens)
  
  Iterations <- 1:length(sens)
  LTY <- Iterations
  
  for(i in Iterations){
    dppvdsens <- prev*(1-spec)*(1-prev)/(sens[i]*prev+(1-spec)*(1-prev))^2
    dppvdspec <- prev*(1-prev)*sens[i]/(sens[i]*prev+(1-spec)*(1-prev))^2
    varsens <- sens[i]*(1-sens[i])/(K*N)
    varspec <- spec*(1-spec)/((1-K)*N)
    varppv <- dppvdsens^2*varsens+dppvdspec^2*varspec
    varppvk <- dppvdsens^2*(sens[i]*(1-sens[i])/(0.5*N))+dppvdspec^2*(spec*(1-spec)/((1-0.5)*N))
    compare <- ((1/varppv)/(1/varppvk))^2-1
    
    for(n in 1:length(compare)) {
      if(compare[n] == max(compare)) {
        vals[i] <- K[n]
        list[i] <- compare[n]
      }
    }
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of PPV by", "percent of total assays in cases",paste("Specificity =",spec,", Prevalence =",prev,", N =",N)),xlab="k: the percent of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-1,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], lwd=3,col = LTY[i])
      points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,legend=vals,"topright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Optimal k values")
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,title="Sensitivity")
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
    
    for(n in 1:length(compare)) {
      if(compare[n] == max(compare)) {
        vals[i] <- K[n]
        list[i] <- compare[n]
      }
    }
    
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of PPV by", "percent of total assays in cases",paste("Sensitivity =",sens,", Prevalence =",prev,", N =",N)),xlab="k: the percent of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-1,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
      points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,legend=vals,"topright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Optimal k values")
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Specificity")
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
    
    for(n in 1:length(compare)) {
      if(compare[n] == max(compare)) {
        vals[i] <- K[n]
        list[i] <- compare[n]
      }
    }
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of cNPV by", "percent of total assays in cases",paste("Sensitivity =",sens,", Prevalence =",prev,", N =",N)),xlab="k: the percent of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-1,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
      points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,legend=vals,"topright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Optimal k values")
  legend(lty=LTY,legend=spec,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,title="Specificity")
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
    
    for(n in 1:length(compare)) {
      if(compare[n] == max(compare)) {
        vals[i] <- K[n]
        list[i] <- compare[n]
      }
    }
    
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(compare~K,type="l",main=c("Efficiency for tests of cNPV by", "percent of total assays in cases",paste("Specificity =",spec,", Prevalence =",prev,", N =",N)),xlab="k: the percent of assays in cases",ylab=c("Relative efficiency gain or loss compared", "to equal numbers of cases and controls"),ylim=c(-1,3),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
      lines(y=K*0,x=K,lty=1,col=1)
      lines(y=Y,x=0.5*(Y*0+1),lty=2,col=1)
    }
    else
      lines(y=compare, x=K,lty=LTY[i], col = LTY[i],lwd=3)
      points(y=list[i],x=vals[i],pch=LTY[i],col=LTY[i],cex=1.5,lwd=2)
  }
  legend(pch=LTY,legend=vals,"topright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Optimal k values")
  legend(lty=LTY,legend=sens,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,title="Sensitivity")
}