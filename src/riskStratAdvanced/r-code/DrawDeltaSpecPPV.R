DrawDeltaSpecPPV <- function(specmin, specmax, prevalence, PPV) {
  jpeg('DeltaSpecPPV.jpg')
  Specificity <- seq(from=specmin,to=specmax,by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Delta <- qnorm(Specificity) - qnorm(1-(PPV[i]/(1-PPV[i]))/(prevalence/(1-prevalence))*(1-Specificity))
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Delta~Specificity,type="l",main=c("Delta vs. Specificity","Given Different Values of Positive Predictive Value",paste("Prevalence =",prevalence)),xlab="Specificity",ylab="Delta",ylim=c(min(Delta),5),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Delta, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV") 
  dev.off()
}