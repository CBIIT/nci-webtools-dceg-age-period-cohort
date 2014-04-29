DrawSensSpecPPV <- function(specmin,specmax,prevalence, PPV) {
  jpeg('SensSpecPPV.jpg')
  Val <- prevalence/(1-prevalence)
  Specificity <- seq(from=specmin,to=specmax,by=0.00001)
  
  Iterations <- 1:length(PPV)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity <- Val*((1 - PPV[i])/PPV[i])*(1 - Specificity)
    if(i==1) {
      par(mar=c(5.1, 4.1, 4.1, 9.5))
      plot(Sensitivity~Specificity,type="l",main=c("Sensitivity vs. Specificity", "Given Different Values of Positive Predictive Value"),xlab="Specificity",ylab="Sensitivity",ylim=c(0,max(Sensitivity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else
      lines(y=Sensitivity, x=Specificity,lty=LTY[i])
  }
  
  legend(lty=LTY,legend=PPV,"bottomright",cex=1.15,text.font=2,bty="o",inset=c(-0.4,0),xpd = TRUE,pch=c(1,3),title="PPV")
  dev.off()
  
}