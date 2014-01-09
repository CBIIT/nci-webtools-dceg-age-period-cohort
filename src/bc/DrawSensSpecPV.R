DrawSensSpecPV <- function(sensref, specref,prevalence,sensitivity,specificity) {
  jpeg('SensSpecPV.jpg')
  Specificity <- seq(from=0,to=1,by=0.00001)
  LRMinus <- specref/(1-sensref)
  LRPlus <-  sensref/(1-specref)
  PPV <- LRPlus*(prevalence/(1-prevalence))/(1+LRPlus*(prevalence/(1-prevalence)))
  cNPV <- ((prevalence/(1+prevalence))/LRMinus)/(1+(prevalence/(1+prevalence))/LRMinus)
  PV <- as.vector(c(PPV,cNPV))
  
  Iterations <- 1:length(PV)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity1 <- ((1-prevalence)/prevalence)*(PPV/(1-PPV))*(1-Specificity)
    Sensitivity2 <- 1 - Specificity*((1+prevalence)/prevalence)*(cNPV/(1-cNPV))
    if(i==1)
      plot(Sensitivity1~Specificity,type="l",main=c("Sensitivity vs. Specificity","with Positive Predictive Value", "and the Complement of the Negative Predictive Value contours"),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    else {
      lines(y=Sensitivity2, x=Specificity,lty=LTY[i])
      points(sensitivity~specificity)
    }
  }
  
  legend(lty=LTY,legend=c(paste("PPV",round(PPV,digits=2)),paste("cNPV",round(cNPV,digits=2))),"bottomright",cex=1.15,text.font=2,bty="o",title="Predictive Values") 
  dev.off()
}