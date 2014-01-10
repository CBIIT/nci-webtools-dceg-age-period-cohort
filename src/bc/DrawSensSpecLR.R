DrawSensSpecLR <- function(sensref, specref,sensitivity,specificity) {
  jpeg('SensSpecLR.jpg')
  Specificity <- seq(from=0,to=1,by=0.00001)
  LRMinus <- specref/(1-sensref)
  LRPlus <-  sensref/(1-specref)
  LR <- as.vector(c(LRMinus,LRPlus))
  
  Iterations <- 1:length(LR)
  LTY <- Iterations
  
  for(i in Iterations){
    Sensitivity1 <- 1 - Specificity/LRMinus
    Sensitivity2 <- LRPlus*(1 - Specificity)
    if(i==1) {
      plot(Sensitivity1~Specificity,type="l",main=c("Sensitivity vs. Specificity","with Likelihood Ratio contours"),xlab="Specificity",ylab="Sensitivity",ylim=c(0,1),xlim=rev(range(Specificity)),lty=LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=Sensitivity2, x=Specificity,lty=LTY[i])
      points(sensitivity~specificity)
    }
  }
  
  legend(lty=LTY,legend=c(paste("LR-",round(LRMinus,digits=1)),paste("LR+",round(LRPlus,digits=1))),"bottomright",cex=1.15,text.font=2,bty="o",title="Likelihood Ratio (LR)") 
  dev.off()
}