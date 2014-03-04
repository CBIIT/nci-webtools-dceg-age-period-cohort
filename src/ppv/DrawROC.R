DrawROC <- function(uniquekey, specmin, specmax, delta) {
  file<- paste("./tmp/",uniquekey,"rplot.png")
  graphfile<-gsub("\\s","", file)
  png(graphfile)
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
  dev.off()
}
