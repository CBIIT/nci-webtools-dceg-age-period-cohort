DrawROC <- function(Delta) {
	jpeg("rplot.jpg")
	x<-seq(from=0,to=1,by=0.0001)
	y<-1-pnorm(qnorm(x,0,1)-Delta,0,1,1)
	plot(y~x, main=c("ROC Curve, Sensitivity vs. Specificity",paste(expression(Delta),"=",Delta)),xlab="Specificity",ylab="Sensitivity",type='l', lwd=1,xlim=rev(range(x)),font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
	dev.off()
}
