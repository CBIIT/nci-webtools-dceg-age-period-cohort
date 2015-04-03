soccerResultPlot<-function(soccerResultsFile, breaks=seq(-0.005,1.005,0.01),...){
  
  opar<-par(no.readonly=T);
  soccerResults<-read.csv(soccerResultsFile,stringsAsFactor=F)
  s=soccerResults
  tie<-(s$Prob1==s$Prob2);
  h<-hist(s$Prob1[!tie],breaks=seq(-0.01,1.01,0.02),plot=F)
  par(mfcol=c(2,1),omi=rep(0,4),mgp=c(2,1,0),mar=c(3,3,1,2))
  plot(h$mids,cumsum(h$counts)/sum(h$counts),xlim=c(0,1),ylim=c(0,1),xlab="SOCcer score",ylab="cdf",...)
  h<-hist(s$Prob1[tie],breaks=seq(-0.01,1.01,0.02),plot=F)
  plot(h$mids,cumsum(h$counts)/sum(h$counts),xlim=c(0,1),ylim=c(0,1),xlab="SOCcer score tie",ylab="cdf",...)
  
  par(opar)
}

