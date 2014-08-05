SensSpecLR <- function(sens,spec) {
  
  vecsens<-as.vector(sens)
  
  vecspec<-as.vector(spec)
  
  lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
  lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
  for(i in 1:length(vecspec)) {
    lrplus[i]<-sens[i]/(1-vecspec[i])
    lrminus[i]<-vecspec[i]/(1-sens[i])
  }
  
  data<-matrix(c(vecspec,vecsens,lrplus,lrminus),nrow=length(vecspec),ncol=4) 
  colnames(data)<-c("Specificity", "Sensitivity","LRplus","LRminus")
  T1<-as.table(data)  	
  T1
}
