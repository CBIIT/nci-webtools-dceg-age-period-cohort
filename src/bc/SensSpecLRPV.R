SensSpecLRPV <- function(sens,spec,prev) {
  
  	vecsens<-as.vector(sens)
  
 	vecspec<-as.vector(spec)
  
  	lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
	lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
	ppv<-as.vector(c(rep(NA,times=length(vecspec))))
	cnpv<-as.vector(c(rep(NA,times=length(vecspec))))
	for(i in 1:length(vecspec)) {
		lrplus[i]<-sens[i]/(1-vecspec[i])
		lrminus[i]<-vecspec[i]/(1-sens[i])
		ppv[i]<-lrplus[i]*prev/(1-prev)/(1 + lrplus[i]*prev/(1-prev))
  		cnpv[i]<-((prev/(1-prev))/lrminus[i])/(1 + (prev/(1-prev))/lrminus[i])
	}
	
	data<-matrix(c(vecspec,vecsens,lrplus,lrminus,ppv,cnpv),nrow=length(vecspec),ncol=6) 
	colnames(data)<-c("Specificity", "Sensitivity","LRplus","LRminus","PPV","cNPV")
	T1<-as.table(data)  	
	T1
}
