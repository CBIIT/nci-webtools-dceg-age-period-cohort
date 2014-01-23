SensSpecLRPV <- function(sens,spec,prev) {
  
  	vecsens<-as.vector(sens)
  
  	vecprev<-as.vector(prev)
  
 	vecspec<-as.vector(spec)
  
  	lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
	lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
	ppv<-as.vector(c(rep(NA,times=length(vecspec))))
	cnpv<-as.vector(c(rep(NA,times=length(vecspec))))
	for(i in 1:length(vecspec)) {
		lrplus[i]<-sens[i]/(1-vecspec[i])
		lrminus[i]<-vecspec[i]/(1-sens[i])
		ppv[i]<-lrplus[i]*vecprev[i]/(1-vecprev[i])/(1 + lrplus[i]*vecprev[i]/(1-vecprev[i]))
  		cnpv[i]<-((vecprev[i]/(1+vecprev[i]))/lrminus[i])/(1 + (vecprev[i]/(1+vecprev[i]))/lrminus[i])
	}
	
	data<-matrix(c(vecspec,vecsens,lrplus,lrminus,vecprev,ppv,cnpv),nrow=length(vecspec),ncol=7) 
	colnames(data)<-c("Specificity", "Sensitivity","LRplus","LRminus","Prevalence","PPV","cNPV")
	T1<-as.table(data)  	
	T1
}
