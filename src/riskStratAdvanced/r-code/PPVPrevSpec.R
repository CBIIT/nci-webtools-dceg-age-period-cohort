PPVPrevSpec <- function(dppv,prev,spec) {

	##Prevalence, PPV, Sensitivity as input and Specificity as output

	p<-as.vector(prev)  ##prevalence values

	vecdppv<-as.vector(dppv)

	vecspec<-as.vector(spec)
	
	podds<-as.vector(c(rep(NA,times=length(p))))
	for(k in 1:length(p)) {
		podds[k]<-p[k]/(1-p[k])
	}

	dppvodds<-as.vector(c(rep(NA,times=length(vecdppv))))
	for(j in 1:length(vecdppv)) {
		dppvodds[j]<-vecdppv[j]/(1-vecdppv[j])
	}

	delta<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(podds))), dim = c(length(vecspec),length(vecdppv),length(podds)), dimnames = list(vecspec,vecdppv,podds))
	for(i in 1:length(vecspec)) {
		for(j in 1:length(vecdppv)) {
			for(k in 1:length(podds)) {
			  delta[i,j,k]<- qnorm(vecspec[i]) - qnorm(1-dppvodds[j]/podds[k]*(1-vecspec[i]))
			}
		}
	}
  
	sens<-array(c(rep(NA,times=length(vecdppv)*length(vecspec)*length(podds))), dim = c(length(vecspec),length(vecdppv),length(podds)), dimnames = list(vecspec,vecdppv,podds))
	for(i in 1:length(vecspec)) {
	  for(j in 1:length(vecdppv)) {
	    for(k in 1:length(podds)) {
	      sens[i,j,k]<- dppvodds[j]/podds[k]*(1-vecspec[i])
	    }
	  }
	}
	
	T1<-as.table(delta)
	T2<-as.table(sens)
	data<-list("Delta_required_to_achieve_specified_PPV_given_prevalence_and_specificity"=T1, "Sensitivity_required_to_achieve_specified_PPV_given_prevalence_and_specificity"=T2, "Prevalence"=prev,"Prevalence_Odds"=podds,"Desired_PPV_as_odds"=dppvodds)
	data
	
}
