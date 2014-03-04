deltaspecppv <- function (cases,controls,specificity,prevalence) {

	##delta table
	a<-as.vector(cases)
	b<-as.vector(controls)
	vecspec<-as.vector(specificity)
	prev<-as.vector(prevalence)

	stdev<-as.vector(c(a[2]*sqrt(a[3]),b[2]*sqrt(b[3])))

	var<-as.vector(c(stdev[1]^2,stdev[2]^2))

	coefvar<-as.vector(c(stdev[1]/a[1],stdev[2]/b[1]))

	n<-a[3]+b[3]
	
	mean<-(a[1]*a[3]+b[1]*b[3])/n
	
	overallvar<-(var[1]*a[3]+var[2]*b[3])/n
	
	overallcoefvar<-sqrt(overallvar)/mean
	
	diff<-a[1]-b[1]

	delta<-abs(diff/sqrt(overallvar))

	data1<-matrix(c(a[1],b[1],mean,a[2],b[2],NA,a[3],b[3],n,stdev[1],stdev[2],NA,var[1],var[2],overallvar,coefvar[1],coefvar[2],overallcoefvar,NA,NA,diff,NA,NA,delta),ncol=3,nrow=8,byrow=T)
	rownames(data1)<-c("Mean","Standard Error","N","Standard Deviation","Variance","Coefficient of Variation (CV)","Difference in means","Delta")
	colnames(data1)<-c("Cases","Controls","Overall")
	T1<-as.table(data1)
	
	##sensitivity given specificity table
	sens<-as.vector(c(rep(NA,times=length(vecspec))))
	for(i in 1:length(vecspec)) {
		sens[i]<-1-pnorm(qnorm(vecspec[i],0,1)-delta,0,1,1)
	}
	
	lrplus<-as.vector(c(rep(NA,times=length(vecspec))))
	lrminus<-as.vector(c(rep(NA,times=length(vecspec))))
	for(i in 1:length(vecspec)) {
		lrplus[i]<-sens[i]/(1-vecspec[i])
		lrminus[i]<-vecspec[i]/(1-sens[i])
	}
	
	data2<-matrix(c(vecspec,sens,lrplus,lrminus),nrow=length(vecspec),ncol=4)  ##Sensitivity Specificity table
	colnames(data2)<-c("Specificity", "Sensitivity","LR+","LR-")
	T2<-as.table(data2)

	val<-as.vector(c(rep(NA,times=length(prev))))
	cval<-as.vector(c(rep(NA,times=length(prev))))
	for(i in 1:length(prev)) {
		val[i]<-prev[i]/(1-prev[i])
		cval[i]<-prev[i]/(1+prev[i])
	}
	
	ab<-matrix(c(rep(NA,times=length(val)*length(lrplus))),nrow=length(lrplus),ncol=length(val))  ##PPV table
	ca<-matrix(c(rep(NA,times=length(cval)*length(lrminus))),nrow=length(lrminus),ncol=length(cval))  ##cNPV table
	for(i in 1:length(val)) {
		for(j in 1:length(lrplus)) {
			ab[j,i]<-lrplus[j]*val[i]/(1+lrplus[j]*val[i])
			ca[j,i]<-(cval[i]/lrminus[j])/(1+cval[i]/lrminus[j])
		}
	}
	
	data3 <- ab
	colnames(data3)<-prevalence
	T3<-as.table(data3)

	data4 <- ca
	colnames(data4)<-prevalence
	T4<-as.table(data4)

	##PPV-cNPV table
	pc<-matrix(c(rep(NA,times=length(cval)*length(lrminus))),nrow=length(lrminus),ncol=length(cval))
	for(i in 1:length(cval)) {
		for(j in 1:length(lrminus)) {
			pc[j,i]<-ab[j,i]-ca[j,i]
		}
	}
	
	data5<-pc
	colnames(data5)<-prevalence
	T5<-as.table(data5)

	progba<-matrix(c(rep(NA,times=length(prev)*length(sens))),nrow=length(sens),ncol=length(prev))  ##Program-based table
	pba<-matrix(c(rep(NA,times=length(prev)*length(lrplus))),nrow=length(lrplus),ncol=length(prev))  ##PPV-based table
	for(i in 1:length(prev)) {
		for(j in 1:length(sens)) {	
			progba[j,i]<-1000*prev[i]*sens[j]
			pba[j,i]<-1000*ab[j,i]
		}
	}

	data6<-progba
	colnames(data6)<-prevalence
	T6<-as.table(data6)
	
	data7<-pba
	colnames(data7)<-prevalence
	T7<-as.table(data7)

	sb<-as.vector(c(rep(NA,times=length(sens)))) ##Sensitivity-Based table
	for(i in 1:length(sens)) {
		sb[i]<-1000*sens[i]
	}

	data8<-matrix(rep(sb,times=length(prev)),nrow=length(sens),ncol=length(prev),byrow=F)
	colnames(data8)<-prevalence
	T8<-as.table(data8)

	##Dominated by specificity for rare disease table
	dsa<-matrix(c(rep(NA,times=length(prev)*length(vecspec))),nrow=length(vecspec),ncol=length(prev))
	for(i in 1:length(prev)) {
		for(j in 1:length(vecspec)) {
			dsa[j,i]<-1000*(prev[i]*sens[j]+(1-prev[i])*(1-vecspec[j]))
		}
	}

	data9<-dsa
	colnames(data9)<-prevalence
	T9<-as.table(data9)

	compdata<-list("Delta"=T1,"Sensitivity Given Specificity"=T2,"PPV"=T3,"cNPV"=T4,"PPV-cNPV"=T5,"Program-Based"=T6,"PPV-Based"=T7,"Sensitivity-Based"=T8,"Dominated by Specificity for a Rare Disease"=T9)
	compdata
}