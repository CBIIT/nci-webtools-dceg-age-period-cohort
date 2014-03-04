TestCCData <- function(cases,controls) {
  a<-as.vector(cases)
  b<-as.vector(controls)
  n<-a[3]+b[3]
  stdev<-as.vector(c(a[2]*sqrt(a[3]),b[2]*sqrt(b[3])))
  var<-as.vector(c(stdev[1]^2,stdev[2]^2))
  overallvar<-(var[1]*a[3]+var[2]*b[3])/n
  diff<-a[1]-b[1]
  delta<-abs(diff/sqrt(overallvar))
  if(9 > delta) {TRUE}
  else FALSE
}
