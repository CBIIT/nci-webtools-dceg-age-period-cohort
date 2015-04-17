# option 1 for calculation
data <- read.csv("p16-ELISA-sample-data.csv")
spec<-c(0.8, 0.9, 0.95, 0.99, 0.995)
prev<-c(0.5, 0.6, 0.7, 0.8, 0.9)
source("RiskFromROC.R")
risk1<-RiskFromROC(data, spec, prev)
DrawRawROC(data)

# option 2 for calculation
cases<-c(4, 0.1, 100)
controls<-c(1, 0.1, 200)
risk2<-deltaspecppv(cases, controls, spec, prev)
specmin<-min(spec)
specmax<-max(spec)
DrawROC(specmin, specmax, risk2$Delta[8,3])