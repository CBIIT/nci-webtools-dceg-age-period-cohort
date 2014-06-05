library(JPSurv);

# read the seer files in current directory: 
# SEER*STAT files:
#    SEER9_Survival_6CancerSitesByStage_1975_2007.dic 
#      and SEER9_Survival_6CancerSitesByStage_1975_2007.txt
seerdata = joinpoint.seerdata(seerfilename="SEER9_Survival_6CancerSitesByStage_1975_2007", 
                              newvarnames=c("Site","Sex","Stage","Year_of_diagnosis"),NoFit=T,
                              UseVarLabelsInData=c("Site","Sex","Stage","Year_of_diagnosis")
);

# fit the joinpoint model with joinpoin.surv
fit.result = joinpoint(seerdata, subset = Site == "Colon and Rectum" & Sex =="Male",
                       year="Year_of_diagnosis",observedrelsurv="Relative_Survival_Cum",
                       model.form = ~-1+factor(Stage), 
                       maxnum.jp = 1);

# calculate trend measures by the function aapc:  
# Average of AAPC (Annual Absolute Percent Changes)
AVEAAPC=aapc(fit.result, type="AVEAAPC", interval=5);
AVEAAPC

# plot: cohorts (Localized, Regional) 
plot(fit.result,Intervals=c(5),covar.continuous=list(NA,NA),covar.cat=list(c("Localized"),c("Regional")));
