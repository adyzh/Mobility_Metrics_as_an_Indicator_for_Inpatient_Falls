## Stratified bootstrap for Erik - Reworking by Victor to apply to our analysis
library('boot')
# Get one instance of each patient
data = wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]

# Stratified bootstrap function, i.e. sampling with replacement within strata
# Returns Estimate, Std. Error, 95% CI
strataboot <- function(formula, data, strata, reps=1000){
  reg1 <- lm(formula, data)
  strata = as.numeric(as.factor(strata))
  ests <- matrix(NA, nrow=reps, ncol=length(coef(reg1)))
    for(i in 1:reps){
    index1 <- sample(seq(1:length(strata))[strata==1], sum(strata==1), replace=TRUE)
    index2 <- sample(seq(1:length(strata))[strata==2], sum(strata==2), replace=TRUE)
    bootdat <- rbind(data[index1,],data[index2,])
    ests[i,] <- coef(lm(formula, bootdat))
    }
  val <- cbind(coef(reg1),apply(ests,2,sd),apply(ests,2,quantile,0.025),apply(ests,2,quantile,0.975))
  colnames(val) <- c("Estimate","Std. Error","Lower limit","Upper limit")
  return(val)
}

set.seed(2222023)
strataboot(as.formula(propHighMob~as.factor(FallInjuryYN)),data,strata=data$FallInjuryYN,reps=5000)
t.test(propLowMob~FallInjuryYN,data=data)
summary(lm(propLowMob~FallInjuryYN,data=data))

## Your two groups are fairly large, so you could also do a simple 
## bootstrap
# my.fun = function(d, x, formula) {
#   coef(lm(formula,data=d[x,]))
# }
# 
# boot.out = boot(data,my.fun,R=5000,formula = as.formula(propLowMob~as.factor(FallInjuryYN)))
# boot.ci(boot.out,type="all",index=2)

#### Omnibus ####
# Stratified bootstrap function, i.e. sampling with replacement within strata
# Returns all estimates from each 
strataboot_est <- function(formula, data, strata, reps=1000){
  reg1 <- lm(formula, data)
  strata = as.numeric(as.factor(strata))
  ests <- matrix(NA, nrow=reps, ncol=length(coef(reg1)))
  for(i in 1:reps){
    index1 <- sample(seq(1:length(strata))[strata==1], sum(strata==1), replace=TRUE)
    index2 <- sample(seq(1:length(strata))[strata==2], sum(strata==2), replace=TRUE)
    bootdat <- rbind(data[index1,],data[index2,])
    ests[i,] <- coef(lm(formula, bootdat))
  }
  return(ests)
}
# From estimate results of strataboot
set.seed(2222023)
boot.estLOW <- strataboot(as.formula(propLowMob~as.factor(FallInjuryYN)),data,strata=data$FallInjuryYN,reps=5000)
boot.estMID <- strataboot(as.formula(propMidMob~as.factor(FallInjuryYN)),data,strata=data$FallInjuryYN,reps=5000)
boot.estHIGH <- strataboot(as.formula(propHighMob~as.factor(FallInjuryYN)),data,strata=data$FallInjuryYN,reps=5000)

# 2nd col is difference
M <- matrix(c(boot.est1[,2],boot.est2[,2],boot.est3[,2]), nrow = 5000)

# Vector with Estimates
est1 = coef(lm(propLowMob~FallInjuryYN, data))[2]
est2 = coef(lm(propMidMob~FallInjuryYN, data))[2]
est3 = coef(lm(propHighMob~FallInjuryYN, data))[2]
est <- matrix(c(est1,est2,est3),ncol=1)
# Variance/Covariance of matrix
est.var <- var(M)

# Matrix with each row representing one of the comparisons
C <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)

# Get test statistic following chi-squared distribution
tstat = t(C%*%est)%*%solve(C%*%est.var%*%t(C))%*%(C%*%est)

# Calculate p-value
pchisq(tstat, df=3, lower.tail=FALSE)
