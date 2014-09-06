cat("
##########################################################################
## These are a set of functions to compute ICC
## and extract Level-1 residual varince and intercept
## variance from an lmer object.
## tau00 : extracts intercept variance
## tau11 : extracts slope variance
## tau01 : extracts covariance between intercept and slopes
## ICC : computes ICC
## Written by Davood Tofighi, 9/6/2014, Georgia Inst of Tech
#########################################################################
\n")
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

if(is.installed("lme4")){

    sigmaSq <- sigma(obj)


tau00 <- function(obj){
    tau<- (VarCorr(obj))[[1]][1,1]
    return(round(tau,3))
}

 tau11 <- function(obj){
    tau<- (VarCorr(obj))[[1]][2,2]
    return(tau)
}

  tau01 <- function(obj){
    tau01<- (VarCorr(obj))[[1]][1,2]
    return(tau01)
}

rho01 <- function(obj){
    s2 <- attr(VarCorr(obj)[[1]],"correlation")[2]
    return(round(s2,3))
}

ICC <- function(obj){
return(tau00(obj)/(tau00(obj)+sigmaSq(obj)))
}
}else {cat("Package lme4 is *not* installed on your compuetr.\nPlease install the package first.\n")}

library(lme4)
(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
summary(fm1)# (with its own print method)
sigma(fm1)
ICC(fm1)
