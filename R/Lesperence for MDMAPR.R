# Function for low quant LOD/LOQ Calculation Method
# Packages used
packages = c("dplyr","ggplot2","knitr","kableExtra","RColorBrewer")

## Load or install&load
package.check <- lapply(packages,
                        FUN = function(x) {
                          if (!require(x, character.only = TRUE)) {
                            install.packages(x, dependencies = TRUE)
                            library(x, character.only = TRUE)
                          }
                        }
)

## Source the functions
source("eLowQuant-Functions-V20210407.R")

MDMAPfile <- read.csv("/Users/abiyogasekaram/Documents/LOD calc/TestStandardCurveData/Brook Trout/01_12_BT_LOD_calc.csv")

# subset the data required for calculation
DAT <- MDMAPfile[, c("standardCurveName", "runRecordedBy", "systemCalculatedCqValue", "standardConc")]

DAT <- MDMAPfile
# rename the columns to match the calculation
colnames(DAT) <- c("Target", "Lab", "Cq", "SQ")

## Ensure data is in the proper format:
DAT$Target <- as.factor(DAT$Target)
DAT$Lab <- as.factor(DAT$Lab)  #ML
DAT$Cq <- suppressWarnings(as.numeric(as.character(DAT$Cq))) #Non-numerical values (i.e. negative wells) will be converted to NAs
DAT$SQ <- suppressWarnings(as.numeric(as.character(DAT$SQ))) #Non-numerical values (i.e. NTC) will be converted to NAs
# by MDMAPR Definitions, if a Cq value is 40, there is no amplification and should be converted to NA
DAT$Cq[which(DAT$Cq==40)] <- NA

# setting all negative controls to 0
DAT$SQ[is.na(DAT$SQ)] <- 0
DAT.df <- data.frame(DAT)


# compute poisson estimates
DAT.Tar.SQ <-  DAT.df %>%
  group_by(Target, SQ) %>%
  dplyr::summarise(detect=sum(!is.na(Cq)), n=n(),  Cqmean=mean(Cq, na.rm=TRUE), 
            Lab=Lab[1])
DAT.Tar.SQ <- droplevels(data.frame(DAT.Tar.SQ))
uLabs <- unique(DAT.Tar.SQ$Lab) #unique labs 

DAT.Tar.SQ <- arrange(DAT.Tar.SQ, Lab, Target, SQ) #sort data by SQ in Target in Lab

## Add variables to data set:  L10.SQ, phat, ... 
DAT.Tar.SQ <- within(DAT.Tar.SQ, {
  L10.SQ <- log10(SQ)  
  phat <- detect/n           #sample proportion detect
  vphat <- phat*(1-phat)/n   #var of phat
  lamhat <- -log(1-phat) 
  vlamhat <- phat/n/(1-phat)  #var of lamhat using the delta method
  sdlamhat <- sqrt(vlamhat)   #sd of lamhat using the delta method
  MElamhat <- 1.96*sdlamhat  #margin of error for lambda hat using delta method
}
)


## All Targets and Labs **DO NOT DUPLICATE Target names over Labs!!
uLabs <- unique(DAT.Tar.SQ$Lab)
uTargets <- unique(DAT.Tar.SQ$Target)
nTargets <- length(uTargets)
uLabsTargets <- unique(DAT.Tar.SQ[,c('Lab','Target')])
uLabsTargets$Lab <- as.character(uLabsTargets$Lab)

#ensure ulabsTargets in same order as uTargets
uLabsTargets <- uLabsTargets[match(uLabsTargets$Target, uTargets),]  
uLabsTargets.names <- apply(uLabsTargets, 1, paste, collapse=', ')

DAT.Tar.SQ <- within(DAT.Tar.SQ, {
  CIexphat.lower <-  1 - qbeta(.975, n-detect+1, detect)  #exact phat bounds
  CIexphat.upper <-  qbeta(.975, detect+1, n-detect)
  
  ## Use transformed exact phat bounds
  Lamhatex.Lower <- -log(1 - CIexphat.lower)
  Lamhatex.Upper <- -log(1 - CIexphat.upper)
}
)

error <- list()
if ((sum(DAT.Tar.SQ$phat<1))<3){
  error[["detect.rate"]] <- paste0("Not enough standard dilutions have less than 100% detection rate. The low quantification method is not appropriate for your standard curve data.")
}


nndetect <- vector("list", nTargets) 
nrowTarget <- rep(0, length=nTargets)

for(i in 1:nTargets) {
  Target.dat <- subset(DAT.Tar.SQ, Target==uTargets[i])
  bSQ <- !is.na(Target.dat$phat)  
  lastSQ <- as.logical(cumprod(Target.dat$phat!=1 & bSQ)) 
  ## removes first observations with SQ with phat=1 and larger SQs
  Target.dat <- Target.dat[lastSQ,]
  nndetect[i] <- list(Target.dat )
  nrowTarget[i] <- nrow(Target.dat)
  
  if(nrow(nndetect[[i]]) < 2) {next}
  
  maxSQ <- max(Target.dat$SQ)
  maxlamhat <- max(Target.dat$lamhat)
  
  ######################### show this plot on the standard curve tab
  plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, ylab='Lambda hat',
       xlab='Starting copy number',
       ylim=c(0, maxlamhat), xlim=c(0, maxSQ), main=uLabsTargets.names[i])
  ## Transformed Exact CI
  arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ, 
         Target.dat$Lamhatex.Upper,
         length=0.05, angle=90, code=3)
  ## overlay simple regression line and R-squared
  jlm <- lm(lamhat ~ SQ, data=Target.dat)
  abline(jlm, col=2)
  legend("topleft", paste('lm Rsq=',round(summary(jlm)$r.squared, 2)), bty="n")
  if(round(summary(jlm)$r.squared, 2)<0.9 || is.na(round(summary(jlm)$r.squared, 2))){
    rsquared <- round(summary(jlm)$r.squared, 2)
    error[["rsquared"]] <- paste0("The linear model R squared value for your data set is ", rsquared, ". This linear model is not appropriate for your standard curve")
    break
  }
}

##### No intercept poisson model calculations
Calib.fit.estimates0 <- vector("list", nTargets)  #list of fit estimates
names(Calib.fit.estimates0) <- uTargets
Calib.fit.all0 <- vector("list", nTargets)  #list of fits
Calib.fit.LLRp0 <- vector("numeric", nTargets)
names(Calib.fit.LLRp0) <- uTargets
Calib.fit.res0 <- matrix(0, nrow=nTargets, ncol=4)
rownames(Calib.fit.res0) <- uTargets
colnames(Calib.fit.res0) <- c("convergence", "LLR", "degf","Pval")
Calib.fit.res0 <- data.frame(Calib.fit.res0)


for(i in 1:nTargets){
  if(nrow(nndetect[[i]]) < 3) {Calib.fit.estimates0[[i]] <- NULL
  next}
  Target.dat <- nndetect[[i]]
  
  # Could use following for starting value
  # j.glm <- glm(cbind(n-detect, detect)~SQ-1, data=Target.dat, family=binomial(link='log'))
  
  
  Calib.fit <- optim(par=c(1), fn=CalibOr.LLik,  nd=Target.dat$detect,
                     S=Target.dat$SQ, nn=Target.dat$n,
                     method="BFGS", control=list(fnscale=-1), gr=CalibOr.dLLik,
                     hessian=TRUE)
  
  Calib.fit.all0[[i]] <- Calib.fit
  
  ## Variance estimates
  if(nrow(Target.dat)==2) {
    Calib.fit.Var <- matrix(0, 2, 2)  #singular hessian for 2 observations
  }  else { 
    Calib.fit.Var <- solve(-Calib.fit$hessian)
  }
  cmat <- cbind(Calib.fit$par, sqrt(diag(Calib.fit.Var)))
  cmat <- cbind(cmat, cmat[,1]/cmat[,2])
  cmat <- cbind(cmat, 2*pnorm(-cmat[, 3]))
  colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
  if (nrow(cmat)==1) {rownames(cmat) <- c("beta")
  } else  rownames(cmat) <- c("alpha","beta")
  Calib.fit.estimates0[[i]] <- cmat
  
  ## Likelihood ratio statistic and pvalue for goodnes-of-fit of the model
  Calib.degf <- nrow(Target.dat) - length(Calib.fit$par)
  #SQ=0 do not contribute to the likelihood for no intercept model
  bool <- Target.dat$SQ !=0 
  Calib.LLR <- 2*(Bin.LLik(Target.dat$detect[bool], Target.dat$n[bool]) 
                  - Calib.fit$value)  
  Calib.LLR.pv <- pchisq(Calib.LLR, Calib.degf, lower.tail = FALSE)
  Calib.fit.LLRp0[i] <- Calib.LLR.pv
  Calib.fit.res0[i,] <- c(Calib.fit$convergence, Calib.LLR, Calib.degf, Calib.LLR.pv)
  
  
  ## Compute fitted values for ML model
  Calib.fitted <- Calib.fit$par *  Target.dat$SQ
  
  # Plot calibration curve on lambda scale
  maxSQ <- max(Target.dat$SQ)
  maxlamhat <- max(Target.dat$lamhat)
  
  plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, 
       ylab='Lambda hat', xlab='Starting copy number',
       ylim=c(0, maxlamhat), xlim=c(0, maxSQ), las=1, 
       main=uTargets[i])
  abline(0, Calib.fit$par, col=4)
  legend("topleft", legend=c('ML fit'), lty=1, col=4, bty="n")
  arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ, 
         Target.dat$Lamhatex.Upper,
         length=0.05, angle=90, code=3)
  
  
  ## Plot calibration curve on phat scale
  ## Compute minSQ such that phat~=1 (1-phat = .99)
  ##  sqs <- seq(0, maxSQ)
  maxSQa <- max( -( log(.01))/Calib.fit$par, maxSQ)
  sqs <- seq(0, maxSQa, by=.1)
  Calib.phat <- 1 - exp(-( Calib.fit$par * sqs))
  
  plot(sqs, Calib.phat, xlog=TRUE, 
       ylab='Proportion detect', xlab='Starting copy number', type='l', col=4, las=1, 
       ylim=c(0, 1), xlim=c(0, maxSQa),  main=uTargets[i])
  points(Target.dat$SQ, Target.dat$phat)
  legend("topleft", legend=c('ML fit'), lty=1, col=4, bty="n")
}

# Estimate Poisson models - intercept model

# List of results
Calib.fit.estimates <- vector("list", nTargets)  #list of fit estimates
names(Calib.fit.estimates) <- uTargets
Calib.fit.all <- vector("list", nTargets)  #list of fits all
Calib.fit.LLRp <- vector("numeric", nTargets)
names(Calib.fit.LLRp) <- uTargets
Calib.fit.res <- matrix(0, nrow=nTargets, ncol=4)
rownames(Calib.fit.res) <- uTargets
colnames(Calib.fit.res) <- c("convergence", "LLR", "degf","Pval")
Calib.fit.res <- data.frame(Calib.fit.res)

for(i in 1:nTargets){
  if(nrow(nndetect[[i]]) < 3) {Calib.fit.estimates[[i]] <- NULL
  cat(paste('Too few values for ', uTargets[i]), '\n')
  next}
  Target.dat <- nndetect[[i]]
  jlm <- lm(lamhat ~ SQ, data=Target.dat) #starting values for alpha and beta
  
  Calib.fit <- optim(par=pmax(c(0.01, 0.01), coef(jlm)), fn=Calib.LLik,  
                     nd=Target.dat$detect,
                     S=Target.dat$SQ, nn=Target.dat$n, gr=Calib.dLLik,
                     method="BFGS", control=list(fnscale=-1),  hessian=TRUE)
  
  Calib.fit.all[[i]] <- Calib.fit
  if(nrow(Target.dat)==2) {
    Calib.fit.Var <- matrix(0, 2, 2)  #singular hessian for 2 observations
  }  else { 
    Calib.fit.Var <- solve(-Calib.fit$hessian)
  }
  cmat <- cbind(Calib.fit$par, sqrt(diag(Calib.fit.Var)))
  cmat <- cbind(cmat, cmat[,1]/cmat[,2])
  cmat <- cbind(cmat, 2*pnorm(-cmat[, 3]))
  colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
  if (nrow(cmat)==1) {rownames(cmat) <- c("beta")
  } else  rownames(cmat) <- c("alpha","beta")
  Calib.fit.estimates[[i]] <- cmat
  
  #Likelihood ratio statistic and pvalue for goodnes-of-fit of the model
  Calib.degf <- nrow(Target.dat) - length(Calib.fit$par)
  Calib.LLR <- 2*(Bin.LLik(Target.dat$detect, Target.dat$n) 
                  - Calib.fit$value)  
  Calib.LLR.pv <- pchisq(Calib.LLR, Calib.degf, lower.tail = FALSE)
  Calib.fit.LLRp[i] <- Calib.LLR.pv
  Calib.fit.res[i,] <- c(Calib.fit$convergence, Calib.LLR, Calib.degf, Calib.LLR.pv)
  
  #Compute fitted values for ML model
  Calib.fitted <- Calib.fit$par[1] + Calib.fit$par[2]*  Target.dat$SQ  
  
  # Plot calibration curve on lambda scale
  maxSQ <- max(Target.dat$SQ)
  maxlamhat <- max(Target.dat$lamhat)
  
  plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, 
       ylab='Lambda hat', xlab='Starting copy number',
       ylim=c(0, maxlamhat), xlim=c(0, maxSQ),  las=1,
       main=uTargets[i])
  abline(Calib.fit$par[1], Calib.fit$par[2], col=4)
  legend("topleft", legend=c('ML fit'), lty=1, col=4, bty="n")
  arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ, 
         Target.dat$Lamhatex.Upper,
         length=0.05, angle=90, code=3)
  
  # Plot calibration curve on phat scale
  # Compute minSQ such that phat~=1
  maxSQa <-max( -(Calib.fit$par[1] + log(.01))/Calib.fit$par[2], maxSQ)
  sqs <- seq(0, maxSQa, by=.1)
  Calib.phat <- 1 - exp(-(Calib.fit$par[1] + Calib.fit$par[2] * sqs))
  
  plot(sqs, Calib.phat, xlog=TRUE, 
       ylab='Proportion detect', xlab='Starting copy number', type='l', col=4, las=1, 
       ylim=c(0, 1), xlim=c(0, maxSQa),  main=uTargets[i])
  points(Target.dat$SQ, Target.dat$phat)
  legend("topleft", legend=c('ML fit'), lty=1, col=4, bty="n")
}   


# Determine Lc, Ld, Lq (LOB, LOD, LOQ) - no intercept model
# No intercept model computations - Here Lc==0
# Lc computation:  P(Y > Lc | S=0) <= alphaLc 
#  where Y ~ Bin(m, p=1 - exp(-betas[1]))
#  to incorporate estimation uncertaintly in betas[1], use upper limit of conf int
#  i.e. Y ~ Bin(m, p=1 - exp(-(betas[1] + 1.96 * s.e.(betas[1])))
# Ld computation:  P(Y <= Lc | p_xd) <= betaLd
#  use relationship between Binomial and Beta distribution, p 278 Bain
# Lq computation:  (Forootan 2017)  choose level, Sj, such that CV<=gammaLq
#  not clear which scale: response, back-transformed values, ... ?
#
##* Note in R:  The quantile is defined as the smallest value x such 
##    that F(x) ≥ p, where F is the distribution function.

## Set values for alphaLC, betaLd, gammaLq
alphaLc <- betaLd <- .05; gammaLq <- .20
NN <- c(3, 8, 16, 24, 32, 48, 64, 96)  #test number of replicates

#set up tables for manuscript output
Lc.all0 <- matrix(0, nrow=nTargets, ncol=length(NN))
row.names(Lc.all0) <- uTargets
colnames(Lc.all0) <- paste(NN)
xdd.all0 <- xd.all0 <- xd_upper.all0 <- xd_lower.all0 <- xq.all0 <- xq_upper.all0 <- 
  xq_lower.all0 <- Lc.all0 


for(i in 1:nTargets){
  #use beta estimated from fits above
  if(nrow(nndetect[[i]]) < 3) {
    cat(paste('Too few values for ', uTargets[i]), '\n') 
    next}
  
  betas <- (Calib.fit.estimates0[[i]])[,1]
  betas <- (Calib.fit.estimates0[[1]])[,1]
  #ML Takes into account uncertainty in new observation and s.e. of betas
  betas.upper <- betas + 1.96 * (Calib.fit.estimates0[[i]])[,2]
  betas.lower <- betas - 1.96 * (Calib.fit.estimates0[[i]])[,2]
  
  #Want: P(Y > Lc | S=0) <= alphaLc where Y ~ Bin(m, p=1 - exp(-betas[1]))
  #Lc at xc=0 values for new observation
  
  # For no intercept model, P(Y = 0 | S=0)=1 and P(i'th tech rep detect| S=0)=0
  #  Lc==0, and the P(Y > Lc | S=0) <= alphaLc
  #  since P(Y > 0 | S=0)==0.  
  # We are saying that sample is negative if Y=0 and positive if Y>0
  
  Lc <- rep(0, length(NN))
  
  #Want xd, P(Y <= Lc | p_xd) <= betaLd
  #Ld and xd calculation
  pxd <- 1 - qbeta(betaLd, NN-Lc, Lc+1)   #proportion detected
  Ld <- NN * pxd
  xd <- ( - log(1 - pxd)) / betas   #concentration
  xdlower <- ( - log(1 - pxd)) / betas.upper
  xdupper <- ( - log(1 - pxd)) / betas.lower
  names(pxd) <- names(xd) <- names(xdlower) <- names(xdupper) <- paste(NN)
  
  #Compute confidence interval for xd
  xd.all0[i,] <- xd
  xd_upper.all0[i,] <- xdupper
  xd_lower.all0[i,] <- xdlower
  
  #Compute model based xq
  xq <- -(log(1 - 1/(1 + gammaLq^2*NN)))/betas
  names(xq) <- paste(NN)
  
  #Compute model based xq using lower estimates of beta - upper bound for xq
  xq_lower <- -(log(1 - 1/(1 + gammaLq^2*NN)))/betas.lower
  names(xq_lower) <- paste(NN)
  
  #Compute model based xq using upper estimates of beta
  xq_upper <- -(log(1 - 1/(1 + gammaLq^2*NN)))/betas.upper
  names(xq_upper) <- paste(NN)
  
  xq.all0[i,] <- xq
  xq_lower.all0[i,] <- xq_lower
  xq_upper.all0[i,] <- xq_upper
  
}



#For a given p_hat, one can compute the sample size m required to attain a
#  CV  <= gammaLq, as   m >= (1-phat)/(phat*gammaLq)

# Determine Lc, Ld, Lq (LOB, LOD and LOQ) - intercept model

alphaLc <- betaLd <- .05; gammaLq <- .20
NN <- c(3, 8, 16, 24, 32, 48, 64, 96)  #test number of replicates

#set up tables for manuscript output
Lc.all <- matrix(0, nrow=nTargets, ncol=length(NN))
row.names(Lc.all) <-  uTargets
colnames(Lc.all) <-  paste(NN)
Lc.upper.all <- xd.all <- xd_upper.all <- xd_lower.all <- 
  xq.all <- xq_lower.all <- xq_upper.all <- Lc.all 

for(i in 1:nTargets){
  #use beta estimated from fits above
  if(nrow(nndetect[[i]]) < 3) {
    cat(paste('Too few values for ', uTargets[i]), '\n') 
    next}
  
  betas <- (Calib.fit.estimates[[i]])[,1]
  
  #ML Takes into account uncertainty in new observation and s.e. of betas
  betas.upper <- betas + 1.96 * (Calib.fit.estimates[[i]])[,2]
  betas.lower <- pmax(0, betas - 1.96 * (Calib.fit.estimates[[i]])[,2])
  
  #Want: P(Y > Lc | S=0) <= alphaLc where Y ~ Bin(m, p=1 - exp(-betas[1]))
  #Lc at xc=0 values for new observation
  p.new <- 1 - exp(-betas[1])
  Lc.new <- qbinom(1 - alphaLc, size=NN, prob=p.new)
  names(Lc.new) <- paste(NN) 
  Lc.all[i,] <- Lc.new
  
  #Lc.upper at xc=0 values for new observation, incl s.e. of betas
  p.upper <- 1 - exp(-betas.upper[1])
  Lc.upper <- qbinom(1 - alphaLc, size=NN, prob=p.upper)
  names(Lc.upper) <- paste(NN) 
  Lc.upper.all[i,] <- Lc.upper
  
  #Want xd, P(Y <= Lc | p_xd) <= betaLd
  #Ld and xd calculation
  pxd <- 1 - qbeta(betaLd, NN-Lc.new, Lc.new+1)   #proportion detected
  Ld <- NN * pxd
  xd <- (-betas[1] - log(1 - pxd)) / betas[2]   #concentration
  names(pxd) <- names(xd) <- paste(NN)
  
  # pxd_upper <- 1 - qbeta(betaLd, NN-Lc.upper, Lc.upper+1)   #proportion detected\
  # Ld_upper <- NN * pxd_upper
  # xd_upper <- (-betas[1] - log(1 - pxd_upper)) / betas[2]   #concentration
  xd_lower <- pmax(0, (-betas.upper[1] - log(1 - pxd)) / betas.upper[2])
  xd_upper <- (-betas.lower[1] - log(1 - pxd)) / betas.lower[2]
  names(xd_upper) <- names(xd_lower) <- paste(NN)
  
  xd.all[i,] <- xd
  xd_upper.all[i,] <- xd_upper
  xd_lower.all[i,] <- xd_lower
  
  #Compute model based xq
  xq <- -(betas[1] + log(1 - 1/(1 + gammaLq^2*NN)))/betas[2]
  names(xq) <- paste(NN)
  
  #Compute model based xq using lower estimates of beta
  xq_lower <- -(betas.lower[1] + log(1 - 1/(1 + gammaLq^2*NN)))/betas.lower[2]
  names(xq_lower) <- paste(NN)
  
  #Compute model based xq using upper estimates of beta
  xq_upper <- -(betas.upper[1] + log(1 - 1/(1 + gammaLq^2*NN)))/betas.upper[2]
  names(xq_upper) <- paste(NN)
  
  
  xq.all[i,] <- xq
  xq_lower.all[i,] <- xq_lower
  xq_upper.all[i,] <- xq_upper
  
  
}



#For a given p_hat, one can compute the sample size m required to attain a
#  CV  <= gammaLq, as   m >= (1-phat)/(phat*gammaLq)


# Estimates, Lc, Ld, Lq (LOB, LOD, LOQ) and confidence limits for a given number of technical reps NN[NNi]

#Chooses the model (intercept versus no intercept) with the best LLR test fit,
#i.e. the largest p-value for the LLR test.  A table of values for all
#assays is printed.  


# Set the index into NN which is defined in chunk LcLdLqNN0 and LcLdLqNN  
# NNi <- 2 corresponds to the 2nd entry of NN
NNi <- 6

#cat('Limits intercept model for N=', NN[NNi])
xdxq.all <- cbind(Lc=Lc.all[,NNi], 
                  # LcUp=Lc.upper.all[,NNi],
                  SdLow=xd_lower.all[,NNi],  
                  Sd=xd.all[,NNi], SdUp=xd_upper.all[,NNi],
                  SqLow=xq_upper.all[,NNi],
                  Sq=xq.all[,NNi], SqUp=xq_lower.all[,NNi])


#cat('Limits for no intercept model for N=', NN[NNi])
xdxq.all0 <- cbind(Lc=0, SdLow=xd_lower.all0[,NNi],  
                   Sd=xd.all0[,NNi], SdUp=xd_upper.all0[,NNi],
                   SqLow=xq_upper.all0[,NNi], 
                   Sq=xq.all0[,NNi], SqUp=xq_lower.all0[,NNi])

#Include alpha and beta estimates in table
alphabeta.se <- alphabeta0.se  <- matrix(0, nrow=nTargets, ncol=4)
colnames(alphabeta.se) <- c("alpha","aSE", "beta", "bSE")
colnames(alphabeta0.se) <- c("alpha","aSE", "beta", "bSE")
rownames(alphabeta0.se) <- uTargets
for (i in 1:nTargets){
  if(nrowTarget[i]>2){
    alphabeta.se[i,1:2] <- Calib.fit.estimates[[i]][1, 1:2]
    alphabeta.se[i, 3:4] <- Calib.fit.estimates[[i]][2, 1:2]
    alphabeta0.se[i, 3:4] <- Calib.fit.estimates0[[i]][1, 1:2]
  }
}



Calib.choice <- Calib.fit.LLRp > Calib.fit.LLRp0
xdxq.choice <- cbind(InterModel=Calib.choice, alphabeta0.se, xdxq.all0)
xdxq.choice[Calib.choice, 6:12] <- xdxq.all[Calib.choice,]
xdxq.choice[Calib.choice, 2:5] <- alphabeta.se[Calib.choice,]

# NNi <- 2 This is set above
cat('Limits for best choice model for N=', NN[NNi], '\n')
print(round(xdxq.choice[nrowTarget > 2,], digits=2))

