# In this script we implement a mean variance optimization by 
# constructing three portfolios: An equally weighted portfolio,
# a global minimum variance portfolio and a tangency portfolio.
# We will do this for each sample period and construct portfolios 
# solely for the stateless environment.

library(readxl)
library(tseries)

######################
### 20-year sample ###
######################

yj <- sapply(readRDS("yj20.rds"), as.numeric)
T <- nrow(yj)
J <- ncol(yj)-2

MuI <- apply(yj[, 1:J],2,mean) 
sdI <- apply(yj[, 1:J],2,mean)
MuEq <- mean(apply(yj[,1:J],1,mean))
sdEq <- sd(apply(yj[,1:J],1,mean))
MuM <- mean(yj[, (J+1)])
sdM <- sd(yj[, (J+1)])

covmat <- cov(yj[, 1:J])
cormat <- cor(yj[,1:J])
covinv <- solve(covmat)
ones <- rep(1, J)

AA <- sum(covinv)
BB <- t(MuI)%*%covinv%*%ones
CC <- t(MuI)%*%covinv%*%MuI
DEL <- AA*CC-BB^2

### Global MVP
wG <- covinv%*%ones/AA
muG <- BB/AA
sigG <- sqrt(1/AA)

mufront <- seq(muG, max(MuI), length=200)
sigfront <- sqrt(CC - 2*BB*as.numeric(mufront) + AA*mufront^2)/sqrt(DEL)

### MiniMuI Variance s.t. Mu = MuM
lams <- solve(matrix(c(AA, BB, BB, CC), nrow=2, byrow=TRUE))%*%c(1, MuM)

wV <- covinv%*%(lams[1]*ones + lams[2]*MuI)
muV <- t(wV)%*%MuI
sigV <- sqrt(t(wV)%*%covmat%*%wV)

### Tangency Portfolio
wT <- covinv%*%MuI
wT <- wT/sum(wT)
muT <- t(wT)%*%MuI
sigT <- sqrt(t(wT)%*%covmat%*%wT)

saveRDS(data.frame(MuI,sdI, MuEq, sdEq, MuM, sdM,muG,wG,sigG, wT, muT, sigT), "NoStateEstimatesMon20.rds")

######################
### 30-year sample ###
######################

yj <- sapply(readRDS("yj30.rds"), as.numeric)
T <- nrow(yj)
J <- ncol(yj)-2

MuI <- apply(yj[, 1:J],2,mean) 
sdI <- apply(yj[, 1:J],2,mean)
MuEq <- mean(apply(yj[,1:J],1,mean))
sdEq <- sd(apply(yj[,1:J],1,mean))
MuM <- mean(yj[, (J+1)])
sdM <- sd(yj[, (J+1)])

covmat <- cov(yj[, 1:J])
cormat <- cor(yj[,1:J])
covinv <- solve(covmat)
ones <- rep(1, J)

AA <- sum(covinv)
BB <- t(MuI)%*%covinv%*%ones
CC <- t(MuI)%*%covinv%*%MuI
DEL <- AA*CC-BB^2

### Global MVP
wG <- covinv%*%ones/AA
muG <- BB/AA
sigG <- sqrt(1/AA)

mufront <- seq(muG, max(MuI), length=200)
sigfront <- sqrt(CC - 2*BB*as.numeric(mufront) + AA*mufront^2)/sqrt(DEL)

### MiniMuI Variance s.t. Mu = MuM
lams <- solve(matrix(c(AA, BB, BB, CC), nrow=2, byrow=TRUE))%*%c(1, MuM)

wV <- covinv%*%(lams[1]*ones + lams[2]*MuI)
muV <- t(wV)%*%MuI
sigV <- sqrt(t(wV)%*%covmat%*%wV)

### Tangency Portfolio
wT <- covinv%*%MuI
wT <- wT/sum(wT)
muT <- t(wT)%*%MuI
sigT <- sqrt(t(wT)%*%covmat%*%wT)

saveRDS(data.frame(MuI,sdI, MuEq, sdEq, MuM, sdM,muG,wG,sigG, wT, muT, sigT), "NoStateEstimatesMon30.rds")