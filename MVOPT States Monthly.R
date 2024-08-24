# This script will read the prepared data.frame for each state for
# the 20-year and 30-year sample from an .rds file. Then it will 
# compute the risk-return profiles for the industry portfolios, 
# the equally weighted portfolio, the global minimum variance 
# portfolio and the tangency portfolio (for state 1 and state 2).

library(readxl)
library(tseries)

######################
### 20-year sample ###
######################

yj1 <- sapply(readRDS("yj120.rds"), as.numeric)
yj2 <- sapply(readRDS("yj220.rds"), as.numeric)
T1 <- nrow(yj1)
T2 <- nrow(yj2)
J <- ncol(yj1)-2

########################
# State 1
########################


MuI1 <- apply(yj1[, 1:J],2,mean) 
sdI1 <- apply(yj1[, 1:J],2,sd)
MuEq1 <- mean(apply(yj1[,1:J],1,mean))
sdEq1 <- sd(apply(yj1[,1:J],1,mean))
MuM1 <- mean(yj1[, (J+1)])
sdM1 <- sd(yj1[, (J+1)])

covmat1 <- cov(yj1[, 1:J])
cormat1 <- cor(yj1[,1:J])
covinv1 <- solve(covmat1)
ones <- rep(1, J)

AA1 <- sum(covinv1)
BB1 <- t(MuI1)%*%covinv1%*%ones
CC1 <- t(MuI1)%*%covinv1%*%MuI1
DEL1 <- AA1*CC1-BB1^2

# Global MVP
wG1 <- covinv1%*%ones/AA1
muG1 <- BB1/AA1
sigG1 <- sqrt(1/AA1)

# Tangency Portfolio
wT1 <- covinv1%*%MuI1
wT1 <- wT1/sum(wT1)
muT1 <- t(wT1)%*%MuI1
sigT1 <- sqrt(t(wT1)%*%covmat1%*%wT1)


########################
# State 2
########################


MuI2 <- apply(yj2[, 1:J],2,mean)
sdI2 <- apply(yj2[, 1:J],2,sd)
MuEq2 <- mean(apply(yj2[,1:J],1,mean))
sdEq2 <- sd(apply(yj2[,1:J],1,mean))
MuM2 <- mean(yj2[, (J+1)])
sdM2 <- sd(yj2[, (J+1)])

covmat2 <- cov(yj2[, 1:J])
cormat2 <- cor(yj2[, 1:J])
covinv2 <- solve(covmat2)
ones <- rep(1, J)

AA2 <- sum(covinv2)
BB2 <- t(MuI2)%*%covinv2%*%ones
CC2 <- t(MuI2)%*%covinv2%*%MuI2
DEL2 <- AA2*CC2-BB2^2

### Global MVP
wG2 <- covinv2%*%ones/AA2
muG2 <- BB2/AA2
sigG2 <- sqrt(1/AA2)

### Tangency Portfolio
wT2 <- covinv2%*%MuI2
wT2 <- wT2/sum(wT2)
muT2 <- t(wT2)%*%MuI2
sigT2 <- sqrt(t(wT2)%*%covmat2%*%wT2)

saveRDS(data.frame(MuI1,sdI1,MuEq1,sdEq1,MuM1,sdM1,muG1,wG1,sigG1,wT1,muT1,sigT1,
                   MuI2,sdI2,MuEq2,sdEq2,MuM2,sdM2,muG2,wG2,sigG2,wT2,muT2,sigT2), "StateEstimatesMon20.rds")

######################
### 30-year sample ###
######################

yj1 <- sapply(readRDS("yj130.rds"), as.numeric)
yj2 <- sapply(readRDS("yj230.rds"), as.numeric)
T1 <- nrow(yj1)
T2 <- nrow(yj2)
J <- ncol(yj1)-2

########################
# State 1
########################


MuI1 <- apply(yj1[, 1:J],2,mean) 
sdI1 <- apply(yj1[, 1:J],2,sd)
MuEq1 <- mean(apply(yj1[,1:J],1,mean))
sdEq1 <- sd(apply(yj1[,1:J],1,mean))
MuM1 <- mean(yj1[, (J+1)])
sdM1 <- sd(yj1[, (J+1)])

covmat1 <- cov(yj1[, 1:J])
cormat1 <- cor(yj1[,1:J])
covinv1 <- solve(covmat1)
ones <- rep(1, J)

AA1 <- sum(covinv1)
BB1 <- t(MuI1)%*%covinv1%*%ones
CC1 <- t(MuI1)%*%covinv1%*%MuI1
DEL1 <- AA1*CC1-BB1^2

# Global MVP
wG1 <- covinv1%*%ones/AA1
muG1 <- BB1/AA1
sigG1 <- sqrt(1/AA1)

# Tangency Portfolio
wT1 <- covinv1%*%MuI1
wT1 <- wT1/sum(wT1)
muT1 <- t(wT1)%*%MuI1
sigT1 <- sqrt(t(wT1)%*%covmat1%*%wT1)


########################
# State 2
########################


MuI2 <- apply(yj2[, 1:J],2,mean)
sdI2 <- apply(yj2[, 1:J],2,sd)
MuEq2 <- mean(apply(yj2[,1:J],1,mean))
sdEq2 <- sd(apply(yj2[,1:J],1,mean))
MuM2 <- mean(yj2[, (J+1)])
sdM2 <- sd(yj2[, (J+1)])

covmat2 <- cov(yj2[, 1:J])
cormat2 <- cor(yj2[, 1:J])
covinv2 <- solve(covmat2)
ones <- rep(1, J)

AA2 <- sum(covinv2)
BB2 <- t(MuI2)%*%covinv2%*%ones
CC2 <- t(MuI2)%*%covinv2%*%MuI2
DEL2 <- AA2*CC2-BB2^2

### Global MVP
wG2 <- covinv2%*%ones/AA2
muG2 <- BB2/AA2
sigG2 <- sqrt(1/AA2)

### Tangency Portfolio
wT2 <- covinv2%*%MuI2
wT2 <- wT2/sum(wT2)
muT2 <- t(wT2)%*%MuI2
sigT2 <- sqrt(t(wT2)%*%covmat2%*%wT2)

saveRDS(data.frame(MuI1,sdI1,MuEq1,sdEq1,MuM1,sdM1,muG1,wG1,sigG1,wT1,muT1,sigT1,
                   MuI2,sdI2,MuEq2,sdEq2,MuM2,sdM2,muG2,wG2,sigG2,wT2,muT2,sigT2), "StateEstimatesMon30.rds")