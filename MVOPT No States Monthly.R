library(readxl)
library(tseries)

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

Zmu <- c(MuEq, MuM, muG, muV, muT)
Zsig <- c(sdEq, sdM, sigG, sigV, sigT)
Zsharpe <- Zmu/Zsig

# write.csv(round(cbind(Zmu, Zsig, Zsharpe), 2), "NoStateMonthly.csv")

saveRDS(data.frame(MuI,sdI, MuEq, sdEq, MuM, sdM,muG,wG,sigG, wT, muT, sigT), "NoStateEstimatesMon20.rds")


# plot(sigfront,  mufront, ylim=range(c(0, MuI)), xlim=range(c(0,sdI)), 
#      xlab=expression(sigma), ylab=expression(paste(mu, "-rf")), type="l")
# points(sdI, MuI)
# points(c(sdEq, sdM, sigG, sigV, sigT), c(MuEq, MuM, muG, muV, muT), 
#        pch="*", col="blue")
# text(c(sdEq, sdM, sigG, sigV, sigT), c(MuEq, MuM, muG, muV, muT), 
#      labels=c("Eq","M", "G", "MVP", "T"), col="red", 
#      pos=c(1, 1, 2, 2, 2))
# title("State : Industries and Market", line=.5)
# segments(0, 0, sigT, muT)
# 
# muTU <- 1
# 
# for (t in 1:T){
#   muTU <- (1+(as.vector(yj[t,1:12])%*%wT)/100)*muTU
# }
# 
# retTangencyU <- as.matrix(yj[,1:12])%*%wT
# 
# sdTU <- sd(retTangencyU)
# 
# muEW <- 1
# Sum <- 0
# 
# for(t in 1:T){
#   muEW <- (1+mean(yj[t,1:12])/100)*muEW
#   Sum <- Sum+mean(yj[t,1:12])/100
# }
# 
# sdEW <- sd(apply(yj[,1:12],1,mean))
# 
