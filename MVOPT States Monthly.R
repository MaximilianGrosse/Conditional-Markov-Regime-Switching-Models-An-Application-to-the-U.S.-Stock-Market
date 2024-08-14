library(readxl)
library(tseries)

yj1 <- sapply(readRDS("yj120.rds"), as.numeric)
yj2 <- sapply(readRDS("yj220.rds"), as.numeric)
T1 <- nrow(yj1)
T2 <- nrow(yj2)
J <- ncol(yj1)-2

################################################################################
# State 1
################################################################################


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

### Global MVP
wG1 <- covinv1%*%ones/AA1
muG1 <- BB1/AA1
sigG1 <- sqrt(1/AA1)

mufront1 <- seq(muG1, max(MuI1), length=200)
sigfront1 <- sqrt(CC1 - 2*BB1*as.numeric(mufront1) + AA1*mufront1^2)/sqrt(DEL1)

### MiniMuI Variance s.t. Mu = MuM
lams1 <- solve(matrix(c(AA1, BB1, BB1, CC1), nrow=2, byrow=TRUE))%*%c(1, MuM1)

wV1 <- covinv1%*%(lams1[1]*ones + lams1[2]*MuI1)
muV1 <- t(wV1)%*%MuI1
sigV1 <- sqrt(t(wV1)%*%covmat1%*%wV1)

### Tangency Portfolio
wT1 <- covinv1%*%MuI1
wT1 <- wT1/sum(wT1)
muT1 <- t(wT1)%*%MuI1
sigT1 <- sqrt(t(wT1)%*%covmat1%*%wT1)



# Zmu1 <- c(MuEq1, MuM1, muG1, muV1, muT1)
# Zsig1 <- c(sdEq1, sdM1, sigG1, sigV1, sigT1)
# Zsharpe1 <- Zmu1/Zsig1

# write.csv(round(cbind(Zmu1, Zsig1, Zsharpe1), 2), "table1.csv")
# 
# plot(sigfront1,  mufront1, ylim=range(c(0, MuI1)), xlim=range(c(0,sdI1)), 
#      xlab=expression(sigma), ylab=expression(paste(mu, "-rf")), type="l")
# points(sdI1, MuI1)
# points(c(sdEq1, sdM1, sigG1, sigV1, sigT1), c(MuEq1, MuM1, muG1, muV1, muT1), 
#        pch="*", col="blue")
# text(c(sdEq1, sdM1, sigG1, sigV1, sigT1), c(MuEq1, MuM1, muG1, muV1, muT1), 
#      labels=c("Eq1","M1", "G1", "MVP1", "T1"), col="red", 
#      pos=c(1, 1, 2, 2, 2))
# title("State 1: Industries and Market", line=.5)
# segments(0, 0, sigT1, muT1)


###############################################################################
# State 2
###############################################################################


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

mufront2 <- seq(muG2, max(MuI2), length=200)
sigfront2 <- sqrt(CC2 - 2*BB2*as.numeric(mufront2) + AA2*mufront2^2)/sqrt(DEL2)

### MiniMuI Variance s.t. Mu = MuM
lams2 <- solve(matrix(c(AA2, BB2, BB2, CC2), nrow=2, byrow=TRUE))%*%c(1, MuM2)

wV2 <- covinv2%*%(lams2[1]*ones + lams2[2]*MuI2)
muV2 <- t(wV2)%*%MuI2
sigV2 <- sqrt(t(wV2)%*%covmat2%*%wV2)

### Tangency Portfolio
wT2 <- covinv2%*%MuI2
wT2 <- wT2/sum(wT2)
muT2 <- t(wT2)%*%MuI2
sigT2 <- sqrt(t(wT2)%*%covmat2%*%wT2)

Zmu2 <- c(MuEq2, MuM2, muG2, muV2, muT2)
Zsig2 <- c(sdEq2, sdM2, sigG2, sigV2, sigT2)
Zsharpe2 <- Zmu2/Zsig2

# write.csv(round(cbind(Zmu2, Zsig2, Zsharpe2), 2), "table2.csv")

saveRDS(data.frame(MuI1,sdI1,MuEq1,sdEq1,MuM1,sdM1,muG1,wG1,sigG1,wT1,muT1,sigT1,
                   MuI2,sdI2,MuEq2,sdEq2,MuM2,sdM2,muG2,wG2,sigG2,wT2,muT2,sigT2), "StateEstimatesMon20.rds")

# plot(sigfront2,  mufront2, ylim=range(c(0, MuI2)), xlim=range(c(0,sdI2)), 
#      xlab=expression(sigma), ylab=expression(paste(mu, "-rf")), type="l")
# points(sdI2, MuI2)
# points(c(sdEq2, sdM2, sigG2, sigV2, sigT2), c(MuEq2, MuM2, muG2, muV2, muT2), 
#        pch="*", col="blue")
# text(c(sdEq2, sdM2, sigG2, sigV2, sigT2), c(MuEq2, MuM2, muG2, muV2, muT2), 
#      labels=c("Eq2","M2", "G2", "MVP2", "T2"), col="red", 
#      pos=c(1, 1, 2, 2, 2))
# title("State 2: Industries and Market", line=.5)
# segments(0, 0, sigT2, muT2)
# 
# 
# figure5 <- efficient.frontiere(yj1, max.mup = 0.03, plot = TRUE)
# 
# figure5 <- figure5 + labs(subtitle = "Stock Porfolio (Monthly Data)", caption = "Figure 5") 
#                     + theme(axis.text.x = element_text(angle = 0, face = "bold", size = 6), 
#                             axis.text.y = element_text(face = "bold",
#                             panel.grid.major.y = element_line(colour = "grey60", 
#                             linetype = "dotted"), legend.position = "top"))
# 
# sapply()
# 
# 
