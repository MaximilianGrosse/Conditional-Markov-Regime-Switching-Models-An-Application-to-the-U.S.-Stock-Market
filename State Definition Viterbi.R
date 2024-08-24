# The purpose of this script is to estimate the optimal state 
# sequence using the Viterbi algorithm. As input we use our 
# estimates as .rds files and perform the algorithm in the 
# following order: initialization, induction and backtracking. 
# We conduct these steps for the 20-year and the 30-year sample.
# To execute the script one must first adjust the path of the 
# read_xlsx functions. ENTERPATH is just a placeholder for the 
# local path.

library(readxl)

######################
### 20-year sample ###
######################

#Load parameters Theta
yM <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="B936:B1175", col_names=FALSE)
T <- nrow(yM);T
model <- readRDS("theta20.rds")
mu1 <- model[1]
mu2 <- model[2]
sigma1 <- model[3]
sigma2 <- model[4]
P <- model[5]
Q <- model[6]
delta1 <- (1-Q)/(2-P-Q)

p1 <- function(val){
  prob <- dnorm(as.numeric(val), mu1, sigma1)
}
p2 <- function(val){
  prob <- dnorm(as.numeric(val), mu2, sigma2)
}

# Initialization
psi <- data.frame()
psi[1, 1] <- 0
psi[1, 2] <- 0
xi <- data.frame()
xi[1, 1] <- log(delta1*p1(yM[1, 1]))
xi[1, 2] <- log((1-delta1)*p2(yM[1,1]))

# Induction
for(t in 2:T){
  psi[t, 1] <- which.max(c(as.numeric(xi[t-1, 1])+log(P), 
                           as.numeric(xi[t-1, 2]+log(1-Q))))
  psi[t, 2] <- which.max(c(as.numeric(xi[t-1, 1])+log(1-P), 
                           as.numeric(xi[t-1, 2]+log(Q))))
  xi[t, 1] <- log(p1(yM[t, 1])) + max(c(as.numeric(xi[t-1, 1])+log(P), 
                                    as.numeric(xi[t-1, 2])+log(1-Q)))
  xi[t, 2] <- log(p2(yM[t, 1])) + max(c(as.numeric(xi[t-1, 1])+log(1-P), 
                                    as.numeric(xi[t-1, 2])+log(Q)))
}

stateVit <- data.frame()
stateVit[T, 1] <- which.max(c(as.numeric(xi[T, 1]), as.numeric(xi[T, 2])))

# Backtracking

for(t in (T-1):1){
  print(t)
  stateVit[t, 1] <- as.numeric(psi[t+1, as.numeric(stateVit[t+1, 1])])
}

saveRDS(object=stateVit, file="stateVit20.rds")

pdf("ViterbiStates20.pdf")
plot(stateVit[, 1], type="l", col="black", lwd=2,
     xlab="Time", ylab="State", ylim=c(1, 2), yaxt="n")
axis(2, at=c(1, 2), labels=c("1", "2"), las=1)

######################
### 30-year sample ###
######################

#Load parameters Theta
yM <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="B816:B1175", col_names=FALSE)
T <- nrow(yM);T
model <- readRDS("theta30.rds")
mu1 <- model[1]
mu2 <- model[2]
sigma1 <- model[3]
sigma2 <- model[4]
P <- model[5]
Q <- model[6]
delta1 <- (1-Q)/(2-P-Q)

p1 <- function(val){
  prob <- dnorm(as.numeric(val), mu1, sigma1)
}
p2 <- function(val){
  prob <- dnorm(as.numeric(val), mu2, sigma2)
}

# Initialization
psi <- data.frame()
psi[1, 1] <- 0
psi[1, 2] <- 0
xi <- data.frame()
xi[1, 1] <- log(delta1*p1(yM[1, 1]))
xi[1, 2] <- log((1-delta1)*p2(yM[1,1]))

# Induction
for(t in 2:T){
  psi[t, 1] <- which.max(c(as.numeric(xi[t-1, 1])+log(P), 
                           as.numeric(xi[t-1, 2]+log(1-Q))))
  psi[t, 2] <- which.max(c(as.numeric(xi[t-1, 1])+log(1-P), 
                           as.numeric(xi[t-1, 2]+log(Q))))
  xi[t, 1] <- log(p1(yM[t, 1])) + max(c(as.numeric(xi[t-1, 1])+log(P), 
                                        as.numeric(xi[t-1, 2])+log(1-Q)))
  xi[t, 2] <- log(p2(yM[t, 1])) + max(c(as.numeric(xi[t-1, 1])+log(1-P), 
                                        as.numeric(xi[t-1, 2])+log(Q)))
}

stateVit <- data.frame()
stateVit[T, 1] <- which.max(c(as.numeric(xi[T, 1]), as.numeric(xi[T, 2])))

# Backtracking

for(t in (T-1):1){
  print(t)
  stateVit[t, 1] <- as.numeric(psi[t+1, as.numeric(stateVit[t+1, 1])])
}

saveRDS(object=stateVit, file="stateVit30.rds")

pdf("ViterbiStates30.pdf")
plot(stateVit[, 1], type="l", col="black", lwd=2,
     xlab="Time", ylab="State", ylim=c(1, 2), yaxt="n")
axis(2, at=c(1, 2), labels=c("1", "2"), las=1)