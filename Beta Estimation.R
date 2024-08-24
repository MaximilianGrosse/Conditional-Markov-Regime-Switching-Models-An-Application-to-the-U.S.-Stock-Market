# This script will read the sector as well as the market data
# (including the risk free rate). Then it performs a linear
# regression for all 12 sectors and combines the estimation
# results in seperate .rds files for each state and sample.
# E.g. coefficients120.rds stores the coefficients for the 
# first state of the 20-year sample. To execute the script
# one must first adjust the path of the read_xlsx functions.
# ENTERPATH is simply a placeholder for the correct path.


library(readxl)
library(writexl)

######################
### 20-year sample ###
######################

market <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                    range="B936:B1175", col_names=FALSE)
rj <- read_xlsx("ENTERPATH/SectorData.xlsx", 
                range="B933:M1172", col_names=FALSE)
rf <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="E936:E1175", col_names=FALSE)
T <- nrow(market)
J <- ncol(rj)


rfFrame <- as.data.frame(cbind(rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf))

yj <- sapply(rj, as.numeric) - sapply(rfFrame, as.numeric)

stateVit <- sapply(readRDS("stateVit20.rds"), as.numeric)
plot(stateVit,type="l")

yj <- as.data.frame(cbind(yj, market, stateVit))
colnames(yj) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                  "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

yj1 <- data.frame()
yj2 <- data.frame()

for (t in 1:T){
  if (stateVit[t,1]==1){
    yj1 <- rbind(yj1,yj[t,])
  }
  else{
    yj2 <- rbind(yj2,yj[t,])
  }
}
colnames(yj1) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                   "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

colnames(yj2) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                   "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

coefficients1 <- list()
coefficients2 <- list()

beta1 <- list()
beta2 <- list()

x1 <- as.vector(sapply(yj1[,13], as.numeric))
x2 <- as.vector(sapply(yj2[,13], as.numeric))

for(j in 1:J){
  y <- as.numeric(yj1[,j])
  result <- lm(y~x1)
  summ <- summary(result)
  coefficients1[[j]] <- coef(summ)
  beta1[j] <- coefficients1[[j]][2,1]
  
  y <- as.numeric(yj2[,j])
  result <- lm(y~x2, data=yj2)
  summ <- summary(result)
  coefficients2[[j]] <- coef(summ)
  beta2[j] <- coefficients2[[j]][2,1]
}

saveRDS(object=coefficients1, file="coefficients120.rds")
saveRDS(object=coefficients2, file="coefficients220.rds")
saveRDS(object=yj, file="yj20.rds")
saveRDS(object=yj1, file="yj120.rds")
saveRDS(object=yj2, file="yj220.rds")


######################
### 30-year sample ###
######################

market <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                    range="B816:B1175", col_names=FALSE)
rj <- read_xlsx("ENTERPATH/SectorData.xlsx", 
                range="B813:M1172", col_names=FALSE)
rf <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="E816:E1175", col_names=FALSE)
T <- nrow(market)
J <- ncol(rj)


rfFrame <- as.data.frame(cbind(rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf))

yj <- sapply(rj, as.numeric) - sapply(rfFrame, as.numeric)

stateVit <- sapply(readRDS("stateVit30.rds"), as.numeric)
plot(stateVit,type="l")

yj <- as.data.frame(cbind(yj, market, stateVit))
colnames(yj) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

yj1 <- data.frame()
yj2 <- data.frame()

for (t in 1:T){
  if (stateVit[t,1]==1){
    yj1 <- rbind(yj1,yj[t,])
  }
  else{
    yj2 <- rbind(yj2,yj[t,])
  }
}
colnames(yj1) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                   "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

colnames(yj2) <- c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm",
                   "Utils", "Shops", "Hlth", "Money", "Other", "Market", "State")

coefficients1 <- list()
coefficients2 <- list()

beta1 <- list()
beta2 <- list()

x1 <- as.vector(sapply(yj1[,13], as.numeric))
x2 <- as.vector(sapply(yj2[,13], as.numeric))

for(j in 1:J){
  y <- as.numeric(yj1[,j])
  result <- lm(y~x1)
  summ <- summary(result)
  coefficients1[[j]] <- coef(summ)
  beta1[j] <- coefficients1[[j]][2,1]
  
  y <- as.numeric(yj2[,j])
  result <- lm(y~x2, data=yj2)
  summ <- summary(result)
  coefficients2[[j]] <- coef(summ)
  beta2[j] <- coefficients2[[j]][2,1]
}

saveRDS(object=coefficients1, file="coefficients130.rds")
saveRDS(object=coefficients2, file="coefficients230.rds")
saveRDS(object=yj, file="yj30.rds")
saveRDS(object=yj1, file="yj130.rds")
saveRDS(object=yj2, file="yj230.rds")