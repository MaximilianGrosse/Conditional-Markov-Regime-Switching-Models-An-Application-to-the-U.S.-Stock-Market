setwd("C:/Users/maxig/OneDrive/Desktop/Documents/Uni/Studium KIT/SS 2024/Bachelorarbeit/Thimme/Data and Code/GARCH code")

library(readxl)
library(writexl)
market <- read_xlsx("C:/Users/maxig/local/MarketExcess.xlsx", 
                    range="B816:B1175", col_names=FALSE)
rj <- read_xlsx("C:/Users/maxig/local/SectorData.xlsx", 
                range="B813:M1172", col_names=FALSE)
rf <- read_xlsx("C:/Users/maxig/local/MarketExcess.xlsx", 
                range="E816:E1175", col_names=FALSE)
T <- nrow(market)
J <- ncol(rj)

rfFrame <- as.data.frame(cbind(rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf,rf))

rj <- sapply(rj, as.numeric)
#rf <- sapply(rfFrame, as.numeric)

yj <- rj
free <- sapply(rf, as.numeric)
rfree <- mean(free)

market <- sapply(market, as.numeric) + sapply(rf, as.numeric);market

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

x1 <- yj1[,13]
x2 <- yj2[,13]

for(j in 1:J){
  y <- as.numeric(yj1[,j])
  result <- lm(y~x1, data=yj1)
  summ <- summary(result)
  coefficients1[[j]] <- coef(summ)
  beta1[j] <- coefficients1[[j]][2,1]
  
  y <- as.numeric(yj2[,j])
  result <- lm(y~x2, data=yj2)
  summ <- summary(result)
  coefficients2[[j]] <- coef(summ)
  beta2[j] <- coefficients2[[j]][2,1]
}

plot(1:J,beta1,col="black",ylim=c(0.4,1.6))
points(1:J,beta2,col="red")

#saveRDS(object=coefficients1, file="coefficients130.rds")
#saveRDS(object=coefficients2, file="coefficients230.rds")
saveRDS(object=yj, file="rj30.rds")
saveRDS(object=yj1, file="rj130.rds")
saveRDS(object=yj2, file="rj230.rds")