library(readxl)

### load data ###
yM <- read_xlsx("C:/Users/maxig/local/MarketExcess.xlsx", 
                range="B816:B1175", col_names=FALSE)
T <- nrow(yM)
plot(yM)

yM <- sapply(yM, as.numeric)



### likelihood function definition ###
likelihood <- function(par, data){
  
  # Extract parameters from the vector
  mu1 <-  par[1]
  mu2 <-  par[2]
  sigma1 <-  par[3]
  sigma2 <-  par[4]
  P <-  par[5]
  Q <-  par[6]
  
  # define density matrix
  Prob <- function(t){
    val <- data[t, 1]
    pdf1 <- dnorm(data[t,1], mu1, sigma1)
    pdf2 <- dnorm(data[t,1], mu2, sigma2)
    # cat("\n pdf1 ", pdf1, " pdf2: ", pdf2)
    ProbMatrix <- matrix(c(pdf1,0,0, pdf2), byrow = TRUE, nrow=2)
    return(ProbMatrix)
  }
  
  Gamma <- matrix(c(P,1-P,1-Q,Q), byrow=TRUE, nrow=2)
  delta1 <- (1-Q)/(2-P-Q)
  delta <- c(delta1, 1-delta1)
  
  
  # Calculate negative log-likelihood
  lscale <- 0
  phi <- delta
  
  for (t in 1:T){
    phi <- phi %*% Gamma %*% Prob(t)
    lscale <- lscale+log(sum(phi))
    phi <- phi/sum(phi)
  }
  -lscale
}

### Producing initial parameters ###
ui <- cbind(c(1,0,-1,0,0,0,0,0,0,0), c(0,1,0,-1,0,0,0,0,0,0), c(0,0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,1,0,0,0,0), 
            c(0,0,0,0,0,0,1,0,-1,0), c(0,0,0,0,0,0,0,1,0,-1))
ci <- as.matrix(cbind(c(-10,-10,-10,-10,0.01,0.01,0.1,0.1,-0.99,-0.99)))
pars <- c(mean(sapply(yM,as.numeric)),mean(sapply(yM,as.numeric)),
          sd(sapply(yM,as.numeric)),sd(sapply(yM,as.numeric)),0.5,0.5)
fit <- constrOptim(theta=pars, f=likelihood, method="Nelder-Mead",grad=NULL,
                   ui=ui, ci=ci, data=yM)
fit

write.csv(cbind(fit$par, pars), "Est2 94-23.csv")

saveRDS(object=fit$par, file="theta30.rds")

readRDS("theta30.rds")
