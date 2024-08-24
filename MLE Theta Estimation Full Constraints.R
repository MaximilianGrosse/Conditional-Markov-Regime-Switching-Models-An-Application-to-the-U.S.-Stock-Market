library(readxl)

# This code will subsequently generate the Theta estimates for
# the 20-year and the 30-year sample. Please adjust the paths
# of the read_xlsx functions so the data can get fed into the
# computation. At the end of the estimation the estimated 
# parameters will be stored in .rds-files to allow the access
# in following scripts. To execute the script one must first 
# adjust the path of the read_xlsx functions. ENTERPATH is 
# simply a placeholder for the correct path.

######################
### 20-year sample ###
######################

# Load data by adjusting path
yM <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="B936:B1175", col_names=FALSE)
T <- nrow(yM)

yM <- sapply(yM, as.numeric)



# Likelihood Function
likelihood <- function(par, data){
  
  # Extract parameters from the vector
  mu1 <-  par[1]
  mu2 <-  par[2]
  sigma1 <-  par[3]
  sigma2 <-  par[4]
  P <-  par[5]
  Q <-  par[6]
  
  # Define density matrix
  Prob <- function(t){
    val <- data[t, 1]
    pdf1 <- dnorm(data[t,1], mu1, sigma1)
    pdf2 <- dnorm(data[t,1], mu2, sigma2)
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

# Producing initial parameters
ui <- cbind(c(1,0,-1,0,0,0,0,0,0,0), c(0,1,0,-1,0,0,0,0,0,0), c(0,0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,1,0,0,0,0), 
            c(0,0,0,0,0,0,1,0,-1,0), c(0,0,0,0,0,0,0,1,0,-1))
ci <- as.matrix(cbind(c(-10,-10,-10,-10,0.01,0.01,0.1,0.1,-0.99,-0.99)))
pars <- c(mean(sapply(yM,as.numeric)),mean(sapply(yM,as.numeric)),
          sd(sapply(yM,as.numeric)),sd(sapply(yM,as.numeric)),0.5,0.5)
fit <- constrOptim(theta=pars, f=likelihood, method="Nelder-Mead",grad=NULL,
                   ui=ui, ci=ci, data=yM)
fit

saveRDS(object=fit$par, file="theta20.rds")

######################
### 30-year sample ###
######################

# Load data by adjusting path
yM <- read_xlsx("ENTERPATH/MarketExcess.xlsx", 
                range="B816:B1175", col_names=FALSE)
T <- nrow(yM)

yM <- sapply(yM, as.numeric)



# Likelihood Function
likelihood <- function(par, data){
  
  # Extract parameters from the vector
  mu1 <-  par[1]
  mu2 <-  par[2]
  sigma1 <-  par[3]
  sigma2 <-  par[4]
  P <-  par[5]
  Q <-  par[6]
  
  # Define density matrix
  Prob <- function(t){
    val <- data[t, 1]
    pdf1 <- dnorm(data[t,1], mu1, sigma1)
    pdf2 <- dnorm(data[t,1], mu2, sigma2)
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

# Producing initial parameters
ui <- cbind(c(1,0,-1,0,0,0,0,0,0,0), c(0,1,0,-1,0,0,0,0,0,0), c(0,0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,1,0,0,0,0), 
            c(0,0,0,0,0,0,1,0,-1,0), c(0,0,0,0,0,0,0,1,0,-1))
ci <- as.matrix(cbind(c(-10,-10,-10,-10,0.01,0.01,0.1,0.1,-0.99,-0.99)))
pars <- c(mean(sapply(yM,as.numeric)),mean(sapply(yM,as.numeric)),
          sd(sapply(yM,as.numeric)),sd(sapply(yM,as.numeric)),0.5,0.5)
fit <- constrOptim(theta=pars, f=likelihood, method="Nelder-Mead",grad=NULL,
                   ui=ui, ci=ci, data=yM)
fit

saveRDS(object=fit$par, file="theta30.rds")