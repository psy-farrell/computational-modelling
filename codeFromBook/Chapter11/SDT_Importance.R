library(MASS)

# Calculate Bayes Factors for SDT model and 1HT model

h <- 60         
f <- 11

sigtrials <- noistrials <- 100   

gmix <- 0.2
N <- 20000

##------ SDT

# specify function to enter in to importance sampling mixture
# here we use the prior distributions
d <- rnorm(N, mean=1, sd=1) #* \label{line:importp1}  *\#
B <- rnorm(N, mean=0, sd=1)
df <- function(x) dnorm(x,1,1)
dB <- function(x) dnorm(x,0,1) #* \label{line:importp4}  *\#

# obtain samples from posterior
source("SDT.R")
mcmcs <- as.matrix(mcmcfin)
d_mu <- mean(mcmcs[,"d"]) #* \label{line:isfitting1}  *\#
d_sd <- sd(mcmcs[,"d"])
B_mu <- mean(mcmcs[,"b"])
B_sd <- sd(mcmcs[,"b"]) #* \label{line:isfitting2}  *\#

d_pos <- rnorm(N, mean=d_mu,sd=d_sd) #* \label{line:importSamplePos}  *\#
B_pos <- rnorm(N, mean=B_mu,sd=B_sd)

mask <- runif(N)>gmix
d[mask] <- d_pos[mask]
B[mask] <- B_pos[mask]

pp <- dnorm(d,1,1)*
  dnorm(B,0,1)/
  ((1-gmix)*dnorm(d,d_mu,d_sd)*dnorm(B,B_mu,B_sd) + gmix*df(d)*dB(B))
L <- dbinom(h,sigtrials,pnorm(d/2-B))* 
  dbinom(f,noistrials,pnorm(-d/2-B))*pp
ml_SDT <- mean(L)

# ---------1HT (beta)

# specify function to enter in to importance sampling mixture with the posterior
# here we use the prior distributions
th1 <- rbeta(N, 1, 1)
th2 <- rbeta(N, 1, 1)
d1 <- function(x) dbeta(x,1,1)
d2 <- function(x) dbeta(x,1,1)

## obtain samples from posterior
source("1HT.R")
mcmcs <- as.matrix(mcmcfin)

#obtain beta parameter estimates using MLE
kk <- fitdistr(mcmcs[,"th1"], "beta", list(shape1=5,shape2=5))
th1_s1 <- kk$estimate[1]
th1_s2 <- kk$estimate[2]
kk <- fitdistr(mcmcs[,"th2"], "beta", list(shape1=5,shape2=5))
th2_s1 <- kk$estimate[1]
th2_s2 <- kk$estimate[2]
  
th1_pos <- rbeta(N, th1_s1,th1_s2)
th2_pos <- rbeta(N, th2_s1,th2_s2)

mask <- runif(N)>gmix
th1[mask] <- th1_pos[mask]
th2[mask] <- th2_pos[mask]

pp <- dbeta(th1,1,1)*
  dbeta(th2,1,1)/
  ((1-gmix)*dbeta(th1,th1_s1,th1_s2)*dbeta(th2,th2_s1,th2_s2) + gmix*d1(th1)*d2(th2))

L <- dbinom(h,sigtrials,th1+(1-th1)*th2) * 
  dbinom(f,noistrials,th2)*pp
ml_HT <- mean(L)

#---What is the Bayes Factor?
ml_SDT/ml_HT
