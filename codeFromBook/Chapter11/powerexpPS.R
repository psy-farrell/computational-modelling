library (rjags)
library(MASS)

tlags <- c(0, 1, 5, 10, 20, 50) 
nlags <- length(tlags)

nitems <- 40

nrecalled <- rep(0,nlags)

a <- 0.1
b <- .95
alpha <- .2

# simulate data
for (j in 1:nlags) {
  p <- a + (1-a) * b * exp(-alpha*tlags[j])
  nrecalled[j] <- rbinom(1,nitems,p) 
}

#plot(tlags,nrecalled)

a1.s1<-{}; a1.s2<-{}; a2.s1<-{}; a2.s2<-{}
b1.s1<-{}; b1.s2<-{}; b2.s1<-{}; b2.s2<-{}
alpha.s1<-{}; alpha.s2<-{}; beta.s1<-{}; beta.s2<-{}

###---- Prior parameters
## Model 1 (exponential)
# priors
a1.s1[1] <- 1; a1.s2[1]<- 1
b1.s1[1] <- 1; b1.s2[1]<- 1
alpha.s1[1] <- 1; alpha.s2[1] <- 1

# psuedo-priors--set these to priors for the moment
a1.s1[2] <- 1; a1.s2[2]<- 1
b1.s1[2] <- 1; b1.s2[2]<- 1
alpha.s1[2] <- 1; alpha.s2[2] <- 1

## Model 2 (power)
# priors
a2.s1[2] <- 1; a2.s2[2]<- 1
b2.s1[2] <- 1; b2.s2[2]<- 1
beta.s1[2] <- 1; beta.s2[2] <- 1

# psuedo-priors (temporary)
a2.s1[1] <- 1; a2.s2[1]<- 1
b2.s1[1] <- 1; b2.s2[1]<- 1
beta.s1[1] <- 1; beta.s2[1] <- 1

# ------------Estimate exponential only
expmod <- jags.model("powerexp.j",
                        data = list(t  = tlags,
                                    k = nrecalled,
                                    n  = nitems,
                                    nt = nlags,
                                    a1.s1 = a1.s1, a1.s2 = a1.s2,
                                    a2.s1 = a2.s1, a2.s2 = a2.s2,
                                    b1.s1 = b1.s1, b1.s2 = b1.s2,
                                    b2.s1 = b2.s1, b2.s2 = b2.s2,
                                    alpha.s1 = alpha.s1, alpha.s2=alpha.s2,
                                    beta.s1 = beta.s1, beta.s2=beta.s2,
                                    prior1 = 1),
                        n.chains=4)

# burnin
update(expmod,n.iter=1000)  
# perform MCMC
parameters <- c("a1", "b1", "alpha")
mcmcfin<-coda.samples(expmod,parameters,5000)
mm <- as.matrix(mcmcfin)

# set pseudo-priors to approximate posterior
a1fit <- fitdistr(mm[,"a1"],"beta",start=list(shape1=1,shape2=1))$estimate
b1fit <- fitdistr(mm[,"b1"],"beta",start=list(shape1=1,shape2=1))$estimate
alphafit <- fitdistr(mm[,"alpha"],"beta",start=list(shape1=1,shape2=1))$estimate
a1.s1[2] <- a1fit[1]; a1.s2[2]<- a1fit[2]
b1.s1[2] <- b1fit[1]; b1.s2[2]<- b1fit[2]
alpha.s1[2] <- alphafit[1]; alpha.s2[2] <- alphafit[2]

# ------------Estimate powerl only
expmod <- jags.model("powerexp.j",
                     data = list(t  = tlags,
                                 k = nrecalled,
                                 n  = nitems,
                                 nt = nlags,
                                 a1.s1 = a1.s1, a1.s2 = a1.s2,
                                 a2.s1 = a2.s1, a2.s2 = a2.s2,
                                 b1.s1 = b1.s1, b1.s2 = b1.s2,
                                 b2.s1 = b2.s1, b2.s2 = b2.s2,
                                 alpha.s1 = alpha.s1, alpha.s2=alpha.s2,
                                 beta.s1 = beta.s1, beta.s2=beta.s2,
                                 prior1 = 0),
                     n.chains=4)

# burnin
update(expmod,n.iter=1000)  
# perform MCMC
parameters <- c("a2", "b2", "beta")
mcmcfin<-coda.samples(expmod,parameters,5000)
mm <- as.matrix(mcmcfin)

# set pseudo-priors to approximate posterior
a2fit <- fitdistr(mm[,"a2"],"beta",start=list(shape1=1,shape2=1))$estimate #* \label{line:BF:usePseudos}  *\#
b2fit <- fitdistr(mm[,"b2"],"beta",start=list(shape1=1,shape2=1))$estimate
betafit <- fitdistr(mm[,"beta"],"beta",start=list(shape1=1,shape2=1))$estimate
a2.s1[1] <- a2fit[1]; a2.s2[1]<- a2fit[2]
b2.s1[1] <- b2fit[1]; b2.s2[1]<- b2fit[2]
beta.s1[1] <- betafit[1]; beta.s2[1] <- betafit[2]

# ------------Estimate pM2
prior1 <- 0.2 # this value affects the mixing, should approximate 1/posterior
expmod <- jags.model("powerexp.j",
                     data = list(t  = tlags,
                                 k = nrecalled,
                                 n  = nitems,
                                 nt = nlags,
                                 a1.s1 = a1.s1, a1.s2 = a1.s2,
                                 a2.s1 = a2.s1, a2.s2 = a2.s2,
                                 b1.s1 = b1.s1, b1.s2 = b1.s2,
                                 b2.s1 = b2.s1, b2.s2 = b2.s2,
                                 alpha.s1 = alpha.s1, alpha.s2=alpha.s2,
                                 beta.s1 = beta.s1, beta.s2=beta.s2,
                                 prior1 = prior1),
                     n.chains=4)

# burnin
update(expmod,n.iter=1000)  
# perform MCMC
parameters <- c("alpha","beta","theta","pM2") 
mcmcfin<-coda.samples(expmod,parameters,10000, thin=1)
#summary(mcmcfin)
mm <- as.matrix(mcmcfin)
post2 <- mean(as.matrix(mcmcfin)[,"pM2"]) #* \label{line:PSposteriorcalc}  *\#
print((1-post2)/post2*(1-prior1)/prior1) 

# plot acf
myacf <- {}
for (chain in 1:4){
  myacf <- cbind(myacf, acf(mcmcfin[[chain]][,"pM2"], lag.max=30, plot=F)$acf)
}

matplot(0:10, myacf[1:11,], type="l",
        xlab="Lag",ylab="Autocorrelation",ylim=c(0,1))

# ------------Numerical Estimation of marginal L for comparison
library(cubature)

expL <- function(theta,tlags,y,n){
  a <- theta[1]
  b <- theta[2]
  alpha <- theta[3]
  p <- dbinom(y,n,a+(1-a)*b*exp(-alpha*tlags))
  return(prod(p))
}

powL <- function(theta,tlags,y,n){
  a <- theta[1]
  b <- theta[2]
  beta <- theta[3]
  p <- dbinom(y,n,a+(1-a)*b*((tlags+1)^(-beta)))
  return(prod(p))
}

expML <- adaptIntegrate(expL,c(0,0,0),c(0.2,1,1),
               tlags=tlags,y=nrecalled,n=nitems)
powML <- adaptIntegrate(powL,c(0,0,0),c(0.2,1,1),
                        tlags=tlags,y=nrecalled,n=nitems)
expML$integral/powML$integral
  