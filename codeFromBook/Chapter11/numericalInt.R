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
BF=expML$integral/powML$integral
print(BF)