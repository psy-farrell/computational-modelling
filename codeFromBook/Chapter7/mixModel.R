#mixture model set of functions. this is called from another program, typically the one that plots the data
library(circular)

#pdf for mixture model (Suchow et al., 2013)
logmixturepdf <- function(data,g,sdv) {
  data4vm <- mkcirc(data)
  return(sum(log((1-g)*dvonmises(data4vm,mkcirc(0),sd2k(sdv))  
                 + g*dunif(data,-180,180))))
}

#convert SD into Kappa (Suchow et al., 2013)   
sd2k<-function (d) { #input is in degrees
  S <- (d/180)*pi  #go to radians
  R <- exp(-S^2/2)
  K = 1/(R^3 - 4*R^2 + 3*R)
  if (R < 0.85) {K <- -0.4 + 1.39*R + 0.43/(1-R)}
  if (R < 0.53) {K <- 2*R + R^3 + (5*R^5)/6}
  return(K)
}

#jeffreys prior for precision (Suchow et al., 2013)
jp4kappa <- function(K) {
  z <- exp((log(besselI(K,1,TRUE)) + K) - 
             (log(besselI(K,0,TRUE)) + K))
  return(z * (K - z - K*z^2))
}

#jeffreys prior for a proportion
jp4prop <- function(p) {p^-0.5 * (1-p)^-0.5}

#get overall prior for model parameters
logprior<- function(g,sdv) {
  return(log(jp4kappa(sd2k(sdv)))+log(jp4prop(g)))
}

#make it circular in degrees
mkcirc<-function(td)  
{as.circular(td,control.circular=list(units="degrees"))}
#previous line must be 39
#main function that fits mixture model to data being passed as argument
#next line must be 43
getMixtmodel <-function(data,svalues) {   
  chain <- matrix(0,5000,2)      
  burnin<-500
  set.seed(1234)
  propsd <- svalues*.05  
  lb <- c(0,4)
  ub <- c(1,360)
  
  chain[1,] <- svalues #starting values for parameters
  for (i in c(2:dim(chain)[1])) { 
    cur <- chain[i-1,]
    doitagain <- TRUE
    while (doitagain) {
      propl <- cur + rnorm(2,0,propsd) 
      doitagain <- any(propl<lb) || any(propl>ub)
    }
    
    lpropval <- logmixturepdf(data,propl[1],propl[2])
                   +logprior(propl[1],propl[2])
    lcurval  <- logmixturepdf(data,cur[1],cur[2])
                   +logprior(cur[1],cur[2])
    llratio  <- exp(lpropval-lcurval)   
    if (runif(1) < llratio) { 
      chain[i,] <- propl
    } else {
      chain[i,] <- cur
    }  
  }  
  finparm<-apply(chain[-c(1:burnin),],2,mean) 
  print(finparm)
  
  td<-c(-180:180)
  pred<-(1-finparm[1])*
    dvonmises(mkcirc(td),mkcirc(0),sd2k(finparm[2]))+
    finparm[1]*dunif(td,-180,180)
  posterior<-chain[-c(1:burnin),]
  return(list(preds=pred,posteriors=posterior))
} 



