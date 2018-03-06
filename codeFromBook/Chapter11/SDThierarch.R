#next line must be L5
library(rjags)
#simulate data from experiment with 10 subjects
n <- 10  
sigtrials <- noistrials <- 100   
h <- rbinom(n,sigtrials, .8)    
f <- rbinom(n,noistrials,.2)  
#f[10]<- 80
#h[10]<- 20
#initialize for JAGS

# top priors
mud_p <- c(1,1/(2^2))
mub_p <- c(0,1/(2^2))
taud_p <- c(.001,.001)
taub_p <- c(.001,.001)

oneinit <- list(mud=0, mub=0, taud=1, taub=1, d=rep(0,n), b=rep(0,n))
myinits <- list(oneinit)[rep(1,4)] 
sdtjh <- jags.model("SDThierarch.j", 
                   data = list("h"=h, "f"=f, "n"=n,
                               mud_p=mud_p,mub_p=mub_p,
                               taud_p=taud_p,taub_p=taub_p,
                               "sigtrials" =sigtrials,
                               "noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)   
# burnin
update(sdtjh,n.iter=1000)  
# perform MCMC
parameters <- c("mud", "mub", "taud", "taub", "phih", "phif") 
mcmcfin<-coda.samples(sdtjh,parameters,5000) 
mcmcs <- as.matrix(mcmcfin)

library(logspline)
blogspl <- logspline(mcmcs[,"mub"])
BF <- dlogspline(0,blogspl)/dnorm(0,0,2)
print(BF)