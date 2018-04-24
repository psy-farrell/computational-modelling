library(rjags)
#simulate data from experiment with 10 subjects
n <- 10  
sigtrials <- noistrials <- 100   
h <- rbinom(n,sigtrials, .8)    
f <- rbinom(n,noistrials,.2)   

#initialize for JAGS
oneinit <- list(mud=0, mub=0, taud=1, taub=1, d=rep(0,n), b=rep(0,n))
myinits <- list(oneinit)[rep(1,4)] 
sdtjh <- jags.model("SDThierarch.j", 
                   data = list("epsilon"=0.001,
                               "h"=h, "f"=f, "n"=n,
                               "sigtrials" =sigtrials,
                               "noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)  
# burnin
update(sdtjh,n.iter=1000)  
# perform MCMC
parameters <- c("d", "b", "taud", "taub", "mud", "mub", "phih", "phif") 
mcmcfin<-coda.samples(sdtjh,parameters,5000)