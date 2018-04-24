library(rjags)
#provide data from experiment
h <- 60         
f <- 11
sigtrials <- noistrials <- 100  

#initialize for JAGS
oneinit <- list(d=0, b=0)   
myinits <- list(oneinit)[rep(1,4)]
myinits <- lapply(myinits,FUN=function(x) lapply(x, FUN=function(y) y+rnorm(1,0,.1)))
sdtj <- jags.model("SDT.j", 
                   data = list("h"=h, "f"=f, 
                               "sigtrials"=sigtrials,"noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)  
# burnin
update(sdtj,n.iter=1000)  
# perform MCMC
parameters <- c("d", "b", "phih", "phif")
mcmcfin<-coda.samples(sdtj,parameters,5000) 

summary(mcmcfin)
plot(mcmcfin)
gelman.plot(mcmcfin)