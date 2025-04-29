
library(rjags)
#provide data from experiment
h <- 12         
f <- 2
sigtrials <- noistrials <- 20  

#initialize for JAGS
oneinit <- list(d=0, b=0)  
myinits <- list(oneinit)[rep(1,4)] 
sdtj <- jags.model("SDT.j",  
                   data = list("h"=h, "f"=f, 
                               "sigtrials"=sigtrials,
                               "noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)   
# burnin
update(sdtj,n.iter=1000)  
# perform MCMC
parameters <- c("d", "b", "phih", "phif")
mcmcfin<-coda.samples(sdtj,parameters,5000) 
